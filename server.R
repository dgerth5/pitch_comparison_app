library(shiny)
library(tidyverse)
library(lsa)
library(formattable)

preprocessed_data <- readRDS("final_data.RDS")

metrics_smry <- preprocessed_data$metrics_smry
primary_fb <- preprocessed_data$primary_fb
df1 <- preprocessed_data$df1
df2 <- preprocessed_data$df2
rel_pt <- preprocessed_data$rel_pt
pitch_dat <- preprocessed_data$pitch_dat
norm_df <- preprocessed_data$norm_df

server <- function(input, output) {
  filtered_data <- reactive({
    rel_pt_row <- rel_pt %>% filter(p_throws == input$release_point)
    pitch_dat_row <- pitch_dat %>% filter(pitch_type == input$fastball_type)
    
    input_vector <- c(
      (input$H_Rel - rel_pt_row$h_rel_mean) / rel_pt_row$h_rel_sd,
      (input$V_Rel - rel_pt_row$v_rel_mean) / rel_pt_row$v_rel_sd,
      (input$Ext - rel_pt_row$ext_mean) / rel_pt_row$ext_sd,
      (input$velo - pitch_dat_row$velo_mean) / pitch_dat_row$velo_sd,
      (input$H_Break - pitch_dat_row$h_break_mean) / pitch_dat_row$h_break_sd,
      (input$V_Break - pitch_dat_row$v_break_mean) / pitch_dat_row$v_break_sd
    )
    
    input_vector <- ifelse(is.finite(input_vector), input_vector, 0)
    
    filtered_comps <- norm_df %>%
      filter(p_throws == input$release_point) %>%
      filter(primary_fb == input$fastball_type) %>%
      filter(pitch_type == input$fastball_type)
    
    # Calculate Euclidean distance and similarity score
    d <- numeric(length(filtered_comps$pitcher))
    max_distance <- 0
    
    for (i in 1:length(filtered_comps$pitcher)) {
      euclidean_dist <- sqrt(sum((as.numeric(as.vector(input_vector)) - as.numeric(as.vector(filtered_comps[i, 27:32])))^2))
      d[i] <- euclidean_dist
      max_distance <- max(max_distance, euclidean_dist)
    }
    
    # Calculate similarity score
    filtered_comps$similarity <- 1 - (d / max_distance)
    
    # Get similarity threshold
    threshold <- as.numeric(gsub("%", "", input$similarity_threshold)) / 100
    num_to_keep <- ceiling(threshold * length(filtered_comps$pitcher))
    
    e <- filtered_comps %>%
      filter(similarity >= threshold)
    
    list(similar_players = e, bb = norm_df %>% filter(pitcher %in% unique(e$pitcher)))
  })
  
  output$resultTable <- renderFormattable({
    data <- filtered_data()
    bb <- data$bb
    total_unique_pitchers <- n_distinct(bb$pitcher)
    
    new_df <- bb %>%
      filter(pitch_type %in% c("CH", "SL", "CU", "ST", "FS")) %>%
      group_by(pitch_type) %>%
      summarize(
        unique_pitchers_percentage = round(n_distinct(pitcher) / total_unique_pitchers * 100, 0),
        avg_velo = round(mean(velo, na.rm = TRUE), 1),
        avg_h_break = round(mean(h_break, na.rm = TRUE), 1),
        avg_v_break = round(mean(v_break, na.rm = TRUE), 1)
      ) %>%
      mutate(pn = case_when(
        pitch_type == "CH" ~ "Changeup",
        pitch_type == "SL" ~ "Slider",
        pitch_type == "CU" ~ "Curveball",
        pitch_type == "ST" ~ "Sweeper",
        pitch_type == "FS" ~ "Splitter"
      )) %>%
      dplyr::select(pitch_type, pn, unique_pitchers_percentage, avg_velo, avg_h_break, avg_v_break)
    
    blank_df <- data.frame(pitch_type = c("CH", "SL", "CU", "ST", "FS"),
                           pn = c("Changeup", "Slider", "Curveball", "Sweeper", "Splitter"),
                           unique_pitchers_percentage = rep("-", 5),
                           avg_velo = rep("-", 5),
                           avg_h_break = rep("-", 5),
                           avg_v_break = rep("-", 5))
    
    table_df <- blank_df %>%
      left_join(new_df, by = c("pitch_type", "pn")) %>%
      mutate(
        unique_pitchers_percentage = if_else(coalesce(as.character(unique_pitchers_percentage.y), unique_pitchers_percentage.x) == "-", "-", paste0(as.character(unique_pitchers_percentage.y), "%")) ,
        avg_velo = coalesce(as.character(avg_velo.y), avg_velo.x),
        avg_h_break = coalesce(as.character(avg_h_break.y), avg_h_break.x),
        avg_v_break = coalesce(as.character(avg_v_break.y), avg_v_break.x)
      ) %>%
      dplyr::select(pitch_type, pn, unique_pitchers_percentage, avg_velo, avg_h_break, avg_v_break)
    
    
    colnames(table_df) <- c("Pitch Code", "Pitch Name", "Percentage of Pitchers That Use", "Velo", "Horz Break", "IVB")
    
    formattable(table_df, list(
      `Percentage of Pitchers That Use` = formatter("span", style = x ~ style(display = "block"), format = "percentage"),
      `Velo` = formatter("span", style = x ~ style(display = "block")),
      `Horz Break` = formatter("span", style = x ~ style(display = "block")),
      `IVB` = formatter("span", style = x ~ style(display = "block"))
    ))
  })
  
  output$similarPlayersTable <- renderFormattable({
    data <- filtered_data()
    similar_players <- data$similar_players %>%
      arrange(-similarity) %>%
      select(player_name, similarity)
    
    colnames(similar_players) <- c("Name", "Similarity Score")
    
    formattable(similar_players, list(
      `Similarity Score` = color_tile("white", "lightgreen")
    ))
  })
  
  output$mlbPitcherData <- renderFormattable({
    
    output_df <- df2 %>%
      mutate(h_rel = round(h_rel, 1),
             v_rel = round(v_rel, 1),
             ext = round(ext, 1),
             velo = round(velo, 1),
             h_break = round(h_break, 1),
             v_break = round(v_break, 1))
    
    colnames(output_df) <- c("MLBAM ID", "Name", "Primary FB",  "H Rel", "V Rel", "Ext", "Velo", "H Break", "IVB")
    
    formattable(output_df, list(
      `MLBAM ID` = formatter("span", style = x ~ style(display = "block")),
      `Name` = formatter("span", style = x ~ style(display = "block")),
      `Primary FB` = formatter("span", style = x ~ style(display = "block")),
      `H Rel` = formatter("span", style = x ~ style(display = "block")),
      `V Rel` = formatter("span", style = x ~ style(display = "block")),
      `Ext` = formatter("span", style = x ~ style(display = "block")),
      `Velo` = formatter("span", style = x ~ style(display = "block")),
      `H Break` = formatter("span", style = x ~ style(display = "block")),
      `IVB` = formatter("span", style = x ~ style(display = "block"))))
  })

}

  

  