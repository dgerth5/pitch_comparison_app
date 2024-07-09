library(tidyverse)
library(readr)

statcast_data2024 <- read_csv("statcast_data_2024_2.csv")

metrics_smry <- statcast_data2024 %>%
  drop_na(release_pos_x, release_pos_z, release_extension, release_speed, pfx_x, pfx_z) %>%
  filter(pitch_type %in% c("FF", "SI", "FC", "CH", "CU", "SL", "FS", "ST", "KC")) %>%
  group_by(pitcher, player_name) %>%
  mutate(total_pitches = n()) %>%
  group_by(pitcher, player_name, pitch_type, p_throws) %>%
  summarise(h_rel = mean(release_pos_x),
            v_rel = mean(release_pos_z),
            ext = mean(release_extension),
            velo = mean(release_speed),
            h_break = mean(pfx_x)*12,
            v_break = mean(pfx_z)*12,
            run_value = mean(delta_run_exp),
            times_thrown = n(),
            usage = n() / first(total_pitches) * 100) %>%
  filter(usage > 5) %>%
  ungroup()

primary_fb <- metrics_smry %>%
  filter(pitch_type %in% c("FF", "SI", "FC")) %>%
  group_by(pitcher) %>%
  slice_max(order_by = usage, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(primary_fb = pitch_type) %>%
  select(pitcher, primary_fb) 

df1 <- metrics_smry %>%
  left_join(primary_fb, by = "pitcher") %>%
  drop_na()

df2 <- df1 %>%
  filter(pitch_type == primary_fb) %>%
  select(pitcher, player_name, primary_fb, h_rel, v_rel, ext, velo, h_break, v_break)

rel_pt <- df1 %>%
  group_by(p_throws) %>%
  summarise(h_rel_mean = mean(h_rel),
            h_rel_sd = sd(h_rel),
            v_rel_mean = mean(v_rel),
            v_rel_sd = sd(h_rel),
            ext_mean = mean(ext),
            ext_sd = sd(ext))

pitch_dat <- df1 %>%
  group_by(pitch_type) %>%
  summarise(velo_mean = mean(velo),
            velo_sd = sd(velo),
            h_break_mean = mean(h_break),
            h_break_sd = sd(h_break),
            v_break_mean = mean(v_break),
            v_break_sd = sd(v_break))

norm_df <- df1 %>%
  left_join(rel_pt, by = "p_throws") %>%
  left_join(pitch_dat, by = "pitch_type") %>%
  mutate(n_hrp = (h_rel - h_rel_mean) / h_rel_sd,
         n_vrp = (v_rel - v_rel_mean) / v_rel_sd,
         n_ext = (ext - ext_mean) / ext_sd,
         n_velo = (velo - velo_mean) / velo_sd,
         n_hb = (h_break - h_break_mean) / h_break_sd,
         n_vb = (v_break - v_break_mean) / v_break_sd)

saveRDS(list(metrics_smry = metrics_smry, primary_fb = primary_fb, df1 = df1, df2 = df2, rel_pt = rel_pt, pitch_dat = pitch_dat, norm_df = norm_df), "final_data.RDS")
