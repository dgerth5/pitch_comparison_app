library(shiny)
library(formattable)

ui <- navbarPage("Pitcher Similarity and Arsenal Tool",
                 tabPanel("Input and Results",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     h3("Release Point"),
                                     selectInput("release_point", "Throwing Hand", choices = c("R", "L")),
                                     numericInput("H_Rel", "Horizontal Release Point (ft)", value = -1.5),
                                     numericInput("V_Rel", "Vertical Release Point (ft)", value = 6.0)),
                              column(4,
                                     h3("Fastball Characteristics"),
                                     selectInput("fastball_type", "Primary Fastball Type", choices = c("FF", "SI", "FC")),
                                     numericInput("velo", "Velo (MPH)", value = 93),
                                     numericInput("H_Break", "Horizontal Break (in)", value = -7),
                                     numericInput("V_Break", "IVB (in)", value = 15)),
                              column(4,
                                     h3("Similarity Threshold"),
                                     selectInput("similarity_threshold", "Threshold", choices = c("50%", "75%", "80%", "90%"))
                              )
                            ),
                            hr(),
                            div(style = "text-align: center;", h3("Average of Similar Pitchers")),
                            formattableOutput("resultTable")
                          )
                 ),
                 tabPanel("Similar Players",
                          fluidPage(
                            div(style = "text-align: center;", h3("Similar Players and Their Scores")),
                            div(style = "display: flex; justify-content: center;", 
                                div(style = "width: 80%;", formattableOutput("similarPlayersTable")))
                          )
                 )
)