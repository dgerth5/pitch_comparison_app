library(shiny)

# Load UI and server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
# test