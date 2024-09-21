# app.R

library(shiny)

# Load the UI and server code
source("src/ui.R")
source("src/server.R")

# Launch the Shiny app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
