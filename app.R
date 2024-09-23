# app.R

library(shiny)

# Load the UI and server code
source("ui.R")
source("server.R")

# Launch the Shiny app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
