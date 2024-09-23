library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  
  # Add custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Garamond', serif; 
        background-color: #f5f5f5;
      }
      .header {
        text-align: center;
      }
      .header img {
        height: 100px;
        margin-bottom: 10px;
      }
      h3 {
        color: #4CAF50;
      }
    "))
  ),
  
  # Header with logo
  div(class = "header", 
      img(src = "logo.png", alt = "Logo", style="float:right"),  # Relative path to the logo
      titlePanel("Crop Production Simulation Tool")  # Updated app name
  ),
  #setBackgroundImage(
   # src = "https://jgi.doe.gov/wp-content/uploads/2016/04/w1-IMG_7757_Prochnik.jpg"
 # ),
  
  
  hr(style = "border-top: 1px solid #000000;"),
  tags$script(src = 'https://www.googletagmanager.com/gtag/js?id=G-VGTSCPCLQQ', async = ""
  ), 
  tags$script(src = "static/js/gtag.js"), 
    
  sidebarLayout(
    sidebarPanel(
      textInput("crop_name", "Crop Name:", value = "Manihot"),  # Allow users to enter the crop name
      numericInput("initial_material", "Initial Material (plants):", value = 100, min = 1),  # Initial material input
      numericInput("n_cycles", "Number of Cycles:", value = 10, min = 1),  # Number of cycles input
      numericInput("n_simulations", "Number of Simulations:", value = 1000, min = 1),  # Number of simulations input
      numericInput("months_per_cycle", "Months per Cycle:", value = 2, min = 1),  # New input for months per cycle
      numericInput("gamma", "Initial Production Period (months):", value = 6, min = 1),  # Initial production period
      numericInput("alpha", "Acclimation Period (months):", value = 2, min = 1),  # Acclimation period
      actionButton("calculate", "Run Simulation")  # Action button for calculation
    ),
    
    mainPanel(
      h3("Simulation Results"),
      tableOutput("results"),
      
      h3("Mean Number of Plants (Explants) with Uncertainty"),
      plotOutput("plant_plot"),
      
      h3("Mean Number of Technicians with Uncertainty"),
      plotOutput("technician_plot")
    )
  )
))