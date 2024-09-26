# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(promises)    # Added for async operations
library(future)      # Added for async operations
library(base64enc)   # For base64 encoding

plan(multisession)   # Set up asynchronous processing

source("src/BayProdCal.R")  # Load your production calculation functions

# Function to upload file to GitHub via API
upload_to_github <- function(file_path, repo, branch, token, message = "Update simulation log") {
  # Prepare the URL for GitHub API
  url <- paste0("https://api.github.com/repos/", repo, "/contents/", file_path)
  
  # Read the file content and encode it in base64
  file_content <- readBin(file_path, "raw", file.info(file_path)$size)
  content <- base64encode(file_content)
  
  # Get the current file's SHA (required for updating an existing file)
  res_get <- GET(
    url,
    add_headers(Authorization = paste("token", token))
  )
  sha <- ifelse(res_get$status_code == 200, content(res_get)$sha, NULL)
  
  # Prepare the body for the API request
  body <- list(
    message = message,
    content = content,
    branch = branch,
    sha = sha
  )
  
  # Send the PUT request to GitHub API
  res <- PUT(
    url,
    add_headers(Authorization = paste("token", token)),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  # Check for successful upload
  if (res$status_code == 201) {
    print("File successfully uploaded to GitHub!")
  } else {
    print(paste("Failed to upload file:", res$status_code, content(res)))
  }
}

# Function to save log entries to CSV
save_log <- function(inputs, summary_results) {
  log_entry <- data.frame(
    timestamp = Sys.time(),
    crop_name = inputs$crop_name,
    initial_material = inputs$initial_material,
    n_cycles = inputs$n_cycles,
    n_simulations = inputs$n_simulations,
    gamma = inputs$gamma,
    alpha = inputs$alpha,
    summary_results = paste(capture.output(print(summary_results)), collapse = "\n"),
    stringsAsFactors = FALSE
  )
  
  # Append to CSV
  log_file_path <- "src/sim_log.csv"
  if (!file.exists(log_file_path)) {
    write.csv(log_entry, log_file_path, row.names = FALSE)
  } else {
    write.table(log_entry, log_file_path, row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  }
}

# Upload the CSV file to GitHub asynchronously
upload_to_github_async <- function(file_path, repo, branch, token) {
  future({
    upload_to_github(file_path, repo, branch, token)
  })
}

# Retrieve the GitHub token from an environment variable
github_token <- Sys.getenv("ShinyAppToken")
github_repo <- "SaviKoissi/ProdCal"
github_branch <- "main"

# Shiny server logic
server <- shinyServer(function(input, output) {
  
  observeEvent(input$calculate, {
    
    # Call the monte_carlo_simulation function with user input
    results_df <- monte_carlo_simulation(
      x = input$initial_material,
      n_cycles = input$n_cycles,
      n_simulations = input$n_simulations
    )
    
    # Summarize results by cycle
    summary_results <- results_df %>%
      group_by(cycle) %>%
      summarize(
        mean_explants = mean(explants_after_n),
        sd_explants = sd(explants_after_n),
        mean_bottles = mean(bottles_required),
        sd_bottles = sd(bottles_required),
        mean_technicians = mean(technicians_needed),
        sd_technicians = sd(technicians_needed),
        time_T = 6 + cycle * 2 + 2
      )
    
    # Asynchronously save the computation details to the log
    future({
      save_log(input, summary_results)
    })
    
    # Render the results table
    output$results <- renderTable({
      summary_results
    })
    
    # Render the plot for number of plants (explants) with uncertainty
    output$plant_plot <- renderPlot({
      ggplot(summary_results, aes(x = cycle)) +
        geom_line(aes(y = mean_explants), color = "green", size = 1.5) +
        geom_ribbon(aes(ymin = mean_explants - sd_explants, ymax = mean_explants + sd_explants), 
                    alpha = 0.2, fill = "green") +
        labs(title = "Mean Number of Plants (Explants) Over Time with Uncertainty",
             x = "Cycle",
             y = "Number of Plants") +
        theme_minimal()
    })
    
    # Render the plot for number of technicians needed with uncertainty
    output$technician_plot <- renderPlot({
      ggplot(summary_results, aes(x = cycle)) +
        geom_line(aes(y = mean_technicians), color = "red", size = 1.5) +
        geom_ribbon(aes(ymin = mean_technicians - sd_technicians, ymax = mean_technicians + sd_technicians), 
                    alpha = 0.2, fill = "red") +
        labs(title = "Mean Number of Technicians Over Time with Uncertainty",
             x = "Cycle",
             y = "Number of Technicians") +
        theme_minimal()
    })
    
    # Asynchronously upload the log to GitHub
    upload_to_github_async("src/sim_log.csv", github_repo, github_branch, github_token)
  })
})
