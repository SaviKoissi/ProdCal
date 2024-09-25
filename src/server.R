# server.R
library(shiny)
library(ggplot2)
library(dplyr)
source("src/BayProdCal.R")  # Load your production calculation functions

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
    summary_results = paste(capture.output(print(summary_results)), collapse = "\n"), # Save summary results as text
    stringsAsFactors = FALSE
  )
  
  # Append to CSV
  if (!file.exists("data/sim_log.csv")) {
    write.csv(log_entry, "data/sim_log.csv", row.names = FALSE)
  } else {
    write.table(log_entry, "data/sim_log.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  }
}

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
      reframe(
        mean_explants = mean(explants_after_n),
        sd_explants = sd(explants_after_n),
        mean_bottles = mean(bottles_required),
        sd_bottles = sd(bottles_required),
        mean_technicians = mean(technicians_needed),
        sd_technicians = sd(technicians_needed),
        # Calculate Time T for each cycle
        time_T = 6 + cycle * 2 + 2
      ) %>%
      distinct()  # Remove any duplicate rows
    
    # Save the computation details to the log
    save_log(input, summary_results)
    
    
    # Render the results table with the Time column
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
    
  })
})
