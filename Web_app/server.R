# Server.r for College football data application

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)

source("API_functions.R")  # Make sure this includes get_teams_data()

server <- function(input, output, session) {
  
  # Dynamically generate input fields based on the selected data function
  output$additional_inputs <- renderUI({
    switch(input$data_function,
           "get_team_records" = tagList(
             textInput("year", "Year", value = "2023"),
             textInput("team", "Team"),
             textInput("conference", "Conference")
           ),
           "get_game_results" = tagList(
             textInput("year", "Year", value = "2023"),
             textInput("week", "Week"),
             textInput("team", "Team"),
             textInput("conference", "Conference")
           ),
           "get_player_stats" = tagList(
             textInput("year", "Year", value = "2023"),
             textInput("team", "Team"),
             textInput("conference", "Conference"),
             textInput("category", "Category (e.g., passing, rushing, receiving)"),
             textInput("statType", "Stat Type (e.g. yds, td")
           ),
           "get_APTOP25" = tagList(
             textInput("year", "Year", value = "2023")
           ),
           "get_teams_talent" = tagList(
             textInput("year", "Year", value = "2023")
           ),
           "get_team_stats" = tagList(
             textInput("year", "Year", value = "2023"),
             textInput("team", "Team"),
             textInput("conference", "Conference")
           )
    )
  })
  
  # Reactive expression to load the teams data for Data Exploration
  teams_data <- reactive({
    get_teams_data()  # This function needs to be defined in API_functions.R
  })
  
  # Handle data fetching based on selected API function
  observeEvent(input$query_data, {
    req(input$data_function)  # Ensure a function is selected
    
    # Build parameters selectively based on the selected data function
    params <- list(year = input$year)
    if (input$data_function %in% c("get_team_records", "get_game_results", "get_player_stats", "get_team_stats")) {
      params$team <- input$team
      params$conference <- input$conference
    }
    if (input$data_function == "get_game_results") {
      params$week <- input$week
    }
    if (input$data_function == "get_player_stats") {
      params$category <- input$category
      params$statType <- input$statType
    }
    
    api_functions <- list(
      get_team_records = get_team_records,
      get_game_results = get_game_results,
      get_player_stats = get_player_stats,
      get_APTOP25 = get_APTOP25,
      get_teams_talent = get_teams_talent,
      get_team_stats = get_team_stats
    )
    
    # Fetch data using the selected API function with proper parameters
    if (input$data_function %in% names(api_functions)) {
      df <- tryCatch({
        do.call(api_functions[[input$data_function]], params)
      }, error = function(e) {
        print(e$message)
        showNotification("Error in querying data.", type = "error")
        NULL
      })
      
      output$data_table <- DT::renderDataTable({
        datatable(df, options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)
      })
    }
  })
  
  # Download handler for downloading data
  output$download_data <- downloadHandler(
    filename = function() { paste("data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Observe changes in teams_data to update variable selection inputs dynamically
  observe({
    updateSelectInput(session, "x_var", choices = names(teams_data()))
    updateSelectInput(session, "y_var", choices = names(teams_data()))
    updateSelectInput(session, "facet_var", choices = c("None" = "", names(teams_data())))
  })
  
  # Generate plot based on user input
  output$plot_output <- renderPlot({
    req(input$x_var, input$y_var)
    data <- teams_data()  # Use the reactive teams_data
    p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + geom_point()
    
    if (input$plot_type == "bar") {
      p <- ggplot(data, aes_string(x = input$x_var, fill = input$y_var)) + geom_bar(stat = "count")
    } else if (input$plot_type == "histogram") {
      p <- ggplot(data, aes_string(x = input$x_var)) + geom_histogram()
    }
    
    if (input$facet_var != "None") {
      p <- p + facet_wrap(~ get(input$facet_var), scales = "free")
    }
    
    p
  })
  
  # Data summary output
  output$summary_output <- renderDataTable({
    datatable(teams_data(), options = list(scrollX = TRUE))
  })
}