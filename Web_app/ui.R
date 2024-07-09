# Shiny ui for college football data

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "College Football Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Data Download", tabName = "data_download", icon = icon("download")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # About tab content
      tabItem(tabName = "about",
              h2("About This Application"),
              p("This application provides tools for exploring and analyzing college football data."),
              p("The data is sourced from the College Football Data API. For more information, visit:"),
              tags$a(href = "https://collegefootballdata.com/", "College Football Data API"),
              br(), br(),
              img(src = "https://seeklogo.com/images/N/ncaa-football-logo-36487E0FA2-seeklogo.com.png", height = "200px")
      ),
      
      # Data Download tab content
      tabItem(tabName = "data_download",
              fluidRow(
                box(title = "Query Data", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width = 6,
                    selectInput("data_function", "Select Data Function:",
                                choices = c("Team Records" = "get_team_records",
                                            "Game Results" = "get_game_results",
                                            "Player Stats" = "get_player_stats",
                                            "AP Top 25" = "get_APTOP25",
                                            "Teams Talent" = "get_teams_talent",
                                            "Team Stats" = "get_team_stats")),
                    uiOutput("additional_inputs"),
                    actionButton("query_data", "Fetch Data"),
                    downloadButton("download_data", "Download Data")
                ),
                box(title = "Data Table", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width = 12,
                    DTOutput("data_table", width = "100%", height = "600px")
                )
              )
      ),
      
      # Data Exploration tab content
      tabItem(tabName = "data_exploration",
              fluidRow(
                box(title = "Plot Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width = 6,
                    selectInput("x_var", "Select X Variable:", choices = NULL),
                    selectInput("y_var", "Select Y Variable:", choices = NULL),
                    selectInput("facet_var", "Facet by Variable:", choices = c("None" = "", NULL)),
                    selectInput("plot_type", "Select Plot Type:", choices = c("Scatter" = "scatter", "Bar" = "bar", "Histogram" = "histogram"))
                ),
                box(title = "Generated Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width = 6,
                    plotOutput("plot_output")
                ),
                box(title = "Data Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width = 12,
                    DTOutput("summary_output")
                )
              )
      )
    )
  )
)