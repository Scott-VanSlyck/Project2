library(httr)
library(jsonlite)
library(tidyverse)

# Function to get team records
get_team_records <- function(year = NULL, team = NULL, conference = NULL) {
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/records"
  
  # Query Parameter list
  params <- list()
  if (!is.null(year)) params$year <- year
  if (!is.null(team)) params$team <- team
  if (!is.null(conference)) params$conference <- conference
  
  
  response <- GET(
    url = base_url,
    add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"), 
    accept_json()
  )
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Error: Unable to fetch data. Status code: ", status_code(response))
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to data.frame
  output <- as.data.frame(data)
  
  # Return output
  return(output)
}

get_team_records(team = "NC State", year = "2023", conference = "ACC")


# Function to get game results
get_game_results <- function(year = NULL, week = NULL, team = NULL, conference = NULL) {
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/games"
  
  # Construct the query parameters list
  params <- list()
  if (!is.null(year)) params$year <- year
  if (!is.null(week)) params$week <- week
  if (!is.null(team)) params$team <- team
  if (!is.null(conference)) params$conference <- conference
  
  # Make the GET request
  response <- GET(
    url = base_url,
    add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"),
    query = params,
    accept_json()
  )
  
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Error: Unable to fetch data. Status code: ", status_code(response))
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to data.frame
  output <- as.data.frame(data)
  
  # Return output
  return(output)
}

# Example usage
get_game_results(year = "2022", week = "3", team = "NC State")


# Function to get game results
get_player_stats <- function(year = NULL, team = NULL, conference = NULL, category = NULL, statType = NULL) {
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/stats/player/season"
  
  # Construct the query parameters list
  params <- list()
  if (!is.null(year)) params$year <- year
  if (!is.null(team)) params$team <- team
  if (!is.null(conference)) params$conference <- conference
  if (!is.null(category)) params$category <- category
  
  # Make the GET request
  response <- GET(
    url = base_url,
    add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"),
    query = params,
    accept_json()
  )
  
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Error: Unable to fetch data. Status code: ", status_code(response))
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to data.frame
  output <- as.data.frame(data)
  
  output$stat = as.numeric(output$stat)
  
  # Return output
  return(output)
}

# Example usage
A = get_player_stats(year = "2023", conference  = "ACC", category = "passing")

