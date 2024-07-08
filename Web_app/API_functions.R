library(httr)
library(jsonlite)
library(tidyverse)


# Function to see conferences
get_conferences <- function(){
  base_url <- "https://api.collegefootballdata.com/conferences"
  
  params = list()
  
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

A = get_conferences()

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

B = get_team_records(team = "NC State", year = "2023", conference = "ACC")


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
C = get_game_results(year = "2022", week = "3", team = "NC State")


# Function to get game results
get_player_stats <- function(year = NULL, team = NULL, conference = NULL, category = NULL, statType = NULL) {
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/stats/player/season"
  
  # Query
  params <- list()
  if (!is.null(year)) params$year <- year
  if (!is.null(team)) params$team <- team
  if (!is.null(conference)) params$conference <- conference
  if (!is.null(category)) params$category <- category
  if (!is.null(statType)) statType <- toupper(statType)
  

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
  
  if (!is.null(statType) && "statType" %in% colnames(output)) {
    output <- output %>% filter(statType == !!statType)
  }
  
  
  return(output)
}


D = get_player_stats(year = "2023", conference  = "ACC", category = "passing", statType = "yds")


get_APTOP25 <- function(year = NULL, week = NULL, seasonType = "postseason"){
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/rankings"
  
  # Query Parameter list
  params <- list()
  if (!is.null(year)) params$year <- year
  if (!is.null(week)) params$week <- week
  if (!is.null(seasonType)) params$seasonType <- seasonType
  
  
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
  
  ap_top_25 <- data[[4]][[1]][[2]][[5]]
  
  # Convert to data.frame
  output <- as.data.frame(ap_top_25)
  
  # Return output
  return(output)
}

E = get_APTOP25(year = "2023")


get_teams_talent <- function(year = NULL){
  base_url <- "https://api.collegefootballdata.com/talent"
  
  # Query Parameter list
  params <- list()
  if (!is.null(year)) params$year <- year
  
  
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

F = get_teams_talent(year = "2023")







