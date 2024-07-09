#### Packages ####
library(httr)
library(jsonlite)
library(tidyverse)


#### Data API Pulling functions ####

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

get_team_records <- function(year = NULL, team = NULL, conference = NULL) {
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/records"
  
  # Function to fetch data for a single conference
  fetch_data <- function(conference) {
    params <- list()
    if (!is.null(year)) params$year <- year
    if (!is.null(team)) params$team <- team
    params$conference <- conference
    
    # Make the GET request with the query parameters
    response <- GET(
      url = base_url,
      add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"),
      query = params,
      accept_json()
    )
    
    # Check if the request was successful
    if (status_code(response) != 200) {
      # Print additional details about the error
      message("Error: Unable to fetch data. Status code: ", status_code(response))
      message("Response content: ", content(response, "text", encoding = "UTF-8"))
      stop("Request failed.")
    }
    
    # Parse the JSON response
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Convert to data.frame
    output <- as.data.frame(data)
    
    return(output)
  }
  
  # Fetch data for each conference and combine results
  if (!is.null(conference)) {
    if (is.vector(conference)) {
      results <- lapply(conference, fetch_data)
      output <- bind_rows(results)
    } else {
      output <- fetch_data(conference)
    }
  } else {
    output <- fetch_data(NULL)
  }
  
  # Return output
  return(output)
}

# Example usage
B <- get_team_records(year = "2023", team = "NC State", conference = "ACC")


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

get_team_stats <- function(year = NULL, team = NULL, conference = NULL){
  # Base URL and endpoint
  base_url <- "https://api.collegefootballdata.com/stats/season"
  
  # Function to fetch data for a single conference
  fetch_data <- function(conference) {
    params <- list()
    if (!is.null(year)) params$year <- year
    if (!is.null(team)) params$team <- team
    params$conference <- conference
    
    # Make the GET request with the query parameters
    response <- GET(
      url = base_url,
      add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"),
      query = params,
      accept_json()
    )
    
    # Check if the request was successful
    if (status_code(response) != 200) {
      # Print additional details about the error
      message("Error: Unable to fetch data. Status code: ", status_code(response))
      message("Response content: ", content(response, "text", encoding = "UTF-8"))
      stop("Request failed.")
    }
    
    # Parse the JSON response
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Convert to data.frame
    output <- as.data.frame(data)
    
    return(output)
  }
  
  # Fetch data for each conference and combine results
  if (!is.null(conference)) {
    if (is.vector(conference)) {
      results <- lapply(conference, fetch_data)
      output <- bind_rows(results)
    } else {
      output <- fetch_data(conference)
    }
  } else {
    output <- fetch_data(NULL)
  }
  
  # Return output
  return(output)
}

G = get_team_stats(year = "2023", conference = c("ACC", "SEC", "B12", "B1G"))

G = G %>% filter(statName == "netPassingYards" | statName == "passingTDs")

G = G %>%
  pivot_wider(names_from = statName, values_from = statValue)



#### Summarizing Data ####
# Tables
table(E$conference)

ACCvsSEC <- get_team_records(year = "2020", conference = c("SEC", "ACC"))

meanW_conf = aggregate(total$wins ~ conference, data = ACCvsSEC, FUN = mean)
meanW_division = aggregate(total$wins ~ division, data = ACCvsSEC, FUN = mean)

meanW_conf
meanW_division

summary(ACCvsSEC)

Summary(D)

# Scatter Plot of Passing yards vs passing TD's by conference/ACC team
ggplot(G, aes(x = netPassingYards, y = passingTDs, color = conference)) + 
  geom_point(size = 4) + 
  labs(title = "Passing Touchdowns vs Yards", x = "Passing Yards", y = "Passing Touchdowns") + 
  theme_minimal()

H = G %>% 
  filter(conference == "ACC")

ggplot(H, aes(x = netPassingYards, y = passingTDs, color = team)) + 
  geom_point(size = 4) + 
  labs(title = "Passing Touchdowns vs Yards by ACC team", x = "Passing Yards", y = "Passing Touchdowns") + 
  theme_minimal()


# Bar Plot
Bardf = F %>%
  left_join(E, by = "school")
Bardf = Bardf[, -c(5,6,7)]

Bardf = Bardf %>%
  filter(year == "2023") %>%
  drop_na(rank)

Bardf$talent = as.numeric(Bardf$talent)

Bardf = Bardf %>%
  arrange(rank) %>%
  head(10)

ggplot(Bardf, aes(x = school, y = talent)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Top 10 in Final Rankings Talent by School", x = "School", y = "talent") + 
  theme_minimal()






