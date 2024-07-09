#### Packages ####
library(httr)
library(jsonlite)
library(tidyverse)
library(reshape2)
library(cowplot)


#### Data API Pulling functions ####

# Team Records function
get_team_records <- function(year = NULL, team = NULL, conference = NULL) {
  base_url <- "https://api.collegefootballdata.com/records"
  
  # Inner function to fetch data for a single conference
  fetch_data <- function(conference) {
    params <- list(year = year, team = team)
    if (!is.null(conference) && nzchar(conference)) {
      params$conference <- conference
    }
    
    response <- GET(
      url = base_url,
      add_headers("Authorization" = "Bearer Y5gwJGQhtTGf3XMDieiO6TlTu8N7MvCQru29Gp48TqW68gO+nhY/U8CyU3lu/m3w"),
      query = params,
      accept_json()
    )
    
    if (status_code(response) != 200) {
      message("Error: Unable to fetch data. Status code: ", status_code(response))
      message("Response content: ", content(response, "text", encoding = "UTF-8"))
      stop("Request failed.")
    }
    
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    as.data.frame(data)
  }
  
  # Handle multiple conferences
  if (!is.null(conference) && length(conference) > 0) {
    results <- lapply(conference, fetch_data)
    output <- bind_rows(results)
  } else {
    output <- fetch_data(conference)
  }
  
  return(output)
}



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


# Function to get player stats
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


# Get Final Season Rankings
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

# Get Teams Talent
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

# Get Team end of season stats
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

# Wrapper function
cfb_API <- function(func, ...){
  if (func == "get_team_records"){
    output <- get_team_records(...)
  }
  else if (func == "get_game_results"){
    output <- get_game_results(...)
  }
  else if (func == "get_player_stats"){
    output <- get_player_stats(...)
  }
  else if (func == "get_APTOP25"){
    output <- get_APTOP25(...)
  }
  else if (func == "get_teams_talent"){
    output <- get_teams_talent(...)
  }
  else if (func == "get_team_stats"){
    output <- get_team_stats(...)
  }
  else {
    stop("ERROR: Argument for func is not valid!")
  }
  
  # Return the output from the appropriate function.
  return(output)
}


#### Sum Data New ####
ACC_records = cfb_API("get_team_records", year = "2023", conference = "ACC")

ACC_records = ACC_records %>%
  mutate(Homewinpct = homeGames$wins / homeGames$games, Awaywinpct = awayGames$wins/ awayGames$games,
         winpct = total$wins / total$games, WPOE = total$wins / expectedWins)

plt1 = ggplot(ACC_records, aes(x = Homewinpct, y = Awaywinpct, color = winpct)) + 
  geom_point(size = 4) +
  geom_smooth(method = lm, formula = y~x, color = "black") + 
  scale_color_gradient(low = "blue", high = "red") + 
  theme(legend.position = "none") + 
  labs(title = "Home vs Away win percentages for ACC teams", x = "Home Win Percentage", y = "Away Win Percentage")
plt1

ACC_records = ACC_records %>%
  mutate(team = fct_reorder(team, WPOE))

plt2 = ggplot(ACC_records, aes(x = team, y = WPOE, fill = winpct)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Win Percentage Over Expected (WPOE) by ACC Team", x = "Team", y = "WPOE") + 
  scale_fill_gradient(low = "blue", high = "red") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plt2



SEC_Passing = cfb_API("get_player_stats", year = "2023", conference = "SEC", category = "passing", statType = "yds")


SEC_Passing = SEC_Passing %>%
  filter(stat >= 50)

plt3 = ggplot(SEC_Passing, aes(x = stat)) + 
  geom_histogram(color = "#004b8d", fill = "#ffd046", bins = 5) + 
  labs(title = "SEC Player Passing Yards (min 50 yards) in 2023", x = "Passing Yards") + 
  theme_minimal()
plt3


team = cfb_API("get_team_stats", year = "2023", conference = c("ACC", "B12", "B1G", "SEC", "PAC"))

mean_stats_conf = team %>%
  group_by(conference, statName) %>%
  summarize(mean_stat = mean(statValue, na.rm = TRUE)) %>%
  mutate(mean_stat = format(mean_stat, scientific = FALSE, digits = 4)) %>%
  arrange(statName)

mean_stats_conf = mean_stats_conf[-c(26:30),]

mean_stats_conf = mean_stats_conf %>%
  pivot_wider(names_from = statName, values_from = mean_stat)

Off_mean_conf = mean_stats_conf[, c(1:3, 13:17, 20, 24:26, 31)]

def_mean_conf = mean_stats_conf[, c(1, 6:9, 27, 28)]


off_melt = melt(Off_mean_conf, id.vars = "conference")
off_melt$value = as.numeric(off_melt$value)

off_melt <- off_melt %>%
  group_by(variable) %>%
  mutate(scaled_value = scales::rescale(value))

plt4 <- ggplot(off_melt, aes(x = variable, y = conference, fill = scaled_value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Mean Statistics by Conference", x = "Statistic", y = "Conference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plt4

def_melt = melt(def_mean_conf, id.vars = "conference")
def_melt$value = as.numeric(def_melt$value)

def_melt <- def_melt %>%
  group_by(variable) %>%
  mutate(scaled_value = scales::rescale(value))

plt5 <- ggplot(def_melt, aes(x = variable, y = conference, fill = scaled_value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Mean Statistics by Conference", x = "Statistic", y = "Conference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plt5

plot_grid(plt4, plt5, ncol = 2)

# Tables
ACCvsSEC <- cfb_API("get_team_records", year = "2020", conference = c("SEC", "ACC"))

meanW_conf = aggregate(total$wins ~ conference, data = ACCvsSEC, FUN = mean)
meanW_division = aggregate(total$wins ~ division, data = ACCvsSEC, FUN = mean)

meanW_conf
meanW_division

# Interactive Data Exploration
get_teams_data <- function() {
  team <- cfb_API("get_team_stats", year = "2023", conference = c("ACC", "B12", "B1G", "SEC", "PAC"))
  team <- team %>%
    pivot_wider(names_from = statName, values_from = statValue)
  return(team)
}
