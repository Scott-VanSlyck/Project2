# Project2
This app allows you to pull data from the past 5 years of college football whether that be end of season rankings or team statistics throughout the year by conference. It's purpose is to allow people to analyze teams and understand how they performed throughout the season.

Code for packages:

packages <- c("httr", "jsonlite", "tidyverse", "reshape2", "cowplot", "shiny", "DT", "shinydashboard")


install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}


sapply(packages, install_if_missing)

Code to run repo:

shiny::runGitHub(repo = "Project2", username = "Scott-VanSlyck", subdir = "Web_app")
