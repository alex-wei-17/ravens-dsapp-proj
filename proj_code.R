### Baltimore Ravens Data Scientist Application
### Project Code Script
### Name: Zihao (Alex) Wei
### Date: June 2, 2026

# Make sure to set working directory to the one with the data folder!

# Load necessary packages
library(tidyverse)
library(here)

# Load the data
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv"))
player_play <- read_csv(here("data", "player_play.csv"))
tracking <- list.files(here("data"), pattern = "tracking_week_",
                       full.names = TRUE) %>% map(read_csv) %>% list_rbind()

# Standardize tracking data direction and orientation
tracking <- tracking %>%
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160 / 3 - y, y),
         dir = ifelse(playDirection == "left", dir + 180, dir),
         dir = ifelse(dir > 360, dir - 360, dir),
         o = ifelse(playDirection == "left", o + 180, o),
         o = ifelse(o > 360, o - 360, o))

# Sauce







































































