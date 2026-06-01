### Baltimore Ravens Data Scientist Application
### Project Code Script
### Name: Zihao (Alex) Wei
### Date: June 2, 2026

# Make sure to set working directory to the one with the data folder!

# Load necessary packages
library(tidyverse)
library(here)
library(sportyR)
library(gganimate)

# Load the data (except for tracking data)
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv"))
player_play <- read_csv(here("data", "player_play.csv"))

# Filter plays to only include designed runs
runs <- plays %>%
  filter(isDropback == FALSE, qbKneel == 0, qbSneak == FALSE) %>%
  filter(is.na(penaltyYards) | prePenaltyYardsGained == yardsGained) %>%
  select(gameId, playId)

# Filter each tracking dataset before binding them together
tracking <- list.files(here("data"),
                       pattern = "tracking_week_", full.names = TRUE) %>%
  map(\(f) {read_csv(f) %>%
      filter(frameType != "BEFORE_SNAP") %>%
      semi_join(runs, by = c("gameId", "playId"))}) %>% list_rbind()

# Standardize tracking data direction and orientation
tracking <- tracking %>%
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160 / 3 - y, y),
         dir = ifelse(playDirection == "left", dir + 180, dir),
         dir = ifelse(dir > 360, dir - 360, dir),
         o = ifelse(playDirection == "left", o + 180, o),
         o = ifelse(o > 360, o - 360, o))

# Store OL IDs
ol_ids <- players %>%
  filter(position %in% c("G", "T", "C")) %>% pull(nflId)

# Store snap frames across all plays
snap_frames <- tracking %>%
  filter(frameType == "SNAP") %>%
  distinct(gameId, playId, snap_frameId = frameId)

# Create variables to detect OL pulling activity
pull_ol <- tracking %>%
  filter(nflId %in% ol_ids) %>%
  inner_join(snap_frames, by = c("gameId", "playId")) %>%
  filter(frameId >= snap_frameId, frameId <= snap_frameId + 15) %>%
  group_by(gameId, playId, nflId) %>%
  arrange(frameId, .by_group = TRUE) %>%
  summarize(y0 = first(y),
            lat_dir = last(y) - first(y),
            lat_dist = max(abs(y - first(y))),
            df_dir = last(x) - first(x),
            speed_max = max(s, na.rm = TRUE),
            .groups = "drop")

# Identify pullers and pulling direction
pull_ol <- pull_ol %>%
  mutate(is_pull = lat_dist >= 3.5 & speed_max >= 2.5 & df_dir <= 1.5,
         pull_side = if_else(lat_dir > 0, "left", "right"))

# Sense-check pull detection by plotting some plays
plot_pull <- function(t, gid, pid) {
  t %>%
    filter(gameId == gid, playId == pid, nflId %in% ol_ids) %>%
    arrange(frameId) %>%
    ggplot(aes(x, y, group = nflId, color = factor(nflId))) +
    geom_path(arrow = arrow(length = unit(0.12, "cm"))) +
    geom_point(data = ~filter(.x, frameType == "SNAP"), size = 3) +
    coord_equal() +
    labs(title = paste("Game", gid, "Play", pid))
}
plot_pull(tracking, 2022091100, 870)
field_params <- list(field_apron = "springgreen3",
                     field_border = "springgreen3",
                     offensive_endzone = "springgreen3",
                     defensive_endzone = "springgreen3",
                     offensive_half = "springgreen3",
                     defensive_half = "springgreen3")
field_background <- geom_football(league = "nfl",
                                  display_range = "in_bounds_only",
                                  x_trans = 60, y_trans = 80 / 3,
                                  xlims = c(10, 110),
                                  color_updates = field_params)
play_example <- tracking %>%
  filter(gameId == 2022090800, playId == 191) %>%
  mutate(pt_color = case_when(club == "BUF" ~ "navy",
                              club == "LA" ~ "white",
                              club == "football" ~ "gold"),
         pt_size = ifelse(club == "football", 3, 6))
field_background +
  geom_point(data = play_example, aes(x, y),
             size = play_example$pt_size,
             color = play_example$pt_color) +
  transition_time(play_example$frameId)























































