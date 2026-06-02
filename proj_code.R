### Baltimore Ravens Data Scientist Application
### Project Code Script
### Name: Zihao (Alex) Wei
### Date: June 2, 2026

# Make sure to set working directory to the one with the data folder!

# Load necessary packages
library(tidyverse)
library(here)
library(lme4)
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
ol_window <- tracking %>%
  filter(nflId %in% ol_ids) %>%
  inner_join(snap_frames, by = c("gameId", "playId")) %>%
  filter(frameId >= snap_frameId, frameId <= snap_frameId + 12) %>%
  group_by(gameId, playId, nflId) %>%
  arrange(frameId, .by_group = TRUE) %>%
  summarize(x0 = first(x), y0 = first(y),
            y_min = min(y), y_max = max(y), x_min = min(x),
            lat_dir = last(y) - first(y),
            df_dir = last(x) - first(x),
            lat_ratio = abs(last(y) - first(y)) / abs(last(x) - first(x)),
            speed_max = max(s, na.rm = TRUE), .groups = "drop")
ol_anchors <- ol_window %>% select(gameId, playId, nflId, y0)
ol_crossings <- ol_window %>%
  inner_join(ol_anchors, by = c("gameId", "playId"), suffix = c("", "_j"),
             relationship = "many-to-many") %>%
  filter(nflId != nflId_j) %>%
  mutate(crossed = y0_j > y_min & y0_j < y_max) %>%
  group_by(gameId, playId, nflId) %>%
  summarize(n_crossed = sum(crossed), .groups = "drop")

# Identify pullers in the dataset
ol_pull <- ol_window %>%
  left_join(ol_crossings, by = c("gameId", "playId", "nflId")) %>%
  mutate(n_crossed = coalesce(n_crossed, 0L),
         is_behind = x_min <= x0 - 0.25,
         is_pull = n_crossed > 0 & is_behind &
           lat_ratio >= 2 & speed_max >= 2.5 &
           abs(lat_dir) >= 1.25 & df_dir <= 1)

# Sense-check pull detection by plotting o-linemen paths on a given play
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
plot_pull(tracking, 2022091806, 2058)

# For animating a given play
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
  filter(gameId == 2022091806, playId == 2058) %>%
  mutate(pt_color = case_when(club == "NE" ~ "white",
                              club == "PIT" ~ "gold",
                              club == "football" ~ "brown"),
         pt_size = ifelse(club == "football", 3, 6))
field_background +
  geom_point(data = play_example, aes(x, y),
             size = play_example$pt_size,
             color = play_example$pt_color) +
  transition_time(play_example$frameId)

# Get all gap scheme (power, counter, & trap) plays
gap_plays <- plays %>%
  filter(pff_runConceptPrimary %in% c("POWER", "COUNTER", "TRAP")) %>%
  select(gameId, playId, pff_runConceptPrimary)

# Filter data for only guard pulls on gap scheme plays
guard_ids <- players %>% filter(position == "G") %>% pull(nflId)
guard_pull <- ol_pull %>%
  filter(nflId %in% guard_ids) %>%
  filter(is_pull) %>%
  inner_join(gap_plays, by = c("gameId", "playId"))

# Get frame-level data for player-plays with guard pulling
gp_keys <- guard_pull %>% distinct(gameId, playId, nflId)
gp_frames <- tracking %>%
  inner_join(gp_keys, by = c("gameId", "playId", "nflId")) %>%
  inner_join(snap_frames, by = c("gameId", "playId")) %>%
  filter(frameId >= snap_frameId, frameId <= snap_frameId + 40) %>%
  group_by(gameId, playId, nflId) %>%
  arrange(frameId, .by_group = TRUE)

# Calculate path efficiency of pulling guards
guard_path_eff <- gp_frames %>%
  mutate(lat = abs(y - first(y)),
         max_frame = frameId[which.max(lat)]) %>%
  filter(frameId <= max_frame) %>%
  mutate(step = sqrt((x - lag(x))^2 + (y - lag(y))^2)) %>%
  summarize(path_line = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2),
            path_actual = sum(step, na.rm = TRUE),
            path_eff = path_line / path_actual, .groups = "drop")

# Calculate initial burst of pulling guards
guard_init_burst <- gp_frames %>%
  filter(frameId <= snap_frameId + 5) %>%
  summarize(init_burst = mean(a, na.rm = TRUE), .groups = "drop")

# Join directly-derived metrics with main data frame
guard_pull <- guard_pull %>%
  left_join(guard_path_eff %>% select(gameId, playId, nflId, path_eff),
            by = c("gameId", "playId", "nflId")) %>%
  left_join(guard_init_burst, by = c("gameId", "playId", "nflId"))

# Prepare guard pulling data for model fitting
players_wt <- players %>% select(nflId, weight)
gp_model_df <- gp_frames %>%
  mutate(lat = abs(y - first(y)),
         max_frame = frameId[which.max(lat)]) %>%
  filter(frameId <= max_frame) %>%
  summarize(pull_time = (last(frameId) - first(snap_frameId)) / 10,
            pull_dist = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2),
            .groups = "drop") %>%
  left_join(guard_pull %>% distinct(gameId, playId, nflId,
                                    pff_runConceptPrimary),
            by = c("gameId", "playId", "nflId")) %>%
  left_join(players_wt, by = "nflId")

# Fit multilevel model to estimate expected pulling time
gp_lm <- lmer(pull_time ~ pull_dist + pff_runConceptPrimary +
                weight + (1 | nflId), data = gp_model_df)

# Sense-check model by printing summary and residual plot
summary(gp_lm)
plot(gp_lm)

# Get random intercepts for each guard
ranef(gp_lm)$nflId


































