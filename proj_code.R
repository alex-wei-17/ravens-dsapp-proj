### Baltimore Ravens Data Scientist Application
### Project Code Script
### Name: Zihao (Alex) Wei
### Date: June 2, 2026

# Make sure to set working directory to the one with the "data" folder!

# Load necessary packages
library(tidyverse)
library(here)
library(sportyR)
library(gganimate)
library(lme4)
library(ggrepel)

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

# For animating a given play (more sense-check)
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
  filter(frameId >= snap_frameId, frameId <= snap_frameId + 30) %>%
  group_by(gameId, playId, nflId) %>%
  arrange(frameId, .by_group = TRUE)

# Calculate path efficiency of pulling guards
guard_path_eff <- gp_frames %>%
  mutate(lat = abs(y - first(y)),
         lat_gain = lead(lat, 3) - lat,
         pull_start = cumany(lat >= 1),
         pull_end = pull_start & (coalesce(lat_gain, -Inf) < 0.5),
         max_frame = if (any(pull_end)) frameId[which.max(pull_end)]
         else frameId[which.max(lat)]) %>%
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
         lat_gain = lead(lat, 3) - lat,
         pull_start = cumany(lat >= 1),
         pull_end = pull_start & (coalesce(lat_gain, -Inf) < 0.5),
         max_frame = if (any(pull_end)) frameId[which.max(pull_end)]
         else frameId[which.max(lat)]) %>%
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

# Find pull time above/below expected for each guard
guard_ptoe <- ranef(gp_lm)$nflId %>%
  rownames_to_column("nflId") %>%
  rename(pt_oe = `(Intercept)`) %>%
  mutate(nflId = as.integer(nflId), mean_time_saved = -pt_oe) %>%
  left_join(players %>% select(nflId, displayName), by = "nflId") %>%
  left_join(gp_model_df %>% count(nflId, name = "n_pulls"), by = "nflId") %>%
  arrange(desc(mean_time_saved))

# Produce summary table of results
guard_summary <- guard_pull %>%
  group_by(nflId) %>%
  summarize(mean_path_eff = mean(path_eff, na.rm = TRUE),
            mean_init_burst = mean(init_burst, na.rm = TRUE),
            n_pulls = n(), .groups = "drop") %>%
  left_join(guard_ptoe %>% select(nflId, displayName, mean_time_saved),
            by = "nflId") %>%
  select(nflId, displayName, mean_path_eff,
         mean_init_burst, mean_time_saved, n_pulls) %>%
  filter(n_pulls >= 5) %>%
  arrange(desc(mean_time_saved))

# Save the summary table to working directory
write_csv(guard_summary, "guard_pull_summary.csv")

# Get pulling metrics (from before) for visualization
gp_metrics <- gp_frames %>%
  mutate(lat = abs(y - first(y)),
         lat_gain = lead(lat, 3) - lat,
         pull_start = cumany(lat >= 1),
         pull_end = pull_start & (coalesce(lat_gain, -Inf) < 0.5),
         max_frame = if (any(pull_end)) frameId[which.max(pull_end)]
         else frameId[which.max(lat)]) %>%
  filter(frameId <= max_frame)

# Find an example play and store game, play, and guard IDs
ep <- gp_metrics %>% ungroup() %>%
  filter(nflId == 41264) %>%
  distinct(gameId, playId) %>%
  slice(1)
ep_gid <- ep$gameId
ep_pid <- ep$playId
ep_nid <- 41264

# Find the path of the pulling guard on example play
ep_pull_path <- gp_metrics %>% ungroup() %>%
  filter(gameId == ep_gid, playId == ep_pid, nflId == ep_nid) %>%
  arrange(frameId)

# Find endpoints of pulling path in a straight line
ep_pull_ends <- ep_pull_path %>%
  summarize(x0 = first(x), y0 = first(y), x1 = last(x), y1 = last(y))

# Find other O-linemen on the same example play
ep_context <- tracking %>%
  filter(gameId == ep_gid, playId == ep_pid,
         nflId %in% ol_ids, nflId != ep_nid,
         frameId >= min(ep_pull_path$frameId),
         frameId <= max(ep_pull_path$frameId)) %>%
  arrange(nflId, frameId)

# Plot pull path vs. straight line of pulling guard on example play
ggplot() +
  geom_path(data = ep_context, aes(x, y, group = nflId),
            color = "purple", linewidth = 0.8) +
  geom_segment(data = ep_pull_ends, aes(x0, y0, xend = x1, yend = y1),
               color = "black", linetype = "dashed") +
  geom_path(data = ep_pull_path, aes(x, y),
            color = "red", linewidth = 1,
            arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_point(data = ep_pull_ends, aes(x0, y0), color = "red", size = 3) +
  coord_equal() +
  theme_bw() +
  labs(x = "Downfield (yds)",
       y = "Sideline (yds)",
       title = "Guard Pull (Joel Bitonio): Actual Path vs. Straight Line",
       subtitle = "Red = actual; Dashed = ideal; Purple = linemates") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5))

# Plot ranking of guards on time saved per pull
guard_summary %>% filter(n_pulls >= 10) %>%
  mutate(displayName = fct_reorder(displayName, mean_time_saved)) %>%
  ggplot(aes(mean_time_saved, displayName)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
  geom_point(aes(size = n_pulls), color = "navy") +
  scale_size_continuous(range = c(1.5, 5), name = "Pulls") +
  theme_bw() +
  labs(x = "Mean time saved per pull (sec)",
       y = NULL,
       title = "Guard Pulling Times over Expected",
       subtitle = "Right of dashed line = faster than expected") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5))

# Plot initial burst and path efficiency of pulling guards
guard_summary %>%
  ggplot(aes(mean_init_burst, mean_path_eff)) +
  geom_point(aes(size = n_pulls), color = "navy", alpha = 0.65) +
  geom_text_repel(aes(label = displayName), size = 1.7, max.overlaps = 7) +
  scale_size_continuous(range = c(1.5, 5), name = "Pulls") +
  theme_bw() +
  labs(x = "Mean initial burst (yds/sec²)",
       y = "Mean path efficiency (%)",
       title = "Initial Burst vs. Path Efficiency for Pulling Guards",
       subtitle = "Top-right = quick off the ball and a tight path") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5))
