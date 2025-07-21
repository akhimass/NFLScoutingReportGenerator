# --- prepare_data.R (Corrected Version) ---

# Load necessary packages
library(tidyverse)
library(nflfastR)

# Define exactly which columns our Shiny app needs
# The "season_type" column has been added to this list.
columns_to_keep <- c(
  "season_type", "posteam", "defteam", "week", "play_type", "down", 
  "ydstogo", "yardline_100", "pass_attempt", "rush_attempt", "qb_scramble",
  "passer_player_name", "rusher_player_name", "receiver_player_name", "pass_location",
  "air_yards", "yards_gained", "complete_pass", "success", "epa",
  "shotgun", "run_location", "pass_touchdown", "rush_touchdown"
)

# Load the full 2024 PBP data, but only select the columns we need
cat("Loading and trimming 2024 play-by-play data...\n")
pbp_slim <- load_pbp(2024) %>%
  select(any_of(columns_to_keep)) # Using any_of() is safer

# Save the much smaller data frame to an .rds file
saveRDS(pbp_slim, "pbp_app_data.rds")

cat("Successfully saved corrected and slimmed-down data to 'pbp_app_data.rds'\n")
cat("This file is now ready for your Shiny app!\n")