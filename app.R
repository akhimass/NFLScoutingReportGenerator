# =================================================================
# ==     UNC CHARLOTTE NFL ANALYTICS PROJECT - FINAL SUITE       ==
# ==            (High-Yield Data & Corrected Colors)             ==
# =================================================================

# --- 1. LOAD LIBRARIES ---
# Ensure you have installed these packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)
library(showtext)
library(sysfonts)
library(ggtext)
library(stringr)
library(readxl)

# --- 1a. FONT SETUP ---
font_add_google("Source Sans Pro", "Source Sans Pro")
showtext_auto()

# --- 2. LOAD AND PREPARE THE DATA (2024 SEASON) ---
cat("Loading pre-prepared application data...\n")
# This now reads your small, local .rds file instead of the huge dataset
pbp_data <- readRDS("pbp_app_data.rds")
cat("Data loaded successfully.\n")

# This filtering method is the most robust and ensures high data yield
pbp_prepared <- pbp_data %>%
  filter(
    season_type == 'REG',
    !is.na(down),
    (pass_attempt == 1 | rush_attempt == 1 | qb_scramble == 1),
    play_type != "no_play"
  ) %>%
  mutate(
    play_category = ifelse(pass_attempt == 1 | qb_scramble == 1, "Pass", "Run")
  )

# Define the correct factor order for our plots and tables
dist_order <- c(
  "1st & 10+", "1st & <10",
  "2nd & Long (7+ yds)", "2nd & Medium (3-6 yds)", "2nd & Short (1-2 yds)",
  "3rd & Long (7+ yds)", "3rd & Medium (3-6 yds)", "3rd & Short (1-2 yds)",
  "4th & Long (7+ yds)", "4th & Medium (3-6 yds)", "4th & Short (1-2 yds)"
)

field_pos_order <- c(
  "Backed Up (Own 1-9)", "In the Field (Own 10-Opp 21)", "Upper Red Zone (Opp 20-11)",
  "Lower Red Zone (Opp 10-3)", "Goal Line (Opp 2-1)"
)

# Create the final analysis data frame with situational buckets
pbp_final <- pbp_prepared %>%
  mutate(
    dist_bucket = factor(case_when(
      down == 1 & ydstogo >= 10 ~ "1st & 10+",
      down == 1 & ydstogo < 10  ~ "1st & <10",
      down == 2 & ydstogo >= 7 ~ "2nd & Long (7+ yds)",
      down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ "2nd & Medium (3-6 yds)",
      down == 2 & ydstogo <= 2 ~ "2nd & Short (1-2 yds)",
      down == 3 & ydstogo >= 7 ~ "3rd & Long (7+ yds)",
      down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ "3rd & Medium (3-6 yds)",
      down == 3 & ydstogo <= 2 ~ "3rd & Short (1-2 yds)",
      down == 4 & ydstogo >= 7 ~ "4th & Long (7+ yds)",
      down == 4 & ydstogo >= 3 & ydstogo <= 6 ~ "4th & Medium (3-6 yds)",
      down == 4 & ydstogo <= 2 ~ "4th & Short (1-2 yds)",
      TRUE ~ "Other"
    ), levels = dist_order),
    field_pos_bucket = factor(case_when(
      yardline_100 >= 91 ~ "Backed Up (Own 1-9)",
      yardline_100 <= 90 & yardline_100 >= 21 ~ "In the Field (Own 10-Opp 21)",
      yardline_100 <= 20 & yardline_100 >= 11 ~ "Upper Red Zone (Opp 20-11)",
      yardline_100 <= 10 & yardline_100 >= 3 ~ "Lower Red Zone (Opp 10-3)",
      yardline_100 <= 2 & yardline_100 >= 1 ~ "Goal Line (Opp 2-1)",
      TRUE ~ "Other"
    ), levels = field_pos_order)
  ) %>%
  filter(dist_bucket != "Other", field_pos_bucket != "Other")

# Get a list of teams for the dropdown menus
team_list <- teams_colors_logos %>%
  filter(!team_abbr %in% c('LAR', 'OAK', 'STL', 'SD')) %>%
  pull(team_abbr) %>%
  sort()

# --- NEW, MORE ROBUST EXCEL READING BLOCK ---
offseason_moves_path <- "NFL_Offseason_Moves.xlsx"

# This ensures we only use the most recent team name for a given abbreviation
clean_logos <- teams_colors_logos %>% 
  group_by(team_abbr) %>% 
  slice_head(n = 1) %>% 
  ungroup()

# Check if the file exists before trying to read it
if (file.exists(offseason_moves_path)) {
  moves_data <- excel_sheets(path = offseason_moves_path) %>%
    set_names() %>%
    map_df(~ read_excel(path = offseason_moves_path, sheet = .x), .id = "team_name") %>%
    left_join(clean_logos %>% select(team_name, team_abbr), by = "team_name") %>%
    # --- THIS IS THE NEW, ROBUST CLEANING STEP ---
    # It cleans the Traded and Retired columns to be perfectly consistent
    mutate(
      Traded = toupper(str_trim(as.character(Traded))),
      Retired = toupper(str_trim(as.character(Retired)))
    )
} else {
  moves_data <- tibble() # Create an empty tibble if the file doesn't exist
}

# --- READ THE 2025 DRAFT RESULTS ---
draft_results_path <- "NFL_2025_Draft_Results.xlsx"
if (file.exists(draft_results_path)) {
  draft_data <- read_excel(draft_results_path)
} else {
  draft_data <- tibble() # Create an empty tibble if the file doesn't exist
}
# =================================================================
# == FUNCTION 1: RUN TENDENCY & SUCCESS HEATMAP  ==
# =================================================================
generate_run_tendency_heatmap <- function(team_abbr) {
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  all_situations <- expand_grid(
    dist_bucket = factor(dist_order, levels = dist_order),
    field_pos_bucket = factor(field_pos_order, levels = field_pos_order)
  )
  
  team_summary <- pbp_final %>%
    filter(posteam == team_abbr) %>%
    group_by(dist_bucket, field_pos_bucket) %>%
    summarise(
      total_runs = sum(play_category == "Run", na.rm = TRUE),
      total_passes = sum(play_category == "Pass", na.rm = TRUE),
      successful_runs = sum(success == 1 & play_category == "Run", na.rm = TRUE),
      successful_passes = sum(success == 1 & play_category == "Pass", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    right_join(all_situations, by = c("dist_bucket", "field_pos_bucket")) %>%
    mutate(
      across(c(total_runs, total_passes, successful_runs, successful_passes), ~replace_na(., 0))
    ) %>%
    mutate(
      total_plays = total_runs + total_passes,
      run_pct = if_else(total_plays > 0, total_runs / total_plays, NA_real_),
      plot_label = if_else(
        total_plays == 0, "",
        paste0(scales::percent(run_pct, accuracy=1), "\n", "R: ", successful_runs, "/", total_runs, " | P: ", successful_passes, "/", total_passes)
      )
    )
  
  heatmap_plot <- ggplot(team_summary, aes(x = field_pos_bucket, y = dist_bucket)) +
    geom_tile(aes(fill = run_pct), color = "#444444", linewidth = 1.5) +
    # *** Text size increased from 3 to 3.5 ***
    geom_text(aes(label = plot_label), color = "black", size = 3.5, fontface = "bold", family = "Source Sans Pro", lineheight = .8) +
    scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", midpoint = 0.5, labels = scales::percent, name = "Run %", na.value = "transparent") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top", limits = field_pos_order, labels = function(x) str_wrap(x, width = 12)) +
    labs(
      title = str_wrap(paste(toupper(team_info$team_name), "RUN TENDENCY & PLAY SUCCESS (2024)"), 85),
      caption = "Color = Run Tendency %. Inside: Run % | Run Success (S/T) | Pass Success (S/T)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
      plot.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(color = "white", size = 11),
      plot.title = element_text(size = 16, color = "white", hjust = 0.5, margin = margin(t=5, b=10)),
      plot.caption = element_text(size = 9, color = "gray", face = "italic"),
      legend.title = element_text(), legend.text = element_text()
    )
  
  return(heatmap_plot)
}

# =================================================================
# == FUNCTION 2: PASS TENDENCY & SUCCESS HEATMAP ==
# =================================================================
generate_pass_tendency_heatmap <- function(team_abbr) {
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  all_situations <- expand_grid(
    dist_bucket = factor(dist_order, levels = dist_order),
    field_pos_bucket = factor(field_pos_order, levels = field_pos_order)
  )
  
  team_summary <- pbp_final %>%
    filter(posteam == team_abbr) %>%
    group_by(dist_bucket, field_pos_bucket) %>%
    summarise(
      total_runs = sum(play_category == "Run", na.rm = TRUE),
      total_passes = sum(play_category == "Pass", na.rm = TRUE),
      successful_runs = sum(success == 1 & play_category == "Run", na.rm = TRUE),
      successful_passes = sum(success == 1 & play_category == "Pass", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    right_join(all_situations, by = c("dist_bucket", "field_pos_bucket")) %>%
    mutate(
      across(c(total_runs, total_passes, successful_runs, successful_passes), ~replace_na(., 0))
    ) %>%
    mutate(
      total_plays = total_runs + total_passes,
      pass_pct = if_else(total_plays > 0, total_passes / total_plays, NA_real_),
      plot_label = if_else(
        total_plays == 0, "",
        paste0(scales::percent(pass_pct, accuracy=1), "\n", "R: ", successful_runs, "/", total_runs, " | P: ", successful_passes, "/", total_passes)
      )
    )
  
  heatmap_plot <- ggplot(team_summary, aes(x = field_pos_bucket, y = dist_bucket)) +
    geom_tile(aes(fill = pass_pct), color = "#444444", linewidth = 1.5) +
    # *** Text size increased from 3 to 3.5 ***
    geom_text(aes(label = plot_label), color = "black", size = 3.5, fontface = "bold", family = "Source Sans Pro", lineheight = .8) +
    scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", midpoint = 0.5, labels = scales::percent, name = "Pass %", na.value = "transparent") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top", limits = field_pos_order, labels = function(x) str_wrap(x, width = 12)) +
    labs(
      title = str_wrap(paste(toupper(team_info$team_name), "PASS TENDENCY & PLAY SUCCESS (2024)"), 85),
      caption = "Color = Pass Tendency %. Inside: Pass % | Run Success (S/T) | Pass Success (S/T)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
      plot.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(color = "white", size = 11),
      plot.title = element_text(size = 16, color = "white", hjust = 0.5, margin = margin(t=5, b=10)),
      plot.caption = element_text(size = 9, color = "gray", face = "italic"),
      legend.title = element_text(), legend.text = element_text()
    )
  
  return(heatmap_plot)
}

# =================================================================
# == FUNCTION 3: PLAY SUCCESS RANK HEATMAP ==
# =================================================================
generate_rank_heatmap <- function(team_abbr) {
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  all_situations <- expand_grid(
    dist_bucket = factor(dist_order, levels = dist_order),
    field_pos_bucket = factor(field_pos_order, levels = field_pos_order)
  )
  
  league_wide_summary <- pbp_final %>%
    group_by(posteam, dist_bucket, field_pos_bucket) %>%
    summarise(
      run_plays = sum(play_category == "Run"),
      pass_plays = sum(play_category == "Pass"),
      run_success = sum(success == 1 & play_category == "Run", na.rm = TRUE),
      pass_success = sum(success == 1 & play_category == "Pass", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      run_success_rate = if_else(run_plays >= 5, run_success / run_plays, NA_real_),
      pass_success_rate = if_else(pass_plays >= 5, pass_success / pass_plays, NA_real_)
    ) %>%
    group_by(dist_bucket, field_pos_bucket) %>%
    mutate(
      run_rank = rank(-run_success_rate, na.last = "keep", ties.method = "min"),
      pass_rank = rank(-pass_success_rate, na.last = "keep", ties.method = "min"),
      avg_rank = rowMeans(cbind(run_rank, pass_rank), na.rm = TRUE)
    ) %>%
    ungroup()
  
  team_summary_formatted <- league_wide_summary %>%
    filter(posteam == team_abbr) %>%
    mutate(
      plot_label = case_when(
        is.na(avg_rank) ~ "N/A",
        is.na(run_rank) ~ paste0("P: #", pass_rank),
        is.na(pass_rank) ~ paste0("R: #", run_rank),
        TRUE ~ paste0("R: #", run_rank, " | P: #", pass_rank)
      )
    ) %>%
    right_join(all_situations, by = c("dist_bucket", "field_pos_bucket")) %>%
    mutate(plot_label = ifelse(is.na(plot_label), "N/A", plot_label))
  
  
  heatmap_plot <- ggplot(team_summary_formatted, aes(x = field_pos_bucket, y = dist_bucket)) +
    geom_tile(aes(fill = avg_rank), color = "#444444", linewidth = 1.5) +
    # *** Text size increased to 3.5 ***
    geom_text(aes(label = plot_label), color = ifelse(team_summary_formatted$plot_label == "N/A", "gray70", "black"), size = 3.5, fontface = "bold", family = "Source Sans Pro") +
    scale_fill_gradient2(low = "#1a9641", mid = "#ffffbf", high = "#d7191c", midpoint = 16, name = "Avg.\nRank", na.value = "#2b2b2b") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top", limits = field_pos_order, labels = function(x) str_wrap(x, width = 12)) +
    labs(
      title = str_wrap(paste(toupper(team_info$team_name), "OFFENSIVE SUCCESS RANK (vs. NFL)"), 85),
      caption = "Color = Avg. Rank of Run & Pass Success Rate. Inside: Run Rank | Pass Rank (1=Best, 32=Worst)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
      plot.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(color = "white", size = 11),
      plot.title = element_text(size = 16, color = "white", hjust = 0.5, margin = margin(t=5, b=10)),
      plot.caption = element_text(size = 9, color = "gray", face = "italic"),
      legend.title = element_text(), legend.text = element_text()
    )
  
  return(heatmap_plot)
}


# =================================================================
# == FUNCTION 4: DETAILED MATCHUP TABLE  ==
# =================================================================
generate_detailed_matchup_table <- function(off_team_abbr, def_team_abbr) {
  off_team_info <- teams_colors_logos %>% filter(team_abbr == !!off_team_abbr)
  def_team_info <- teams_colors_logos %>% filter(team_abbr == !!def_team_abbr)
  # (The data prep part of the function is unchanged)
  pbp_detailed <- pbp_final %>%
    mutate(
      breakdown = case_when(
        play_category == 'Pass' & shotgun == 1 ~ "Pass: Shotgun",
        play_category == 'Pass' & shotgun == 0 ~ "Pass: Under Center",
        play_category == 'Run' & run_location == 'left' ~ "Run: Left",
        play_category == 'Run' & run_location == 'middle' ~ "Run: Middle",
        play_category == 'Run' & run_location == 'right' ~ "Run: Right",
        TRUE ~ "Other"
      )
    ) %>%
    filter(breakdown != "Other")
  league_off_ranks <- pbp_detailed %>%
    group_by(posteam, dist_bucket, breakdown) %>%
    filter(n() > 4) %>%
    summarise(success_rate = mean(success, na.rm = TRUE), .groups = 'drop') %>%
    group_by(dist_bucket, breakdown) %>%
    mutate(off_success_rank = rank(-success_rate, ties.method = "min")) %>%
    select(posteam, dist_bucket, breakdown, off_success_rank)
  league_def_ranks <- pbp_detailed %>%
    group_by(defteam, dist_bucket, breakdown) %>%
    filter(n() > 4) %>%
    summarise(success_rate_allowed = mean(success, na.rm = TRUE), .groups = 'drop') %>%
    group_by(dist_bucket, breakdown) %>%
    mutate(def_success_rank = rank(success_rate_allowed, ties.method = "min")) %>%
    select(defteam, dist_bucket, breakdown, def_success_rank)
  offense_summary <- pbp_detailed %>%
    filter(posteam == off_team_abbr) %>%
    group_by(posteam, dist_bucket, breakdown) %>%
    summarise(off_plays = n(), off_epa = mean(epa, na.rm = TRUE), off_ypp = mean(yards_gained, na.rm = TRUE), .groups = 'drop') %>%
    left_join(league_off_ranks, by = c("posteam", "dist_bucket", "breakdown"))
  defense_summary <- pbp_detailed %>%
    filter(defteam == def_team_abbr) %>%
    group_by(defteam, dist_bucket, breakdown) %>%
    summarise(def_plays = n(), def_epa_allowed = mean(epa, na.rm = TRUE), def_ypp_allowed = mean(yards_gained, na.rm = TRUE), .groups = 'drop') %>%
    left_join(league_def_ranks, by = c("defteam", "dist_bucket", "breakdown"))
  full_summary <- offense_summary %>%
    full_join(defense_summary, by = c("dist_bucket", "breakdown")) %>%
    rename(Situation = dist_bucket, `Play Type` = breakdown) %>%
    arrange(Situation, desc(`Play Type`)) %>%
    filter(!is.na(Situation) & (off_plays > 2 | def_plays > 2))
  header_html <- html(paste0(
    "<div style='margin-bottom:20px;'><div style='display:flex; align-items:center; justify-content:center; gap:20px; margin-bottom:10px;'>",
    "<img src='", off_team_info$team_logo_espn, "' style='height:60px;'>",
    "<span style='font-family:\"Source Sans Pro\"; font-weight:900; font-size:40px;'>VS</span>",
    "<img src='", def_team_info$team_logo_espn, "' style='height:60px;'></div>",
    "<div style='font-family:\"Source Sans Pro\"; font-weight:700; font-size:30px; text-align:center;'>",
    "<span style='color:", off_team_info$team_color, ";'>", toupper(off_team_info$team_name), "</span>",
    "<span style='color:white;'> OFFENSE vs. </span>",
    "<span style='color:", def_team_info$team_color, ";'>", toupper(def_team_info$team_name), "</span>",
    "<span style='color:white;'> DEFENSE</span></div></div>"
  ))
  
  # Create the gt table
  gt_table <- full_summary %>%
    select(
      Situation, `Play Type`, off_plays, off_epa, off_ypp, off_success_rank,
      def_plays, def_epa_allowed, def_ypp_allowed, def_success_rank
    ) %>%
    gt(groupname_col = "Situation", rowname_col = "Play Type") %>%
    tab_header(title = header_html) %>%
    tab_spanner(label = md(paste0(off_team_abbr, " OFFENSE")), columns = c(off_plays, off_epa, off_ypp, off_success_rank)) %>%
    tab_spanner(label = md(paste0(def_team_abbr, " DEFENSE")), columns = c(def_plays, def_epa_allowed, def_ypp_allowed, def_success_rank)) %>%
    cols_label(
      off_plays = "Plays", off_epa = "EPA/Play", off_ypp = "Yards/Play", off_success_rank = "Success Rank",
      def_plays = "Plays", def_epa_allowed = "EPA/Play", def_ypp_allowed = "Yards/Play", def_success_rank = "Success Rank"
    ) %>%
    fmt_number(columns = c(off_epa, off_ypp, def_epa_allowed, def_ypp_allowed), decimals = 2) %>%
    fmt_number(columns = c(off_success_rank, def_success_rank), decimals = 0) %>%
    fmt_missing(columns = everything(), missing_text = "-") %>%
    # *** FIX IS HERE: Removed the 'domain' argument to make scales dynamic ***
    data_color(columns = c(off_epa), colors = scales::col_numeric(palette = c("#d7191c", "#ffffbf", "#1a9641"), domain = NULL)) %>%
    data_color(columns = c(off_ypp), colors = scales::col_numeric(palette = c("#d7191c", "#ffffbf", "#1a9641"), domain = NULL)) %>%
    data_color(columns = c(def_epa_allowed), colors = scales::col_numeric(palette = c("#1a9641", "#ffffbf", "#d7191c"), domain = NULL)) %>%
    data_color(columns = c(def_ypp_allowed), colors = scales::col_numeric(palette = c("#1a9641", "#ffffbf", "#d7191c"), domain = NULL)) %>%
    data_color(columns = c(off_success_rank, def_success_rank), colors = scales::col_numeric(palette = c("#1a9641", "#ffffbf", "#d7191c"), domain = c(1, 32))) %>%
    # Theming
    opt_table_font(font = "Source Sans Pro", weight = 600) %>%
    tab_style(style = list(cell_fill(color = off_team_info$team_color), cell_text(color = off_team_info$team_color2, weight=900)), locations = cells_column_spanners(spanners = md(paste0(off_team_abbr, " OFFENSE")))) %>%
    tab_style(style = list(cell_fill(color = def_team_info$team_color), cell_text(color = def_team_info$team_color2, weight=900)), locations = cells_column_spanners(spanners = md(paste0(def_team_abbr, " DEFENSE")))) %>%
    tab_style(style = cell_borders(sides = "all", color = "#555555", weight = px(1.5)), locations = cells_body()) %>%
    tab_style(style = cell_borders(sides = "all", color = "#555555", weight = px(1.5)), locations = cells_column_labels()) %>%
    tab_style(style = list(cell_fill(color = "#333333"), cell_text(weight = "bold", size = px(16))), locations = cells_row_groups()) %>%
    cols_width(`Play Type` ~ px(200), everything() ~ px(100)) %>%
    tab_options(heading.border.bottom.style = "hidden", table.background.color = "#1A1A1A", column_labels.background.color = "#2A2A2A")
  
  return(gt_table)
}

# =================================================================
# == FUNCTION 5: QB PASSING HEATMAP ==
# =================================================================
generate_passing_heatmap <- function(qb_name, team_abbr, opponent_abbr, pbp_data, matchup_weeks) {
  
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  # Filter for the QB's pass plays ONLY from the specified matchup weeks
  qb_passes <- pbp_data %>%
    filter(passer_player_name == qb_name,
           week %in% matchup_weeks,
           !is.na(pass_location),
           !is.na(air_yards))
  
  # Return a themed message if there are no head-to-head passes
  if(nrow(qb_passes) == 0) {
    return(
      ggplot() + 
        labs(title = paste("No Head-to-Head Passing Data for", qb_name, "vs.", opponent_abbr)) + 
        theme(
          text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
          plot.background = element_rect(fill = "#1A1A1A", color = NA),
          panel.background = element_rect(fill = "#1A1A1A", color = NA),
          panel.grid = element_blank(),
          plot.title = element_text(size = 16, color = "white", hjust = 0.5, margin = margin(t=5, b=10))
        )
    )
  }
  
  # Create the 12 passing zones
  qb_passes_zoned <- qb_passes %>%
    mutate(
      depth_bucket = case_when(
        air_yards < 0 ~ "Behind LOS (-5 to 0)",
        air_yards >= 0 & air_yards < 10 ~ "Short (0-9)",
        air_yards >= 10 & air_yards < 20 ~ "Intermediate (10-19)",
        air_yards >= 20 ~ "Deep (20+)"
      ),
      pass_location = str_to_title(pass_location)
    )
  
  # Summarize completions and attempts in each zone
  zone_summary <- qb_passes_zoned %>%
    group_by(pass_location, depth_bucket) %>%
    summarise(
      completions = sum(complete_pass == 1, na.rm = TRUE),
      attempts = n(),
      .groups = "drop"
    ) %>%
    mutate(
      comp_pct = completions / attempts,
      plot_label = paste0(completions, "/", attempts, "\n", scales::percent(comp_pct, accuracy = 1))
    )
  
  # Define the plotting order for the axes
  zone_summary <- zone_summary %>%
    mutate(
      pass_location = factor(pass_location, levels = c("Left", "Middle", "Right")),
      depth_bucket = factor(depth_bucket, levels = c("Deep (20+)", "Intermediate (10-19)", "Short (0-9)", "Behind LOS (-5 to 0)"))
    )
  
  # Create the plot with the final dark theme and markings
  pass_map <- ggplot(zone_summary, aes(x = pass_location, y = depth_bucket)) +
    geom_hline(yintercept = c(2.5, 3.5), color="white", linetype="22", alpha=0.5) +
    geom_vline(xintercept = c(1.5, 2.5), color="white", linetype="22", alpha=0.5) +
    geom_tile(aes(fill = comp_pct), color = "white", linewidth = 2, alpha = 0.85) +
    geom_text(aes(label = plot_label), color="black", size = 6, fontface = "bold", lineheight = .8) +
    geom_hline(yintercept = 1.5, color = "#fec525", linewidth = 2) +
    annotate(
      geom = "text",
      x = 3.6,
      y = 1.5,
      label = "◄ LOS",
      color = "#fec525",
      fontface = "bold",
      size = 4,
      hjust = "left"
    ) +
    scale_fill_gradientn(
      colors = c("#d7191c", "#1a9641", "#ffffbf"), # Red, Green, Yellow
      na.value = "grey50", 
      name = "Comp %", 
      labels = scales::percent
    ) +
    scale_y_discrete(limits = rev(levels(zone_summary$depth_bucket))) +
    scale_x_discrete(position = "top") +
    coord_cartesian(clip = "off") +
    labs(
      title = paste(qb_name, "Passing Chart vs.", opponent_abbr),
      subtitle = paste("2024 Head-to-Head Matchups (Week(s):", paste(matchup_weeks, collapse=", "), ")")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
      plot.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.grid = element_blank(),
      # *** FIX IS HERE: Increased title and subtitle font sizes ***
      plot.title = element_text(size = 28, hjust = 0.5, margin = margin(t=10, b=5)),
      plot.subtitle = element_text(size = 20, hjust = 0.5, face="italic", color="gray90"),
      axis.text = element_text(color="white", face="bold", size=15),
      axis.title = element_blank(),
      legend.background = element_rect(fill = "#1A1A1A", color = NA),
      legend.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      plot.margin = margin(5, 40, 5, 5)
    )
  
  return(pass_map)
}

# =================================================================
# == FUNCTION 6: RUN DIRECTION HEATMAP ==
# =================================================================

generate_run_direction_map <- function(team_abbr, pbp_data) {
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  run_summary <- pbp_data %>%
    filter(posteam == team_abbr, play_category == 'Run', run_location %in% c('left', 'middle', 'right')) %>%
    group_by(run_location) %>%
    summarise(
      n_runs = n(),
      epa_per_run = mean(epa, na.rm=TRUE),
      ypr = mean(yards_gained, na.rm=TRUE)
    ) %>%
    mutate(
      run_location = str_to_title(run_location),
      plot_label = paste0(
        "Runs: ", n_runs, "\n",
        "YPC: ", sprintf("%.1f", ypr), "\n",
        "EPA: ", sprintf("%.2f", epa_per_run)
      )
    )
  
  ggplot(run_summary, aes(x = run_location, y = n_runs, fill = epa_per_run)) +
    geom_col(color = "white", width=0.8) +
    geom_text(aes(label = plot_label), vjust = 1.5, color="white", size=5, fontface="bold") +
    scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", midpoint = 0, name="EPA/Run") +
    labs(
      title = paste(team_info$team_name, "Run Direction Tendency & Efficiency"),
      subtitle = "2024 Season",
      y = "Number of Runs"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "Source Sans Pro", color = "white", face="bold"),
      plot.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.background = element_rect(fill = "#1A1A1A", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(color="white", face="bold", size=12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color="white", face="bold", size=14),
      plot.title = element_text(size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, face="italic", color="gray90"),
      legend.position = "none"
    )
}

# =================================================================
# == FUNCTION 7: OFFSEASON MOVES ANALYSIS ==
# =================================================================
generate_moves_list <- function(team_abbr, moves_data) {
  
  # Handle case where the Excel file doesn't exist
  if (is.null(moves_data) || nrow(moves_data) == 0) {
    return(gt(tibble(Message="NFL_Offseason_Moves.xlsx not found in app directory.")) %>% gt_theme_538())
  }
  
  # --- NEW, ACCURATE LOGIC BASED ON YOUR RULES ---
  
  # Get the official nickname for the selected team (e.g., "GB" -> "Packers")
  current_team_nick <- teams_colors_logos %>%
    filter(team_abbr == !!team_abbr) %>%
    pull(team_nick)
  
  # 1. Find all players ADDED by the selected team from their own sheet.
  # An addition is any player listed with a "From" value that isn't "retiring".
  additions <- moves_data %>%
    filter(team_abbr == !!team_abbr, !is.na(From), tolower(From) != "retiring") %>%
    select(Player, Position) %>%
    mutate(Status = "Incoming")
  
  # 2. Find players LOST to Retirement from this team's sheet.
  losses_retired <- moves_data %>%
    filter(team_abbr == !!team_abbr, tolower(From) == "retiring") %>%
    select(Player, Position) %>%
    mutate(Status = "Outgoing (Retired)")
  
  # 3. Find players LOST to Free Agency/Trade by searching the ENTIRE dataset 
  # for anyone whose "From" column matches this team's nickname.
  losses_fa_trade <- moves_data %>%
    filter(From == current_team_nick) %>%
    select(Player, Position) %>%
    mutate(Status = "Outgoing (FA/Trade)")
  
  # 4. Combine all moves into one comprehensive list
  final_list <- bind_rows(additions, losses_fa_trade, losses_retired) %>%
    distinct() %>% # Ensure no duplicates
    arrange(Status, Player)
  
  # If no moves are found for the team after all checks, return a message
  if (nrow(final_list) == 0) {
    return(
      gt(tibble(Message = paste("No notable Incoming/Outgoing players found for", team_abbr))) %>%
        gt_theme_538() %>%
        tab_options(table.width = pct(100))
    )
  }
  
  # Create the final gt table
  final_list %>%
    gt() %>%
    tab_header(title = md(paste0("**Key Offseason Moves: ", team_abbr, "**"))) %>%
    cols_align(align = "left", columns = everything()) %>%
    tab_style(style = cell_fill(color = "#dcf0e8"), locations = cells_body(rows = Status == "Incoming")) %>%
    tab_style(style = cell_fill(color = "#fbeceb"), locations = cells_body(rows = str_detect(Status, "Outgoing"))) %>%
    gt_theme_538() %>%
    tab_options(table.width = pct(100))
}


# =====================================================================
# == FUNCTION 8: 2025 DRAFT CLASS REPORT                             ==
# =====================================================================
generate_draft_report <- function(team_abbr, draft_data) {
  
  # Handle case where the Excel file doesn't exist or is empty
  if (is.null(draft_data) || nrow(draft_data) == 0) {
    return(gt(tibble(Message="NFL_2025_Draft_Results.xlsx not found in app directory.")) %>% gt_theme_538())
  }
  
  # --- THIS IS THE FIX ---
  # 1. Look up the team's nickname (e.g., "Packers") from their abbreviation (e.g., "GB")
  team_nickname <- teams_colors_logos %>%
    filter(team_abbr == !!team_abbr) %>%
    pull(team_nick)
  
  # 2. Filter the draft data using the correct nickname
  draft_class <- draft_data %>%
    filter(TEAM == team_nickname) %>%
    # Ensure columns are named exactly as they appear in your file
    select(
      Round = RD,
      Pick = PICK,
      Player = PLAYER,
      College,
      Position,
      Pos_Rank = `Position Rank`
    )
  
  # If no draft picks are found for the team, return a message
  if (nrow(draft_class) == 0) {
    return(
      gt(tibble(Message = paste("No 2025 draft picks found for", team_abbr))) %>%
        gt_theme_538() %>%
        tab_options(table.width = pct(100))
    )
  }
  
  # Get team colors for theming
  team_info <- teams_colors_logos %>% filter(team_abbr == !!team_abbr)
  
  # Create the final gt table
  draft_class %>%
    gt() %>%
    tab_header(
      title = md(paste0("**2025 Draft Class: ", team_info$team_name, "**"))
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_width(
      Player ~ px(200),
      College ~ px(180),
      everything() ~ px(80)
    ) %>%
    gt_theme_dark() %>%
    tab_style(
      style = list(
        cell_fill(color = team_info$team_color),
        cell_text(color = team_info$team_color2)
      ),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(table.width = pct(100))
}

# =================================================================
# ==     3. SHINY UI (USER INTERFACE) - FINAL VERSION            ==
# =================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        body { background-color: #1A1A1A; color: white; }
        .well { background-color: #2b2b2b; }
        .btn-primary { background-color: #013369; border-color: #012345; font-weight: bold; }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus { background-color: #333333; border-color: #444444; color: white; }
        .nav-tabs > li > a { color: #ccc; }
        .gt_table { background-color: #1A1A1A !important; }
        hr { border-top: 1px solid #444; }
      ")),
    # --- CLT Logo removed. Only the embedded NFL logo remains ---
    tags$div(
      style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
      HTML('<svg xmlns="http://www.w3.org/2000/svg" width="60" height="60" viewBox="0 0 500 500" role="img" aria-label="NFL Shield Logo"><g fill="none" fill-rule="nonzero"><path fill="#FFF" d="M411.917 64.857c-15.037 10.733-43.622 25.491-80.544 16.398-63.87-15.801-82.182-70.807-82.182-70.807s-18.312 55.155-82.182 70.807c-36.922 9.093-65.507-5.516-80.544-16.398h-11.464v297.093c0 7.453 1.787 22.36 13.995 37.118 15.037 18.335 40.496 30.112 75.78 35.18 28.138 4.025 48.833 12.969 63.423 26.981 10.719 10.435 20.992 28.323 20.992 28.323s10.571-18.335 20.992-28.323c14.59-14.161 35.434-22.957 63.423-26.981 35.285-5.068 60.743-16.994 75.78-35.18 12.208-14.758 13.995-29.665 13.995-37.118v-297.093h-11.464z"/><path fill="#013369" d="M412.959 76.484s-34.987 27.28-84.118 14.907c-57.319-14.311-79.651-57.391-79.651-57.391s-22.332 43.081-79.651 57.391c-49.131 12.373-84.118-14.907-84.118-14.907v285.466c0 9.689 4.02 50.832 80.842 62.012 30.372 4.323 53.002 14.161 69.229 29.814 5.806 5.665 10.273 11.329 13.697 16.547 3.424-5.217 7.891-11.031 13.697-16.547 16.228-15.652 38.858-25.491 69.229-29.814 76.822-11.18 80.842-52.323 80.842-62.012v-285.466z"/><path fill="#FFF" d="M249.19 441.404s22.332-27.429 80.098-36.373c66.252-10.286 64.316-41.441 64.316-42.932v-149.665h-288.977v149.516c0 1.64-1.935 32.795 64.316 42.932 57.915 8.944 80.247 36.522 80.247 36.522z"/><path fill="#D50A0A" d="M181.896 321.702v-71.106l-8.933-5.068v-18.484h40.496v18.484l-8.486 5.068v145.043l-21.737-6.559-31.414-92.72v61.118l9.975 7.602v18.783l-43.473-11.031v-16.099l9.975-3.876v-102.41l-10.124-4.919v-18.484h32.903l30.818 94.658zm74.589 64.994l8.635 10.435v18.484l-43.175-16.099v-15.652l9.528-3.876v-129.242l-9.528-4.77v-18.634h74.887v37.565h-16.526l-5.062-15.354h-19.057v43.379h14.293l5.657-8.199h11.464v38.311h-11.464l-5.36-7.752h-14.59v71.404h.298zm47.642 6.559v-18.484l10.719-8.795v-115.379l-9.082-4.77v-18.634h43.175v18.634l-8.486 4.77v108.224l18.61-5.068 3.573-28.323h17.568v47.404l-76.078 20.422z"/><path fill="#FFF" d="M112.22 181.727l-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.764 15.354-12.953-9.093-12.953 9.093 5.062-15.354zm56.724 0l-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.764 15.354-12.953-9.093-12.953 9.093 5.062-15.354zm-56.724-51.13l-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.764 15.354-12.953-9.093-12.953 9.093 5.062-15.354zm56.724 0l-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.764 15.354-12.953-9.093-12.953 9.093 5.062-15.354zm221.981 66.634l-12.953-9.093-12.953 9.093 4.764-15.354-13.101-9.839h16.079l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.913 15.354zm-56.575 0l-12.953-9.093-12.953 9.093 4.764-15.354-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 5.062 15.354zm56.575-51.28l-12.953-9.093-12.953 9.093 4.764-15.354-13.101-9.839h16.079l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 4.913 15.354zm-56.575 0l-12.953-9.093-12.953 9.093 4.764-15.354-13.101-9.839h15.93l5.211-15.205 5.211 15.205h15.93l-13.101 9.839 5.062 15.354zm-49.131-56.497c1.34-.149 2.084 1.491 2.829 2.534 5.657 14.311 7.146 31.752 2.829 48.447-6.551 25.491-27.99 48.894-73.994 54.857-1.042.149-2.233-.149-2.382-.745 2.084-9.689 7.295-28.174 18.461-47.702 2.084 1.342 4.169 2.683 6.104 4.174.744-1.193 1.638-2.534 2.978-4.77-5.657-4.472-12.208-7.602-19.503-8.944 0 0-1.34 2.832-2.382 4.919 0 0 2.829.596 6.253 1.789-9.975 21.168-13.399 40.845-14.293 49.789-2.829-4.472-5.062-14.907-5.806-20.571-7.295-53.665 31.265-84.969 73.249-85.565 2.829-.149 3.871.447 3.871.447s-9.231 3.429-19.95 11.627c0 0-2.531-1.043-5.211-1.789-1.638 1.043-3.424 2.236-5.062 3.429 5.062 1.193 11.464 3.727 17.568 8.795 1.34-1.193 2.68-2.534 4.169-3.876-1.489-1.193-3.871-2.832-5.955-4.025 9.528-9.242 16.228-12.82 16.228-12.82zm-39.602 50.683c1.042-1.491 2.233-3.28 3.424-5.068-4.764-4.025-11.613-7.901-19.95-9.54-1.191 1.789-3.275 5.217-3.275 5.217 6.849 1.342 13.846 4.472 19.801 9.391zm7.593-10.584c1.191-1.64 2.382-3.13 3.722-4.77-6.551-5.366-13.995-8.497-19.801-9.54-1.34 1.491-2.68 3.13-3.871 4.621 7.593 1.491 14.441 5.068 19.95 9.689zm8.486-10.137c1.191-1.491 2.531-2.832 4.169-4.323-4.02-3.28-9.826-7.155-18.461-9.242-1.489 1.193-3.275 2.534-4.913 4.174 6.849 1.491 13.399 4.621 19.206 9.391z"/></g></svg>')
    ),
    
    titlePanel("NFL Matchup Tendency Report Generator"),
    
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Matchup"),
        selectInput("team1", "Team 1:", choices = team_list, selected = "GB"),
        selectInput("team2", "Team 2:", choices = team_list, selected = "DET"),
        actionButton("go", "Generate Report", class = "btn-primary btn-lg", icon = icon("bolt")),
        hr(),
        h5("About"),
        p("This tool generates a situational tendency report for any NFL matchup using 2024 regular season play-by-play data from nflfastR.")
      ),
      
      mainPanel(
        width = 9,
        uiOutput("main_report_content")
      )
    )
  )
)

# =================================================================
# ==     4. SHINY SERVER (BACKEND LOGIC) - FINAL VERSION         ==
# =================================================================
server <- function(input, output, session) {
  
  # This single reactive block will generate all our data and UI components
  report_data <- eventReactive(input$go, {
    
    team1 <- input$team1
    team2 <- input$team2
    
    withProgress(message = 'Generating report...', value = 0, {
      
      incProgress(0.1, detail = "Finding matchups & QBs...")
      matchup_weeks <- pbp_final %>% filter((posteam == team1 & defteam == team2) | (posteam == team2 & defteam == team1)) %>% pull(week) %>% unique()
      qb1_name <- pbp_final %>% filter(posteam == team1, play_category == 'Pass') %>% count(passer_player_name, sort = TRUE) %>% head(1) %>% pull(passer_player_name)
      qb2_name <- pbp_final %>% filter(posteam == team2, play_category == 'Pass') %>% count(passer_player_name, sort = TRUE) %>% head(1) %>% pull(passer_player_name)
      
      incProgress(0.2, detail = "Building Team 1 visuals...")
      run_map_1 <- generate_run_tendency_heatmap(team1)
      pass_map_1 <- generate_pass_tendency_heatmap(team1)
      rank_map_1 <- generate_rank_heatmap(team1)
      run_dir_map_1 <- generate_run_direction_map(team1, pbp_final)
      qb_map_1 <- if(length(matchup_weeks) > 0) generate_passing_heatmap(qb1_name, team1, team2, pbp_final, matchup_weeks) else { ggplot() + labs(title=paste("No H2H data for", qb1_name)) + theme_void() }
      
      incProgress(0.2, detail = "Building Team 2 visuals...")
      run_map_2 <- generate_run_tendency_heatmap(team2)
      pass_map_2 <- generate_pass_tendency_heatmap(team2)
      rank_map_2 <- generate_rank_heatmap(team2)
      run_dir_map_2 <- generate_run_direction_map(team2, pbp_final)
      qb_map_2 <- if(length(matchup_weeks) > 0) generate_passing_heatmap(qb2_name, team2, team1, pbp_final, matchup_weeks) else { ggplot() + labs(title=paste("No H2H data for", qb2_name)) + theme_void() }
      
      incProgress(0.1, detail = "Building matchup table...")
      matchup_table <- generate_detailed_matchup_table(team1, team2)
      
      incProgress(0.1, detail = "Analyzing offseason moves...")
      offseason_summary_1 <- generate_moves_list(team1, moves_data)
      offseason_summary_2 <- generate_moves_list(team2, moves_data)
      
      incProgress(0.1, detail = "Building draft reports...")
      draft_table_1 <- generate_draft_report(team1, draft_data)
      draft_table_2 <- generate_draft_report(team2, draft_data)
      
      list(
        team1 = team1, team2 = team2,
        run_map_1 = run_map_1, pass_map_1 = pass_map_1, rank_map_1 = rank_map_1, run_dir_map_1 = run_dir_map_1, qb_map_1 = qb_map_1,
        run_map_2 = run_map_2, pass_map_2 = pass_map_2, rank_map_2 = rank_map_2, run_dir_map_2 = run_dir_map_2, qb_map_2 = qb_map_2,
        matchup_table = matchup_table,
        offseason_summary_1 = offseason_summary_1,
        offseason_summary_2 = offseason_summary_2,
        draft_table_1 = draft_table_1,
        draft_table_2 = draft_table_2
      )
    })
  })
  
  # This single renderUI block builds the entire main panel dynamically
  output$main_report_content <- renderUI({
    
    data <- report_data()
    req(data)
    
    tagList(
      h2(paste(data$team1, "vs.", data$team2, "Report"), style="text-align: center;"),
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Team 1 Report", 
          h3(paste(data$team1, "Offensive Report"), style="text-align: center;"),
          hr(), plotOutput("run_map_1_out"), helpText("Shows the percentage of time the team runs in each situation."),
          hr(), plotOutput("pass_map_1_out"), helpText("Shows the percentage of time the team passes in each situation."),
          hr(), plotOutput("rank_map_1_out"), helpText("Ranks the team's offensive success rate against the rest of the league."),
          hr(), plotOutput("run_dir_map_1_out"), helpText("Shows run frequency and efficiency for each run direction."),
          hr(), plotOutput("qb_map_1_out"), helpText("Shows the QB's completion percentage in head-to-head matchups.")
        ),
        tabPanel(
          "Team 2 Report",
          h3(paste(data$team2, "Offensive Report"), style="text-align: center;"),
          hr(), plotOutput("run_map_2_out"), helpText("Shows the percentage of time the team runs in each situation."),
          hr(), plotOutput("pass_map_2_out"), helpText("Shows the percentage of time the team passes in each situation."),
          hr(), plotOutput("rank_map_2_out"), helpText("Ranks the team's offensive success rate against the rest of the league."),
          hr(), plotOutput("run_dir_map_2_out"), helpText("Shows run frequency and efficiency for each run direction."),
          hr(), plotOutput("qb_map_2_out"), helpText("Shows the QB's completion percentage in head-to-head matchups.")
        ),
        tabPanel("Detailed Matchup", 
                 br(),
                 gt_output("matchup_table_out"),
                 helpText("Compares season-long efficiency and ranks for specific play types.")
        ),
        tabPanel("Offseason Impact",
                 fluidRow(
                   # *** FIX IS HERE: Using textOutput for dynamic headers ***
                   column(6, h4(textOutput("impact_header_1_out")), gt_output("impact_table_1_out")),
                   column(6, h4(textOutput("impact_header_2_out")), gt_output("impact_table_2_out"))
                 ),
                 hr(),
                 helpText("Lists key players added or lost by each team during the offseason, based on the provided Excel file.")
        ),
        tabPanel("Draft Report",
                 fluidRow(
                   column(6, h4(textOutput("draft_header_1_out")), gt_output("draft_table_1_out")),
                   column(6, h4(textOutput("draft_header_2_out")), gt_output("draft_table_2_out"))
                 ),
                 hr(),
                 helpText("Displays the full 2025 draft class for each team.")
        )
      )
    )
  })
  
  # Render plots and tables to their respective output IDs
  output$run_map_1_out <- renderPlot({ report_data()$run_map_1 })
  output$pass_map_1_out <- renderPlot({ report_data()$pass_map_1 })
  output$rank_map_1_out <- renderPlot({ report_data()$rank_map_1 })
  output$run_dir_map_1_out <- renderPlot({ report_data()$run_dir_map_1 })
  output$qb_map_1_out <- renderPlot({ report_data()$qb_map_1 })
  
  output$run_map_2_out <- renderPlot({ report_data()$run_map_2 })
  output$pass_map_2_out <- renderPlot({ report_data()$pass_map_2 })
  output$rank_map_2_out <- renderPlot({ report_data()$rank_map_2 })
  output$run_dir_map_2_out <- renderPlot({ report_data()$run_dir_map_2 })
  output$qb_map_2_out <- renderPlot({ report_data()$qb_map_2 })
  
  output$matchup_table_out <- render_gt({ report_data()$matchup_table })
  
  # *** FIX IS HERE: Added renderText calls for the dynamic headers ***
  output$impact_header_1_out <- renderText({ paste(report_data()$team1, "Key Moves") })
  output$impact_table_1_out <- render_gt({ report_data()$offseason_summary_1 })
  
  output$impact_header_2_out <- renderText({ paste(report_data()$team2, "Key Moves") })
  output$impact_table_2_out <- render_gt({ report_data()$offseason_summary_2 })
  
  output$draft_header_1_out <- renderText({ paste(report_data()$team1, "2025 Draft Class") })
  output$draft_table_1_out <- render_gt({ report_data()$draft_table_1 })
  
  output$draft_header_2_out <- renderText({ paste(report_data()$team2, "2025 Draft Class") })
  output$draft_table_2_out <- render_gt({ report_data()$draft_table_2 })
}
# =================================================================
# ==     5. RUN THE SHINY APPLICATION                            ==
# =================================================================
shinyApp(ui = ui, server = server)
