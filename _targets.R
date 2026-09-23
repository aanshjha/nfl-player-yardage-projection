# ============================================================
# _targets.R
# Project: NFL Player Yardage Projection
# Run: source("run_all.R")
# ============================================================

library(targets)

tar_option_set(
  packages = c("dplyr","readr","zoo","glmnet","nflreadr","stringr","tidyr","purrr"),
  format = "rds"
)

source("R/functions.R")

PARAMS <- list(
  seasons = 2025,
  min_week_train = 5,
  out_dir = "artifacts",
  
  focus_team = "DET",
  matchup_opponent = "MIN",
  target_week = 17,
  
  clamp_low = 0.90,
  clamp_high = 1.10
)

list(
  tar_target(params, PARAMS),
  tar_target(out_dir, dir_create(params$out_dir)),
  
  # Data Pulls
  tar_target(player_week, pull_player_week(params$seasons), cue = tar_cue(mode = "always")),
  tar_target(team_week, pull_team_week(params$seasons), cue = tar_cue(mode = "always")),
  tar_target(snap_counts, pull_snap_counts(params$seasons), cue = tar_cue(mode = "always")),
  
  # Setup
  tar_target(latest_season, max(player_week$season, na.rm = TRUE)),
  tar_target(week_upto, as.integer(params$target_week - 1L)),
  
  # Features
  tar_target(player_week_features, build_player_week_features(player_week)),
  tar_target(player_week_with_snaps, add_last_week_snaps(player_week_features, snap_counts, week_upto)),
  
  # Backtesting & Bands
  tar_target(rec_backtest, backtest_receiving(player_week_with_snaps, params$min_week_train)),
  tar_target(rush_backtest, backtest_rushing(player_week_with_snaps, params$min_week_train)),
  tar_target(pass_backtest, backtest_passing(player_week_with_snaps, params$min_week_train)),
  tar_target(uncertainty_bands, compute_uncertainty_bands(rec_backtest, rush_backtest, pass_backtest)),
  
  # Matchup Logic
  tar_target(matchup_multipliers, compute_matchup_multipliers(team_week, params$matchup_opponent, week_upto, params$clamp_low, params$clamp_high)),
  
  # Predictions
  tar_target(pw_latest_season, player_week_with_snaps %>% filter(season == latest_season)),
  tar_target(rec_pred, fit_predict_receiving_for_week(pw_latest_season, week_upto, params$target_week)),
  tar_target(rush_pred, fit_predict_rushing_for_week(pw_latest_season, week_upto, params$target_week)),
  tar_target(pass_pred, fit_predict_passing_for_week(pw_latest_season, week_upto, params$target_week)),
  
  # Master Table
  tar_target(master_latest, build_master_for_matchup(rec_pred, rush_pred, pass_pred, params$focus_team, params$matchup_opponent, matchup_multipliers, uncertainty_bands, player_week_with_snaps, week_upto, params$target_week)),
  
  # Artifacts (Only write the essentials)
  tar_target(master_artifacts, write_master_artifacts(master_latest, matchup_multipliers, uncertainty_bands, params$out_dir, params$focus_team, params$matchup_opponent, params$target_week), format = "file")
)