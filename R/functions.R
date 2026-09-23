# ============================================================
# R/functions.R
# Shared helpers for NFL Player Yardage Projection targets pipeline
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(zoo)
  library(glmnet)
  library(nflreadr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ----------------------------
# filesystem & helpers
# ----------------------------
dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

pick_first_existing <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# ============================================================
# DATA PULLS
# ============================================================

pull_player_week <- function(seasons) {
  nflreadr::load_player_stats(seasons = seasons, summary_level = "week", file_type = "csv") %>%
    rename_with(tolower) %>% rename_with(trimws) %>%
    mutate(
      season = as.integer(season), week = as.integer(week),
      team = nflreadr::clean_team_abbrs(coalesce(!!!rlang::syms(intersect(names(.), c("recent_team","team","posteam"))))),
      opponent_team = if ("opponent_team" %in% names(.)) nflreadr::clean_team_abbrs(opponent_team) else NA_character_
    )
}

pull_team_week <- function(seasons) {
  nflreadr::load_team_stats(seasons = seasons, summary_level = "week", file_type = "csv") %>%
    rename_with(tolower) %>% rename_with(trimws) %>%
    mutate(
      season = as.integer(season), week = as.integer(week),
      team = nflreadr::clean_team_abbrs(team),
      opponent_team = if ("opponent_team" %in% names(.)) nflreadr::clean_team_abbrs(opponent_team) else NA_character_
    )
}

pull_snap_counts <- function(seasons) {
  nflreadr::load_snap_counts(seasons = seasons) %>%
    rename_with(tolower) %>% rename_with(trimws) %>%
    mutate(season = as.integer(season), week = as.integer(week), team = nflreadr::clean_team_abbrs(team))
}

# ============================================================
# FEATURES
# ============================================================

build_player_week_features <- function(player_week) {
  df <- player_week %>% rename_with(tolower) %>% rename_with(trimws)
  player_col <- pick_first_existing(names(df), c("player_display_name","player_name","player"))
  pos_col <- pick_first_existing(names(df), c("position","pos"))
  
  out <- df %>%
    transmute(
      season = as.integer(season), week = as.integer(week),
      team = nflreadr::clean_team_abbrs(team),
      opponent_team = if ("opponent_team" %in% names(df)) nflreadr::clean_team_abbrs(opponent_team) else NA_character_,
      player = .data[[player_col]], position = .data[[pos_col]],
      targets = safe_num(targets), receptions = safe_num(receptions), receiving_yards = safe_num(receiving_yards),
      carries = safe_num(carries), rushing_yards = safe_num(rushing_yards),
      attempts = if ("attempts" %in% names(df)) safe_num(attempts) else NA_real_,
      passing_yards = if ("passing_yards" %in% names(df)) safe_num(passing_yards) else NA_real_
    ) %>%
    filter(position %in% c("QB","RB","FB","WR","TE")) %>%
    
    group_by(season, week, team) %>%
    mutate(
      team_targets_week = sum(targets, na.rm = TRUE),
      team_carries_week = sum(carries, na.rm = TRUE)
    ) %>%
    mutate(
      target_share = if_else(team_targets_week > 0, targets / team_targets_week, NA_real_),
      rush_share   = if_else(team_carries_week > 0, carries / team_carries_week, NA_real_)
    ) %>%
    ungroup() %>%
    
    arrange(player, season, week) %>%
    group_by(player) %>%
    mutate(
      targets_roll_3_l1 = lag(rollapply(targets, 3, mean, fill = NA, align = "right", partial = TRUE), 1),
      targets_roll_5_l1 = lag(rollapply(targets, 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      share_roll_3_l1   = lag(rollapply(target_share, 3, mean, fill = NA, align = "right", partial = TRUE), 1),
      share_roll_5_l1   = lag(rollapply(target_share, 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      ypt_roll_5_l1     = lag(rollapply(if_else(targets > 0, receiving_yards/targets, NA_real_), 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      catch_roll_5_l1   = lag(rollapply(if_else(targets > 0, receptions/targets, NA_real_), 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      
      carries_roll_3_l1 = lag(rollapply(carries, 3, mean, fill = NA, align = "right", partial = TRUE), 1),
      carries_roll_5_l1 = lag(rollapply(carries, 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      rushshare_roll_3_l1 = lag(rollapply(rush_share, 3, mean, fill = NA, align = "right", partial = TRUE), 1),
      rushshare_roll_5_l1 = lag(rollapply(rush_share, 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      ypc_roll_5_l1     = lag(rollapply(if_else(carries > 0, rushing_yards/carries, NA_real_), 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      
      attempts_roll_5_l1 = lag(rollapply(attempts, 5, mean, fill = NA, align = "right", partial = TRUE), 1),
      pass_yds_roll_5_l1 = lag(rollapply(passing_yards, 5, mean, fill = NA, align = "right", partial = TRUE), 1)
    ) %>%
    ungroup()
  out
}

add_last_week_snaps <- function(player_week_features, snap_counts, week_upto) {
  sc <- snap_counts %>% rename_with(tolower) %>% rename_with(trimws)
  sc_player_col <- pick_first_existing(names(sc), c("player","player_display_name","name","full_name"))
  if (is.na(sc_player_col)) return(player_week_features %>% mutate(offense_snaps_last=NA, snap_pct_last=NA))
  
  sc_last <- sc %>% filter(week == week_upto) %>%
    transmute(season=as.integer(season), week=as.integer(week), team=nflreadr::clean_team_abbrs(team),
              player=as.character(.data[[sc_player_col]]), offense_snaps_last=safe_num(offense_snaps)) %>%
    group_by(season, week, team) %>%
    mutate(team_max_snaps = max(offense_snaps_last, na.rm = TRUE)) %>%
    mutate(snap_pct_last = if_else(team_max_snaps > 0, offense_snaps_last / team_max_snaps, NA_real_)) %>%
    ungroup() %>% select(season, team, player, offense_snaps_last, snap_pct_last)
  
  player_week_features %>% 
    left_join(sc_last, by = c("season","team","player")) %>%
    mutate(offense_snaps_last = safe_num(offense_snaps_last), snap_pct_last = safe_num(snap_pct_last))
}

# ============================================================
# MATCHUP MULTIPLIERS
# ============================================================

compute_matchup_multipliers <- function(team_week, opponent, week_upto, clamp_low=0.90, clamp_high=1.10) {
  tw <- team_week %>%
    filter(!is.na(season), !is.na(week)) %>%
    mutate(
      attempts = pmax(safe_num(attempts), 1), carries = pmax(safe_num(carries), 1),
      pass_epa_per_play = safe_num(passing_epa) / attempts,
      rush_epa_per_play = safe_num(rushing_epa) / carries
    )
  
  max_season <- max(tw$season, na.rm = TRUE)
  tw_s <- tw %>% filter(season == max_season, week <= week_upto)
  
  lg_pass <- mean(tw_s$pass_epa_per_play, na.rm = TRUE)
  lg_rush <- mean(tw_s$rush_epa_per_play, na.rm = TRUE)
  
  opp <- tw_s %>%
    filter(opponent_team == opponent) %>%
    summarise(
      opp_pass_allowed = mean(pass_epa_per_play, na.rm = TRUE),
      opp_rush_allowed = mean(rush_epa_per_play, na.rm = TRUE),
      .groups = "drop"
    )
  
  pass_mult <- 1 + (opp$opp_pass_allowed - lg_pass)
  rush_mult <- 1 + (opp$opp_rush_allowed - lg_rush)
  
  tibble(
    season = max_season, week_upto = as.integer(week_upto), opponent = opponent,
    pass_multiplier = as.numeric(min(max(pass_mult, clamp_low), clamp_high)),
    rush_multiplier = as.numeric(min(max(rush_mult, clamp_low), clamp_high))
  )
}

# ============================================================
# MODELS
# ============================================================

fit_predict_receiving_for_week <- function(pw, week_upto, target_week) {
  train <- pw %>% filter(!is.na(receiving_yards), week <= week_upto) %>% mutate(y=receiving_yards) %>% replace_na(list(targets_roll_3_l1=0, targets_roll_5_l1=0, share_roll_3_l1=0, share_roll_5_l1=0, ypt_roll_5_l1=0, catch_roll_5_l1=0))
  if(nrow(train)<10) return(tibble())
  fit <- glmnet::cv.glmnet(as.matrix(train %>% select(targets_roll_3_l1, targets_roll_5_l1, share_roll_3_l1, share_roll_5_l1, ypt_roll_5_l1, catch_roll_5_l1)), train$y, alpha = 0)
  
  latest <- pw %>% filter(week == week_upto) %>% replace_na(list(targets_roll_3_l1=0, targets_roll_5_l1=0, share_roll_3_l1=0, share_roll_5_l1=0, ypt_roll_5_l1=0, catch_roll_5_l1=0))
  pred <- as.numeric(predict(fit$glmnet.fit, newx = as.matrix(latest %>% select(targets_roll_3_l1, targets_roll_5_l1, share_roll_3_l1, share_roll_5_l1, ypt_roll_5_l1, catch_roll_5_l1)), s = fit$lambda.min))
  
  latest %>% 
    mutate(proj_receiving_yards = pmax(pred, 0), week = target_week) %>% 
    select(season, week, team, opponent_team, player, position, proj_receiving_yards)
}

fit_predict_rushing_for_week <- function(pw, week_upto, target_week) {
  train <- pw %>% filter(!is.na(carries), !is.na(rushing_yards), week <= week_upto) %>% mutate(ypc = if_else(carries>0, rushing_yards/carries, 0)) %>% replace_na(list(carries_roll_3_l1=0, carries_roll_5_l1=0, rushshare_roll_3_l1=0, rushshare_roll_5_l1=0, ypc_roll_5_l1=0))
  if(nrow(train)<10) return(tibble())
  fit_c <- glmnet::cv.glmnet(as.matrix(train %>% select(carries_roll_3_l1, carries_roll_5_l1, rushshare_roll_3_l1, rushshare_roll_5_l1)), train$carries, alpha = 0)
  fit_y <- glmnet::cv.glmnet(as.matrix(train %>% select(ypc_roll_5_l1, rushshare_roll_5_l1, carries_roll_5_l1)), train$ypc, alpha = 0)
  
  latest <- pw %>% filter(week == week_upto) %>% replace_na(list(carries_roll_3_l1=0, carries_roll_5_l1=0, rushshare_roll_3_l1=0, rushshare_roll_5_l1=0, ypc_roll_5_l1=0))
  pc <- as.numeric(predict(fit_c$glmnet.fit, newx = as.matrix(latest %>% select(carries_roll_3_l1, carries_roll_5_l1, rushshare_roll_3_l1, rushshare_roll_5_l1)), s = fit_c$lambda.min))
  py <- as.numeric(predict(fit_y$glmnet.fit, newx = as.matrix(latest %>% select(ypc_roll_5_l1, rushshare_roll_5_l1, carries_roll_5_l1)), s = fit_y$lambda.min))
  
  latest %>% 
    mutate(proj_carries = pmax(pc, 0), proj_rushing_yards = proj_carries * pmax(py, 0), week = target_week) %>% 
    select(season, week, team, opponent_team, player, position, proj_carries, proj_rushing_yards)
}

fit_predict_passing_for_week <- function(pw, week_upto, target_week) {
  train <- pw %>% filter(position=="QB", !is.na(passing_yards), week <= week_upto) %>% replace_na(list(attempts_roll_5_l1=0, pass_yds_roll_5_l1=0))
  if(nrow(train)<10) return(tibble())
  fit <- glmnet::cv.glmnet(as.matrix(train %>% select(attempts_roll_5_l1, pass_yds_roll_5_l1)), train$passing_yards, alpha = 0)
  
  latest <- pw %>% filter(position=="QB", week == week_upto) %>% replace_na(list(attempts_roll_5_l1=0, pass_yds_roll_5_l1=0))
  if(nrow(latest)==0) return(tibble())
  pred <- as.numeric(predict(fit$glmnet.fit, newx = as.matrix(latest %>% select(attempts_roll_5_l1, pass_yds_roll_5_l1)), s = fit$lambda.min))
  
  latest %>% 
    mutate(proj_passing_yards = pmax(pred, 0), week = target_week) %>% 
    select(season, week, team, opponent_team, player, position, proj_passing_yards)
}

# Backtesting wrappers - FIX: Coalesce NAs to 0 to prevent empty bands
backtest_receiving <- function(pw, min_w) map_dfr(unique(pw$season), function(s) { df<-pw%>%filter(season==s); map_dfr(sort(unique(df$week)), function(w) if(w<min_w) NULL else fit_predict_receiving_for_week(df,w-1,w) %>% left_join(df%>%filter(week==w),by=c("season","week","team","player","position")) %>% mutate(resid = coalesce(receiving_yards, 0) - coalesce(proj_receiving_yards, 0))) })
backtest_rushing <- function(pw, min_w) map_dfr(unique(pw$season), function(s) { df<-pw%>%filter(season==s); map_dfr(sort(unique(df$week)), function(w) if(w<min_w) NULL else fit_predict_rushing_for_week(df,w-1,w) %>% left_join(df%>%filter(week==w),by=c("season","week","team","player","position")) %>% mutate(resid = coalesce(rushing_yards, 0) - coalesce(proj_rushing_yards, 0))) })
backtest_passing <- function(pw, min_w) map_dfr(unique(pw$season), function(s) { df<-pw%>%filter(season==s); map_dfr(sort(unique(df$week)), function(w) if(w<min_w) NULL else fit_predict_passing_for_week(df,w-1,w) %>% left_join(df%>%filter(position=="QB",week==w),by=c("season","week","team","player","position")) %>% mutate(resid = coalesce(passing_yards, 0) - coalesce(proj_passing_yards, 0))) })

compute_uncertainty_bands <- function(r, u, p) {
  # FIX: Ensure we have data before quantiles to avoid all-NA rows
  calc <- function(d, n) {
    if(nrow(d) == 0) return(tibble(position=character(), !!paste0(n,"_p25"):=numeric(), !!paste0(n,"_p75"):=numeric()))
    d %>% filter(!is.na(resid)) %>% group_by(position) %>% summarise(!!paste0(n,"_p25"):=quantile(resid,0.25,na.rm=T), !!paste0(n,"_p75"):=quantile(resid,0.75,na.rm=T), .groups="drop")
  }
  reduce(list(calc(r,"rec_resid"), calc(u,"rush_resid"), calc(p,"pass_resid")), full_join, by="position")
}

# ============================================================
# MASTER & WRITING
# ============================================================

build_master_for_matchup <- function(rec, rush, pass, focus, opp, mult, bands, pw, w_upto, t_week) {
  base <- full_join(rec, rush, by=c("season","week","team","opponent_team","player","position")) %>%
    full_join(pass, by=c("season","week","team","opponent_team","player","position")) %>%
    mutate(team = nflreadr::clean_team_abbrs(team), opponent_team = opp) %>%
    filter(team == focus, week == t_week)
  
  snaps <- pw %>% filter(team == focus, week == w_upto) %>% select(season, team, player, offense_snaps_last, snap_pct_last) %>% distinct()
  
  base %>% left_join(snaps, by=c("season","team","player")) %>%
    mutate(
      across(c(proj_receiving_yards, proj_carries, proj_rushing_yards, proj_passing_yards), ~coalesce(., 0)),
      proj_receiving_yards_adj = proj_receiving_yards * mult$pass_multiplier[1],
      proj_rushing_yards_adj   = proj_rushing_yards * mult$rush_multiplier[1],
      proj_passing_yards_adj   = proj_passing_yards * mult$pass_multiplier[1],
      proj_total_yards_adj     = proj_receiving_yards_adj + proj_rushing_yards_adj + proj_passing_yards_adj,
      
      stable_role = !is.na(snap_pct_last) & snap_pct_last >= 0.60
    ) %>%
    left_join(bands, by="position") %>%
    mutate(
      proj_total_yards_p25 = pmax(proj_total_yards_adj + coalesce(rec_resid_p25,0) + coalesce(rush_resid_p25,0) + coalesce(pass_resid_p25,0), 0),
      proj_total_yards_p75 = pmax(proj_total_yards_adj + coalesce(rec_resid_p75,0) + coalesce(rush_resid_p75,0) + coalesce(pass_resid_p75,0), 0),
      
      # FIX: Add Explicit Risk-Adjusted Column (Floor)
      risk_adj_total_yards = proj_total_yards_p25
    ) %>%
    arrange(desc(proj_total_yards_adj))
}

write_master_artifacts <- function(master, mult, bands, out_dir, focus, opp, t_week) {
  dir_create(out_dir)
  tag <- paste0(focus, "_vs_", opp, "_week_", t_week)
  f_master <- file.path(out_dir, "MASTER_latest_projections.csv")
  f_full   <- file.path(out_dir, "latest_projections_adjusted_full.csv")
  f_mult   <- file.path(out_dir, paste0(tag, "_matchup_multipliers.csv"))
  f_team   <- file.path(out_dir, paste0(tag, "_team_totals.csv"))
  f_bands  <- file.path(out_dir, "uncertainty_bands_by_position.csv")
  
  team_totals <- master %>%
    group_by(team, opponent_team) %>%
    summarise(
      sum_proj_rush = sum(proj_rushing_yards_adj, na.rm = TRUE),
      sum_proj_pass = sum(proj_passing_yards_adj, na.rm = TRUE),
      sum_proj_total_offense = sum_proj_rush + sum_proj_pass, 
      .groups = "drop"
    )
  
  write_csv(master, f_master)
  write_csv(master, f_full)
  write_csv(mult, f_mult)
  write_csv(team_totals, f_team)
  write_csv(bands, f_bands)
  
  c(f_master, f_full, f_mult, f_team, f_bands)
}