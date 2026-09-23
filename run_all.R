# ============================================================
# run_all.R  (ONE COMMAND: builds + prints what matters)
# Folder: ~/Desktop/NFL Player Yardage Projection
# Run: source("run_all.R")
# ============================================================

suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(readr)
  library(nflreadr)
})

setwd("~/Desktop/NFL Player Yardage Projection")

cat(">> Checking pipeline status (Note: 'Skipped' targets are normal if data hasn't changed)...\n")
# ---- build pipeline ----
targets::tar_make("master_artifacts")

# ---- refresh shortlist ----
source("betting_filter_today.R")

# ---- params & paths ----
p <- tryCatch(targets::tar_read(params), error = function(e) NULL)
focus_team <- if (!is.null(p)) p$focus_team else "SF"
opp_team   <- if (!is.null(p)) p$matchup_opponent else "IND"
wk         <- if (!is.null(p)) p$target_week else 16

tag <- paste0(focus_team, "_vs_", opp_team, "_week_", wk)

# Exact filenames
mult_path   <- file.path("artifacts", paste0(tag, "_matchup_multipliers.csv"))
team_path   <- file.path("artifacts", paste0(tag, "_team_totals.csv"))
master_path <- "artifacts/MASTER_latest_projections.csv"
short_path  <- "artifacts/shortlist_latest.csv"
bands_path  <- "artifacts/uncertainty_bands_by_position.csv"

# ---- Helpers ----
read_safe <- function(path) {
  if (!file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

# Smart Column Picker: avoids crashing if a column is missing
smart_select <- function(df, ...) {
  wanted <- c(...)
  existing <- intersect(wanted, names(df))
  df %>% select(all_of(existing))
}

cat("\n==============================\n")
cat("MATCHUP:", focus_team, "vs", opp_team, "(Week", wk, ")\n")
cat("==============================\n")

# 1) MATCHUP MULTIPLIERS
cat("\n--- 1) MATCHUP MULTIPLIERS ---\n")
mult <- read_safe(mult_path)
if(nrow(mult) > 0) print(mult) else cat("File not found or empty.\n")

# 2) TEAM TOTALS
cat("\n--- 2) TEAM TOTALS (ADJUSTED) ---\n")
tt <- read_safe(team_path)
if(nrow(tt) > 0) print(tt) else cat("File not found (check tar_make output).\n")

# 3) PLAYER PROJECTIONS
cat("\n--- 3) PLAYER PROJECTIONS (Top Drivers) ---\n")
pa <- read_safe(master_path)
if (nrow(pa) == 0) {
  cat("Master file missing or empty.\n")
} else {
  # Robust filtering even if column names drift
  cols_to_show <- c(
    "player", "position", 
    "proj_total_yards_adj", "total_yards_adj", 
    "proj_receiving_yards_adj", "proj_rushing_yards_adj",
    "offense_snaps_last", "snap_pct_last", "stable_role",
    "proj_total_yards_p25", "proj_total_yards_p75"
  )
  
  # Standardize sort column
  sort_col <- if("proj_total_yards_adj" %in% names(pa)) "proj_total_yards_adj" else names(pa)[grep("total", names(pa))][1]
  
  pa %>%
    arrange(desc(.data[[sort_col]])) %>%
    smart_select(cols_to_show) %>%
    head(20) %>%
    print()
}

# 4) UNCERTAINTY BANDS
cat("\n--- 4) UNCERTAINTY BANDS ---\n")
bands <- read_safe(bands_path)
if(nrow(bands) > 0) print(bands) else cat("Missing bands file.\n")

# 5) SHORTLIST
cat("\n--- 5) SHORTLIST (Betting Candidates) ---\n")
short <- read_safe(short_path)
if (nrow(short) == 0) {
  cat("Shortlist empty or missing.\n")
} else {
  short %>%
    select(player, position, starts_with("proj_total"), starts_with("shortlist")) %>%
    head(25) %>%
    print()
}

cat("\nDone.\n")