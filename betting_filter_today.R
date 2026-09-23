# ============================================================
# betting_filter_today.R
# Reads: artifacts/MASTER_latest_projections.csv
# Writes: artifacts/shortlist_latest.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

IN_FILE  <- "artifacts/MASTER_latest_projections.csv"
OUT_FILE <- "artifacts/shortlist_latest.csv"

THRESH <- list(
  rb_carries_min     = 5,
  wrte_rec_adj_min   = 20,
  min_snaps_last     = 15,
  min_snap_pct_last  = 0.20
)

if (!file.exists(IN_FILE)) {
  message("⚠️  Master file not found. Run 'tar_make()' first.")
  if (!interactive()) quit(save="no") else stop("Stopping.")
}

x <- read_csv(IN_FILE, show_col_types = FALSE)

# Ensure columns exist
ensure_col <- function(df, col_name, default_val = NA) {
  if (!col_name %in% names(df)) df[[col_name]] <- default_val
  df
}

x <- x %>%
  ensure_col("proj_carries", 0) %>%
  ensure_col("proj_receiving_yards_adj", 0) %>%
  ensure_col("proj_total_yards_adj", 0) %>%
  ensure_col("offense_snaps_last", NA) %>%
  ensure_col("snap_pct_last", NA) %>%
  ensure_col("stable_role", FALSE)

x2 <- x %>%
  mutate(
    meets_usage_thresh = case_when(
      position == "RB" ~ proj_carries >= THRESH$rb_carries_min,
      position %in% c("WR", "TE") ~ proj_receiving_yards_adj >= THRESH$wrte_rec_adj_min,
      TRUE ~ FALSE
    ),
    # If stable_role exists (TRUE), use it. Else fall back to raw snaps.
    meets_snap_thresh = if_else(
      !is.na(stable_role) & stable_role == TRUE,
      TRUE,
      (is.na(offense_snaps_last) | offense_snaps_last >= THRESH$min_snaps_last) &
        (is.na(snap_pct_last) | snap_pct_last >= THRESH$min_snap_pct_last)
    ),
    
    shortlist_flag = meets_usage_thresh & meets_snap_thresh,
    
    shortlist_reason = case_when(
      !shortlist_flag ~ NA_character_,
      position == "RB" ~ "RB Volume + Role",
      position %in% c("WR", "TE") ~ "WR/TE Yds + Role",
      TRUE ~ "Qualifies"
    )
  )

shortlist <- x2 %>%
  filter(shortlist_flag) %>%
  arrange(desc(proj_total_yards_adj))

write_csv(shortlist, OUT_FILE)
message("Shortlist updated: ", OUT_FILE, " (", nrow(shortlist), " rows)")