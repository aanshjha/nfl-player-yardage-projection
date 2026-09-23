# NFL Player Yardage Projection

An R pipeline that projects NFL player passing, rushing, receiving, and total
yardage for a selected matchup. It downloads weekly data with `nflreadr`, builds
rolling usage and efficiency features, fits ridge-regression models, applies
matchup adjustments, and writes projection tables to CSV.

## What it does

- Pulls weekly player, team, and snap-count data.
- Builds lagged rolling features for volume, market share, and efficiency.
- Backtests passing, rushing, and receiving projections.
- Estimates position-level uncertainty bands from backtest residuals.
- Adjusts projections using opponent matchup multipliers.
- Produces a role- and usage-filtered shortlist of RB, WR, and TE candidates.
- Uses the [`targets`](https://books.ropensci.org/targets/) package to cache and
  reproduce pipeline results.

## Requirements

- R
- Internet access for downloading data through `nflreadr`
- The following R packages:

```r
install.packages(c(
  "targets",
  "dplyr",
  "readr",
  "zoo",
  "glmnet",
  "nflreadr",
  "stringr",
  "tidyr",
  "purrr"
))
```

## Configure a projection

Edit `PARAMS` in `_targets.R` before running the pipeline:

```r
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
```

`target_week` is the week being projected. The pipeline uses data through the
preceding week. `clamp_low` and `clamp_high` limit the opponent matchup
adjustment.

## Run

From the project directory, start R and run:

```r
targets::tar_make("master_artifacts")
source("betting_filter_today.R")
```

To build the pipeline, refresh the shortlist, and print the main results in one
command, use:

```r
source("run_all.R")
```

`run_all.R` currently sets the working directory to
`~/Desktop/NFL Player Yardage Projection`. If the repository is stored
elsewhere, update or remove that `setwd()` line first.

## Outputs

The pipeline writes results to `artifacts/`, including:

| File | Description |
| --- | --- |
| `MASTER_latest_projections.csv` | Player-level raw, adjusted, and uncertainty-band projections |
| `latest_projections_adjusted_full.csv` | Full adjusted player projection table |
| `shortlist_latest.csv` | Players passing the configured usage and role filters |
| `uncertainty_bands_by_position.csv` | Position-level residual bands from backtesting |
| `<TEAM>_vs_<OPPONENT>_week_<WEEK>_matchup_multipliers.csv` | Matchup adjustment factors |
| `<TEAM>_vs_<OPPONENT>_week_<WEEK>_team_totals.csv` | Adjusted team totals |

## Project structure

```text
.
├── _targets.R                 # Pipeline definition and matchup parameters
├── R/functions.R              # Data, modeling, backtesting, and output helpers
├── betting_filter_today.R     # Shortlist filters
├── run_all.R                  # One-command runner and console summary
└── artifacts/                 # Generated CSV outputs
```

## Notes

- Team abbreviations should use the values recognized by `nflreadr`.
- The `_targets/` cache and local R session files are intentionally excluded
  from version control.
- Projections are estimates based on historical data and model assumptions;
  they are not guarantees of future performance or financial advice.
