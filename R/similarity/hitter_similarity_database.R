# ==============================================================================
# Hitter Similarity Database Builder
# ==============================================================================
# Constructs a SQLite database of minor league hitter career profiles and
# same-level/career-wide similarity comparisons using weighted z-score deltas.
#
# Pipeline: Load raw stats -> Calculate rates -> Build league context ->
# Apply regression/z-scores -> Aggregate profiles -> Score similarities -> Export
#
# Scoring: 4 weighted features (BB z-score, K z-score, ISO z-score, Speed)
# Distance: Weighted Euclidean delta in standardized space
# Scale: 1000-point similarity (higher = more similar)
# ==============================================================================

# Check for required packages before execution
required_packages <- c("arrow", "DBI", "RSQLite", "dplyr")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the database build.",
    call. = FALSE
  )
}

# ==============================================================================
# PIPELINE CONSTANTS
# ==============================================================================
# All tunable parameters and magic numbers organized for easy maintenance
# Modify these to adjust scoring behavior across the entire pipeline

# Bayesian regression shrinkage coefficients (pseudo plate-appearances/at-bats added)
# Larger values = more shrinkage toward league mean (more regularization)
REGRESSION_SHRINKAGE <- list(
  bb_pa = 65L,      # Walk rate: typical ~1/8 season
  k_pa = 35L,       # Strikeout rate: typical ~1/5 season
  iso = 85L,        # Isolated power: typical ~1/4 season
  obp = 197L,       # On-base percentage: typical ~2/5 season
  slg = 137L        # Slugging percentage: typical ~2/5 season
)

# Feature weights for similarity scoring
# Higher weight = more important in player comparison
# Must be positive numbers; they are normalized in distance calculation
SIMILARITY_WEIGHTS <- c(
  w_zbb = 1.4,      # Walk rate z-score importance
  w_zk = 1.2,       # Strikeout rate z-score importance
  w_ziso = 1.5,     # Isolated power z-score importance (highest)
  w_speed = 0.8     # Speed composite importance (lowest)
)

# Similarity scoring scale parameters
# Formula: similarity_score = SCORING_BASE - SCORING_DISTANCE_MULTIPLIER * weighted_delta
SCORING_BASE <- 1000       # Maximum possible similarity score
SCORING_DISTANCE_MULTIPLIER <- 125  # Scales weighted delta to 0-1000 range

# Speed composite score parameters
# Formula: speed = SPEED_BASE + SPEED_COEFFICIENT * (sum of 4 centered z-scores)
# Creates a 50-based scout scale (40-60 is typical range)
SPEED_BASE <- 50
SPEED_COEFFICIENT <- 4.25
SPEED_DISTANCE_SCALE <- 10  # Divide speed by 10 before distance calculation

# Recency weighting for career profile aggregation
# Players' recent seasons count more heavily; older seasons decay
RECENCY_WEIGHTS <- c(
  "current" = 1.00,    # Same year as latest season
  "prior_1" = 0.75,    # 1 year prior
  "prior_2" = 0.55,    # 2 years prior
  "older" = 0.35       # 3+ years prior
)

# Rolling window for league statistics stabilization
# Larger windows reduce year-to-year noise; smaller windows increase responsiveness
ROLLING_WINDOW_YEARS <- 3L

# Minimum plate appearance thresholds for inclusion in scoring
# Prevents unreliable statistics from small sample sizes from distorting similarity
MIN_PA_SIMILARITY <- 100L   # Minimum PA for player to be scored/compared

# Number of top comparable players to retain
TOP_N_COMPS <- 20L

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================
# Small, reusable functions for common data operations

# Safe division that handles zero and NA values (returns NA_real_ instead of Inf/NaN)
safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

collapse_unique <- function(x) {
  values <- unique(x[!is.na(x) & x != ""])
  paste(values, collapse = ", ")
}

# Calculate weighted mean ignoring NA and non-positive weight values
# Returns NA_real_ if no valid observations
weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Convert baseball level abbreviations to numeric scale for sorting
# Used to identify latest/highest level reached and order progression
# Scale: 1=Rookie (FRk, Rk, R, DSL), 2=Single-A (A-), 3=Single-A (A), 4=Double-A+ (A+, int, Wnt),
#        5=Triple-A (AA), 6=Major League (AAA), 7=MLB
level_value <- function(level) {
  values <- c(
    "FRk" = 1,
    "Rk" = 1,
    "R" = 1,
    "DSL" = 1,
    "A-" = 2,
    "A" = 3,
    "A+" = 4,
    "int" = 4,
    "Wnt" = 4,
    "AA" = 5,
    "AAA" = 6,
    "MLB" = 7
  )
  unname(values[as.character(level)])
}

# Wrapper for weighted_mean_safe that references columns by name in a data frame
weighted_mean_by_col <- function(data, value_col, weight_col) {
  weighted_mean_safe(data[[value_col]], data[[weight_col]])
}

# Calculate weighted mean of z-scores with compounded weights (denominator * recency)
# Used in career profile aggregation where weights reflect both sample size and seasonrecency
weighted_z_by_season <- function(data, value_col, denominator_col, recency_col) {
  weighted_mean_safe(data[[value_col]], data[[denominator_col]] * data[[recency_col]])
}

# ==============================================================================
# ROLLING WINDOW AGGREGATION
# ==============================================================================
# Calculate rolling aggregates (sums/means) within groups for a specified window
# Used to stabilize league statistics across years

# Calculate rolling sum of columns within groups (default: 3-year window)
# Groups by interaction of group_cols (default: lvl, lg)
# Window: sums current year + prior N-1 years
rolling_prior_years <- function(df, value_cols, group_cols = c("lvl", "lg"), window = ROLLING_WINDOW_YEARS) {
  df <- df[order(df$lvl, df$lg, df$year), ]
  out <- df

  for (col in value_cols) {
    out[[col]] <- NA_real_
  }

  groups <- split(seq_len(nrow(df)), interaction(df[group_cols], drop = TRUE, lex.order = TRUE))

  for (idx in groups) {
    for (pos in seq_along(idx)) {
      current_year <- df$year[idx[pos]]
      prior_idx <- idx[df$year[idx] >= current_year - window + 1 & df$year[idx] <= current_year]

      for (col in value_cols) {
        out[[col]][idx[pos]] <- sum(df[[col]][prior_idx], na.rm = TRUE)
      }
    }
  }

  out
}

# Calculate rolling mean of columns within groups (default: 3-year window)
# Used to smooth standard deviations and other statistics over time
rolling_prior_means <- function(df, value_cols, group_cols = c("lvl", "lg"), window = ROLLING_WINDOW_YEARS) {
  df <- df[order(df$lvl, df$lg, df$year), ]
  out <- df

  groups <- split(seq_len(nrow(df)), interaction(df[group_cols], drop = TRUE, lex.order = TRUE))

  for (idx in groups) {
    for (pos in seq_along(idx)) {
      current_year <- df$year[idx[pos]]
      prior_idx <- idx[df$year[idx] >= current_year - window + 1 & df$year[idx] <= current_year]

      for (col in value_cols) {
        out[[col]][idx[pos]] <- mean(df[[col]][prior_idx], na.rm = TRUE)
      }
    }
  }

  out
}

# ==============================================================================
# FEATURE ENGINEERING: BATTING RATES AND METRICS
# ==============================================================================
# Calculate advanced batting statistics from raw counts
# Handles safe division and standardization across different denominators

# Compute standard batting rates and efficiency metrics
# 
# Output columns added to data frame:
#   h = Hits (1B + 2B + 3B + HR)
#   tb = Total bases (1B + 2*2B + 3*3B + 4*HR)
#   bb_pa = Walk rate (BB / PA)
#   k_pa = Strikeout rate (K / PA)
#   obp = On-base percentage ((H + BB + HBP) / PA)
#   slg = Slugging percentage (TB / AB)
#   ops = On-base plus slugging (OBP + SLG)
#   iso = Isolated power ((2B + 2*3B + 3*HR) / AB)
#   sbp = Stolen base percentage ((SB + 3) / (SB + CS + 7))  [speed component]
#   sba = Stolen base attempts ((SB + CS) / (1B + BB + HBP + 1))  [speed component]
#   sb_trip = Triples per opportunity (3B / (AB - HR - K))  [speed component]
#   sb_runs = Speed-related runs ((R - HR) / (1B + 2B + 3B + BB + HBP + 1))  [speed component]
add_hitter_rates <- function(df) {
  dplyr::mutate(
    df,
    h = x1b + x2b + x3b + hr,
    tb = x1b + 2 * x2b + 3 * x3b + 4 * hr,
    bb_pa = safe_divide(bb, pa),
    k_pa = safe_divide(so, pa),
    obp = safe_divide(h + bb + hbp, pa),
    slg = safe_divide(tb, ab),
    ops = obp + slg,
    iso = safe_divide(x2b + 2 * x3b + 3 * hr, ab),
    sbp = safe_divide(sb + 3, sb + cs + 7),
    sba = safe_divide(sb + cs, x1b + bb + hbp + 1),
    sb_trip = safe_divide(x3b, ab - hr - so),
    sb_runs = safe_divide(r - hr, x1b + x2b + x3b + bb + hbp + 1)
  )
}

# ==============================================================================
# LEAGUE CONTEXT: ROLLING AVERAGES AND STANDARD DEVIATIONS
# ==============================================================================
# Build reference statistics for z-score standardization and regression
# Uses 3-year rolling windows to stabilize estimates across seasons/levels

# Compute rolling 3-year league averages and standard deviations by year/level/league
# 
# Creates one row per (year, lvl, lg) combination with:
#   - lg_avg_* columns: Rolling average of each batting rate
#   - sd_* columns: Rolling standard deviation for each metric
# These normalization values enable:
#   - Z-score calculation: (value - league_avg) / league_sd
#   - Bayesian regression shrinkage toward league mean
#   - Fair comparison across eras/levels with different difficulty
build_league_context <- function(raw_df) {
  rate_cols <- c("bb_pa", "k_pa", "obp", "slg", "ops", "iso", "sbp", "sba", "sb_trip", "sb_runs")

  annual_sums <- raw_df |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      pa_wtd_age_num = sum(age * pa, na.rm = TRUE),
      age_pa = sum(pa[is.finite(age)], na.rm = TRUE),
      dplyr::across(pa:r, ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  rolling_sums <- rolling_prior_years(
    annual_sums,
    value_cols = c("pa_wtd_age_num", "age_pa", "pa", "ab", "x1b", "x2b", "x3b", "hr", "bb", "so", "hbp", "sb", "cs", "r")
  )

  league_averages <- rolling_sums |>
    dplyr::mutate(pa_wtd_age = safe_divide(pa_wtd_age_num, age_pa)) |>
    add_hitter_rates() |>
    dplyr::select(year, lvl, lg, pa_wtd_age, dplyr::all_of(rate_cols)) |>
    dplyr::rename_with(~paste0("lg_avg_", .x), dplyr::all_of(rate_cols))

  annual_sd <- raw_df |>
    dplyr::filter(is.finite(sb_runs)) |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(rate_cols), ~stats::sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      .groups = "drop"
    )

  league_sd <- rolling_prior_means(
    annual_sd,
    value_cols = paste0("sd_", rate_cols)
  )

  dplyr::left_join(league_averages, league_sd, by = c("year", "lvl", "lg"))
}

# ==============================================================================
# Z-SCORE CALCULATION: BAYESIAN REGRESSION AND STANDARDIZATION
# ==============================================================================
# Apply Bayesian regression shrinkage to rates and calculate z-scores
# Reduces noise from small sample sizes and enables era/level-neutral comparison

# Apply Bayesian regression to rate statistics, then standardize as z-scores
#
# Regression shrinkage coefficients (pseudo plate-appearances/at-bats added):
#   Walk/Strikeout rates: +65 PA  (typical ~1/8 season)
#   ISO: +85 AB                    (typical ~1/4 season)
#   OBP: +197 PA                   (typical ~2/5 season)
#   SLG: +137 AB                   (typical ~2/5 season)
# Higher coefficient = more shrinkage toward league mean (stronger regulation)
#
# Speed metric adjustment:
#   1. Calculate raw z-scores for speed components
#   2. Center within each (year, level, league) group to remove era effects
#   3. Combine into composite speed score: 50 + 4.25 * (sum of 4 z-scores)
#      This creates a 50-based scale similar to scouting grades (40-60 typical)
add_regressed_zscores <- function(raw_df, league_context) {
  df <- dplyr::left_join(raw_df, league_context, by = c("year", "lvl", "lg"))

  # Apply Bayesian regression using league averages to shrink toward population mean
  df <- dplyr::mutate(
    df,
    reg_bb_pa = safe_divide(bb + lg_avg_bb_pa * REGRESSION_SHRINKAGE$bb_pa, pa + REGRESSION_SHRINKAGE$bb_pa),
    reg_k_pa = safe_divide(so + lg_avg_k_pa * REGRESSION_SHRINKAGE$k_pa, pa + REGRESSION_SHRINKAGE$k_pa),
    reg_iso = safe_divide(iso * ab + lg_avg_iso * REGRESSION_SHRINKAGE$iso, ab + REGRESSION_SHRINKAGE$iso),
    reg_obp = safe_divide(obp * pa + lg_avg_obp * REGRESSION_SHRINKAGE$obp, pa + REGRESSION_SHRINKAGE$obp),
    reg_slg = safe_divide(slg * ab + lg_avg_slg * REGRESSION_SHRINKAGE$slg, ab + REGRESSION_SHRINKAGE$slg),
    reg_ops = reg_obp + reg_slg,
    # Standardize regressed rates as z-scores using league standard deviations
    zbb = safe_divide(reg_bb_pa - lg_avg_bb_pa, sd_bb_pa),
    zk = safe_divide(reg_k_pa - lg_avg_k_pa, sd_k_pa),
    zobp = safe_divide(reg_obp - lg_avg_obp, sd_obp),
    zslg = safe_divide(reg_slg - lg_avg_slg, sd_slg),
    zops = safe_divide(reg_ops - lg_avg_ops, sd_ops),
    ziso = safe_divide(reg_iso - lg_avg_iso, sd_iso),
    # Raw speed z-scores (will be centered next)
    zsbp_raw = safe_divide(sbp - lg_avg_sbp, sd_sbp),
    zsba_raw = safe_divide(sba - lg_avg_sba, sd_sba),
    zsb_trip_raw = safe_divide(sb_trip - lg_avg_sb_trip, sd_sb_trip),
    zsb_runs_raw = safe_divide(sb_runs - lg_avg_sb_runs, sd_sb_runs)
  )

  # Center speed metrics around zero within each (year, level, league) group
  # This removes era-specific patterns in base-running
  speed_means <- df |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      zsbp_mean = mean(zsbp_raw, na.rm = TRUE),
      zsba_mean = mean(zsba_raw, na.rm = TRUE),
      zsb_trip_mean = mean(zsb_trip_raw, na.rm = TRUE),
      zsb_runs_mean = mean(zsb_runs_raw, na.rm = TRUE),
      .groups = "drop"
    )

  # Apply centering and combine into composite speed score
  df |>
    dplyr::left_join(speed_means, by = c("year", "lvl", "lg")) |>
    dplyr::mutate(
      zsbp = zsbp_raw - zsbp_mean,
      zsba = zsba_raw - zsba_mean,
      zsb_trip = zsb_trip_raw - zsb_trip_mean,
      zsb_runs = zsb_runs_raw - zsb_runs_mean,
      # Composite speed: scout scale from centered component z-scores
      speed = SPEED_BASE + SPEED_COEFFICIENT * (zsbp + zsba + zsb_trip + zsb_runs)
    ) |>
    dplyr::select(-dplyr::ends_with("_raw"), -dplyr::ends_with("_mean"))
}

# ==============================================================================
# PLAYER AGGREGATION: LEVELS AND PROFILES
# ==============================================================================
# Aggregate season-by-season data into player-level and career profiles

# Aggregate all seasons for a player at a specific level into one player-level row
# Groups by: batter + lvl
# Output: One row per player per level (e.g., same player has separate AAA and AA rows)
build_player_levels <- function(player_seasons) {
  player_seasons |>
    dplyr::group_by(batter, lvl) |>
    dplyr::summarise(
      player_level_id = paste(dplyr::first(batter), dplyr::first(lvl), sep = "::"),
      name = dplyr::first(name),
      pa = sum(pa, na.rm = TRUE),
      ab = sum(ab, na.rm = TRUE),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      min_age = suppressWarnings(min(age, na.rm = TRUE)),
      max_age = suppressWarnings(max(age, na.rm = TRUE)),
      avg_age = weighted_mean_safe(age, pa),
      team = collapse_unique(team),
      franchise = collapse_unique(franchise),
      pos = collapse_unique(pos),
      mlb_pa = max(mlb_pa, na.rm = TRUE),
      vorp = sum(vorp, na.rm = TRUE),
      w_zbb = weighted_mean_safe(zbb, pa),
      w_zk = weighted_mean_safe(zk, pa),
      w_zobp = weighted_mean_safe(zobp, pa),
      w_zslg = weighted_mean_safe(zslg, ab),
      w_zops = weighted_mean_safe(zops, pa),
      w_ziso = weighted_mean_safe(ziso, ab),
      w_zsbp = weighted_mean_safe(zsbp, sb + cs + 7),
      w_zsba = weighted_mean_safe(zsba, x1b + bb + hbp + 1),
      w_zsb_trip = weighted_mean_safe(zsb_trip, pmax(ab - hr - so, 0)),
      w_zsb_runs = weighted_mean_safe(zsb_runs, x1b + x2b + x3b + bb + hbp + 1),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      min_age = ifelse(is.infinite(min_age), NA_real_, min_age),
      max_age = ifelse(is.infinite(max_age), NA_real_, max_age),
      mlb_pa = ifelse(is.infinite(mlb_pa), NA_real_, mlb_pa),
      w_speed = 50 + 4.25 * (w_zsbp + w_zsba + w_zsb_trip + w_zsb_runs)
    )
    
}
    
    
# Aggregate all MILB seasons for a player into one career-wide profile row
# Filters to MILB only (lvl != "MLB") and applies recency weighting
# Recency weights: Current = 1.0x, Prior = 0.75x, 2-Prior = 0.55x, Older = 0.35x
# This emphasizes recent performance while still accounting for full career progression
#
# Output: One row per player (career-wide MILB profile)
# Includes latest season/level tracking for context in similarity results

build_player_profiles <- function(player_seasons) {
  milb_seasons <- player_seasons |>
    dplyr::filter(lvl != "MLB") |>
    dplyr::mutate(
      season_age_weight = pa * recency_weight,
      level_numeric = level_value(lvl),
      level_weight = pa * recency_weight
    )

  milb_seasons |>
    dplyr::group_by(batter) |>
    dplyr::group_modify(~{
      latest_year <- max(.x$year, na.rm = TRUE)
      latest_rows <- .x[.x$year == latest_year, , drop = FALSE]
      latest_level_numeric <- max(latest_rows$level_numeric, na.rm = TRUE)
      latest_level <- latest_rows$lvl[which.max(latest_rows$level_numeric)]

      data.frame(
        name = dplyr::first(.x$name),
        career_pa = sum(.x$pa, na.rm = TRUE),
        career_ab = sum(.x$ab, na.rm = TRUE),
        min_year = min(.x$year, na.rm = TRUE),
        max_year = max(.x$year, na.rm = TRUE),
        min_age = suppressWarnings(min(.x$age, na.rm = TRUE)),
        max_age = suppressWarnings(max(.x$age, na.rm = TRUE)),
        avg_age = weighted_mean_by_col(.x, "age", "season_age_weight"),
        latest_year = latest_year,
        latest_age = weighted_mean_safe(latest_rows$age, latest_rows$pa),
        latest_level = latest_level,
        latest_level_numeric = latest_level_numeric,
        avg_level_numeric = weighted_mean_by_col(.x, "level_numeric", "level_weight"),
        levels = collapse_unique(.x$lvl),
        team = collapse_unique(.x$team),
        franchise = collapse_unique(.x$franchise),
        pos = collapse_unique(.x$pos),
        mlb_pa = max(.x$mlb_pa, na.rm = TRUE),
        vorp = sum(.x$vorp, na.rm = TRUE),
        w_zbb = weighted_z_by_season(.x, "zbb", "pa", "recency_weight"),
        w_zk = weighted_z_by_season(.x, "zk", "pa", "recency_weight"),
        w_zobp = weighted_z_by_season(.x, "zobp", "pa", "recency_weight"),
        w_zslg = weighted_z_by_season(.x, "zslg", "ab", "recency_weight"),
        w_zops = weighted_z_by_season(.x, "zops", "pa", "recency_weight"),
        w_ziso = weighted_z_by_season(.x, "ziso", "ab", "recency_weight"),
        w_zsbp = weighted_z_by_season(.x, "zsbp", "sbp_denom", "recency_weight"),
        w_zsba = weighted_z_by_season(.x, "zsba", "sba_denom", "recency_weight"),
        w_zsb_trip = weighted_z_by_season(.x, "zsb_trip", "sb_trip_denom", "recency_weight"),
        w_zsb_runs = weighted_z_by_season(.x, "zsb_runs", "sb_runs_denom", "recency_weight")
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      min_age = ifelse(is.infinite(min_age), NA_real_, min_age),
      max_age = ifelse(is.infinite(max_age), NA_real_, max_age),
      latest_level_numeric = ifelse(is.infinite(latest_level_numeric), NA_real_, latest_level_numeric),
      mlb_pa = ifelse(is.infinite(mlb_pa), NA_real_, mlb_pa),
      w_speed = SPEED_BASE + SPEED_COEFFICIENT * (w_zsbp + w_zsba + w_zsb_trip + w_zsb_runs)
    )
}
    
# ==============================================================================
# SIMILARITY SCORING
# ==============================================================================
# Find similar players based on weighted z-score deltas in 4-feature space
# Scoring formula: 1000 - 125 * weighted_delta (0-1000 scale, higher = more similar)

# Calculate within-level similarity: find top-N most similar players at same level
#
# Features compared (with weights in distance calculation):
#   w_zbb: Walk rate z-score (weight: 1.4)
#   w_zk: Strikeout rate z-score (weight: 1.2)
#   w_ziso: Isolated power z-score (weight: 1.5)
#   w_speed: Composite speed score (weight: 0.8, scaled /10 for distance)
#
# Distance metric: Weighted Euclidean delta = sum(|feature_delta| * weight) / sum(weights)
# Only includes players with min_pa >= threshold and finite feature values
score_one_level <- function(level_df, top_n = 20, min_pa = 100) {
  candidates <- level_df |>
    dplyr::filter(
      pa >= min_pa,
      is.finite(w_zbb),
      is.finite(w_zk),
      is.finite(w_ziso),
      is.finite(w_speed)
    )

  if (nrow(candidates) < 2) {
    return(data.frame())
  }

  features <- as.matrix(candidates[, c("w_zbb", "w_zk", "w_ziso", "w_speed")])
  weights <- SIMILARITY_WEIGHTS
  names(weights) <- colnames(features)
  features[, "w_speed"] <- features[, "w_speed"] / SPEED_DISTANCE_SCALE

  out <- vector("list", nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    deltas <- abs(t(t(features) - features[i, ]))
    weighted_delta <- as.numeric(deltas %*% weights) / sum(weights)
    total_delta <- weighted_delta
    total_delta[i] <- Inf

    keep <- order(total_delta)[seq_len(min(top_n, nrow(candidates) - 1))]

    out[[i]] <- data.frame(
      player_level_id = candidates$player_level_id[i],
      batter = candidates$batter[i],
      name = candidates$name[i],
      lvl = candidates$lvl[i],
      comp_player_level_id = candidates$player_level_id[keep],
      comp_batter = candidates$batter[keep],
      comp_name = candidates$name[keep],
      comp_lvl = candidates$lvl[keep],
      rank = seq_along(keep),
      similarity_score = round(pmax(0, SCORING_BASE - SCORING_DISTANCE_MULTIPLIER * total_delta[keep]), 1),
      weighted_delta = round(weighted_delta[keep], 4),
      pa = candidates$pa[i],
      comp_pa = candidates$pa[keep],
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out)
}

# Calculate career-wide similarity: find top-N most similar players across all MILB levels
# Same algorithm as score_one_level() but:
#   - Compares across all MILB levels (not restricted to same level)
#   - Uses career_pa threshold instead of per-level PA
#   - Returns additional context columns (levels, latest_level, individual z-scores)
score_player_profiles <- function(player_profiles, top_n = 20, min_pa = 100) {
  candidates <- player_profiles |>
    dplyr::filter(
      career_pa >= min_pa,
      is.finite(w_zbb),
      is.finite(w_zk),
      is.finite(w_ziso),
      is.finite(w_speed)
    )

  if (nrow(candidates) < 2) {
    return(data.frame())
  }

  features <- as.matrix(candidates[, c("w_zbb", "w_zk", "w_ziso", "w_speed")])
  weights <- SIMILARITY_WEIGHTS
  names(weights) <- colnames(features)
  features[, "w_speed"] <- features[, "w_speed"] / SPEED_DISTANCE_SCALE

  out <- vector("list", nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    deltas <- abs(t(t(features) - features[i, ]))
    weighted_delta <- as.numeric(deltas %*% weights) / sum(weights)
    total_delta <- weighted_delta
    total_delta[i] <- Inf

    keep <- order(total_delta)[seq_len(min(top_n, nrow(candidates) - 1))]

    out[[i]] <- data.frame(
      batter = candidates$batter[i],
      name = candidates$name[i],
      comp_batter = candidates$batter[keep],
      comp_name = candidates$name[keep],
      rank = seq_along(keep),
      similarity_score = round(pmax(0, SCORING_BASE - SCORING_DISTANCE_MULTIPLIER * total_delta[keep]), 1),
      weighted_delta = round(weighted_delta[keep], 4),
      career_pa = candidates$career_pa[i],
      comp_career_pa = candidates$career_pa[keep],
      selected_levels = candidates$levels[i],
      comp_levels = candidates$levels[keep],
      selected_latest_level = candidates$latest_level[i],
      comp_latest_level = candidates$latest_level[keep],
      selected_zbb = round(candidates$w_zbb[i], 3),
      comp_zbb = round(candidates$w_zbb[keep], 3),
      selected_zk = round(candidates$w_zk[i], 3),
      comp_zk = round(candidates$w_zk[keep], 3),
      selected_ziso = round(candidates$w_ziso[i], 3),
      comp_ziso = round(candidates$w_ziso[keep], 3),
      selected_speed = round(candidates$w_speed[i], 1),
      comp_speed = round(candidates$w_speed[keep], 1),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out)
}

# Orchestrate within-level similarity scoring: split by level and score each separately
# Combines results from score_one_level() for all levels into single output table
build_similarity_scores <- function(player_levels, top_n = 20, min_pa = 100) {
  split(player_levels, player_levels$lvl) |>
    lapply(score_one_level, top_n = top_n, min_pa = min_pa) |>
    dplyr::bind_rows()
}

# ==============================================================================
# DATABASE I/O: WRITE AND COPY FUNCTIONS
# ==============================================================================
# Export results to SQLite with indexes for efficient querying

# Create SQLite database with all analysis tables and efficient lookup indexes
# Input: db_path (destination file path), tables (named list of data frames)
write_database <- function(db_path, tables) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(db_path)) {
    unlink(db_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Write all tables to database
  for (table_name in names(tables)) {
    DBI::dbWriteTable(con, table_name, as.data.frame(tables[[table_name]]), overwrite = TRUE)
  }

  # Create indexes for common lookup patterns
  # Speeds up both within-database queries and external API calls
  DBI::dbExecute(con, "CREATE INDEX idx_player_levels_player_level_id ON player_levels(player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_levels_name ON player_levels(name)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_profiles_batter ON player_profiles(batter)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_profiles_name ON player_profiles(name)")
  DBI::dbExecute(con, "CREATE INDEX idx_similarity_player_level_id ON hitter_similarity(player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_similarity_comp_player_level_id ON hitter_similarity(comp_player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_career_similarity_batter ON hitter_career_similarity(batter, rank)")
  DBI::dbExecute(con, "CREATE INDEX idx_career_similarity_comp_batter ON hitter_career_similarity(comp_batter)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_seasons_batter_level ON player_seasons(batter, lvl)")

  invisible(db_path)
}

# Copy database file from one location to another (e.g., private to public/deployable location)
copy_database <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  file.copy(source, destination, overwrite = TRUE)
}

# ==============================================================================
# MAIN ORCHESTRATION FUNCTION
# ==============================================================================
# Execute complete pipeline: Load -> Engineer -> Aggregate -> Score -> Export
#
# Usage:
#   result <- build_hitter_similarity_database()
#   print(result$tables)  # View row counts for each output table
#
# Output databases:
#   - private_db: Full database with all 8 tables (~110 MB)
#   - public_db: Copy of private_db for distribution
#
# Call build_site_database.R separately to create smaller browser-friendly version

# Main entry point for complete database build pipeline
build_hitter_similarity_database <- function(
  input_feather = file.path("data", "raw", "milb_raw_selected_data.feather"),
  private_db = file.path("data", "db", "prospect_comps.sqlite"),
  public_db = file.path("output", "db", "prospect_comps.sqlite"),
  top_n = 20,
  min_pa = 100
) {
  # ============================================================================
  # Step 1: Load raw data and calculate initial batting rates
  # ============================================================================
  raw_df <- arrow::read_feather(
    input_feather,
    col_select = c(
      batter, name, year, lvl, lg, team, franchise, pos, age, pa, ab,
      x1b, x2b, x3b, hr, bb, so, hbp, sb, cs, r, vorp, mlb_pa
    )
  ) |>
    dplyr::mutate(
      batter = as.character(batter),
      year = as.integer(year),
      age = as.numeric(age),
      level_numeric = level_value(lvl)
    ) |>
    add_hitter_rates()

  # ============================================================================
  # Step 2: Build league context (rolling 3-year averages and standard deviations)
  # ============================================================================
  league_context <- build_league_context(raw_df)
  player_seasons <- add_regressed_zscores(raw_df, league_context) |>
    dplyr::group_by(batter) |>
    dplyr::mutate(
      seasons_since_latest = max(year, na.rm = TRUE) - year,
      recency_weight = dplyr::case_when(
        seasons_since_latest <= 0 ~ RECENCY_WEIGHTS["current"],
        seasons_since_latest == 1 ~ RECENCY_WEIGHTS["prior_1"],
        seasons_since_latest == 2 ~ RECENCY_WEIGHTS["prior_2"],
        TRUE ~ RECENCY_WEIGHTS["older"]
      ),
      sbp_denom = sb + cs + 7,
      sba_denom = x1b + bb + hbp + 1,
      sb_trip_denom = pmax(ab - hr - so, 0),
      sb_runs_denom = x1b + x2b + x3b + bb + hbp + 1
    ) |>
    dplyr::ungroup()
  player_levels <- build_player_levels(player_seasons)
  player_profiles <- build_player_profiles(player_seasons)
  hitter_similarity <- build_similarity_scores(player_levels, top_n = top_n, min_pa = min_pa)
  hitter_career_similarity <- score_player_profiles(player_profiles, top_n = top_n, min_pa = min_pa)

  # ============================================================================
  # Step 3: Prepare data exports and build metadata
  # ============================================================================
  player_seasons_export <- player_seasons |>
    dplyr::select(
      batter, name, year, lvl, lg, team, franchise, pos, age, pa, ab,
      x1b, x2b, x3b, hr, bb, so, hbp, sb, cs, r, vorp, mlb_pa,
      bb_pa, k_pa, obp, slg, ops, iso, speed,
      zbb, zk, zobp, zslg, zops, ziso, recency_weight
    )

  # Build metadata table documenting build parameters and methodology
  # This is stored in database for version tracking and audit trail
  metadata <- data.frame(
    key = c(
      "build_time_utc",
      "input_feather",
      "top_n",
      "min_pa",
      "scoring_version",
      "scope",
      "career_profile_weighting"
    ),
    value = c(
      format(Sys.time(), tz = "UTC", usetz = TRUE),
      input_feather,
      as.character(top_n),
      as.character(min_pa),
      "hitter_career_to_date_weighted_delta_v2",
      "historical minor league hitters only; MLB rows retained in player_seasons but excluded from career profiles",
      paste0("PA/denominator weighted season z-scores with recency weights: ",
             "latest ", RECENCY_WEIGHTS["current"], ", ",
             "previous ", RECENCY_WEIGHTS["prior_1"], ", ",
             "two years prior ", RECENCY_WEIGHTS["prior_2"], ", ",
             "older ", RECENCY_WEIGHTS["older"])
    )
  )

  # Document the similarity scoring feature weights and transformations
  # This explains how distances are calculated and allows future adjustments
  scoring_features <- data.frame(
    feature = names(SIMILARITY_WEIGHTS),
    weight = unname(SIMILARITY_WEIGHTS),
    transform = c("absolute z-score delta", "absolute z-score delta", "absolute z-score delta", "absolute speed delta divided by 10"),
    stringsAsFactors = FALSE
  )

  # Assemble all output tables for database export
  # Order reflects pipeline flow: metadata -> context -> seasons -> aggregates -> scores
  tables <- list(
    metadata = metadata,
    scoring_features = scoring_features,
    league_context = league_context,
    player_seasons = player_seasons_export,
    player_levels = player_levels,
    player_profiles = player_profiles,
    hitter_similarity = hitter_similarity,
    hitter_career_similarity = hitter_career_similarity
  )

  # ============================================================================
  # Step 4: Write to database and create public copy
  # ============================================================================
  write_database(private_db, tables)
  copy_database(private_db, public_db)

  # Return summary of build: paths and table sizes for verification
  list(
    private_db = private_db,
    public_db = public_db,
    tables = vapply(tables, nrow, integer(1))
  )
}
