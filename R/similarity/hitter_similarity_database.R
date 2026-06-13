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
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the database build.",
    call. = FALSE
  )
}

source("R/utils/constants.R")
source("R/utils/math_utils.R")
source("R/utils/database.R")
source("R/similarity/hitter_aggregation.R")
source("R/similarity/hitter_scoring.R")

# ==============================================================================
# STRING HELPERS
# ==============================================================================

# Collapse unique non-NA, non-empty values into a comma-separated string.
# Used to summarise team / franchise / position lists in aggregated profiles.
collapse_unique <- function(x) {
  values <- unique(x[!is.na(x) & x != ""])
  paste(values, collapse = ", ")
}

# Convert baseball level abbreviations to numeric scale for sorting.
# Uses LEVEL_SCALE defined in constants.R.
level_value <- function(level) {
  unname(LEVEL_SCALE[as.character(level)])
}

# ==============================================================================
# PHASE 1: FEATURE ENGINEERING — BATTING RATES AND METRICS
# ==============================================================================
# Calculate advanced batting statistics from raw counts.
# Handles safe division and standardization across different denominators.
#
# Output columns added:
#   h         Hits (1B + 2B + 3B + HR)
#   tb        Total bases (1B + 2*2B + 3*3B + 4*HR)
#   bb_pa     Walk rate (BB / PA)
#   k_pa      Strikeout rate (K / PA)
#   obp       On-base percentage ((H + BB + HBP) / PA)
#   slg       Slugging percentage (TB / AB)
#   ops       On-base plus slugging (OBP + SLG)
#   iso       Isolated power ((2B + 2*3B + 3*HR) / AB)
#   sbp       Stolen base percentage ((SB+3)/(SB+CS+7))        [speed component]
#   sba       Stolen base attempts per opportunity               [speed component]
#   sb_trip   Triples per opportunity                            [speed component]
#   sb_runs   Speed-related runs per opportunity                 [speed component]
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
# PHASE 2: LEAGUE CONTEXT — ROLLING AVERAGES AND STANDARD DEVIATIONS
# ==============================================================================
# Build reference statistics for z-score standardization and Bayesian regression.
# Uses 3-year rolling windows (ROLLING_WINDOW_YEARS) to stabilize estimates.
#
# Output: one row per (year, lvl, lg) with:
#   lg_avg_*  Rolling average of each batting rate
#   sd_*      Rolling standard deviation for each metric
build_league_context <- function(raw_df) {
  # Diagnostic: confirm all expected count columns are numeric before summarise.
  # If this prints character columns, the feather schema has unexpected types.
  count_col_types <- vapply(raw_df[RAW_COUNT_COLS], class, character(1))
  non_numeric <- count_col_types[!count_col_types %in% c("integer", "numeric")]
  if (length(non_numeric) > 0) {
    message(
      "[build_league_context] WARNING: expected numeric count columns are not numeric:\n",
      paste(names(non_numeric), non_numeric, sep = " = ", collapse = "\n")
    )
  } else {
    message("[build_league_context] OK: all count columns are numeric (", nrow(raw_df), " rows)")
  }

  annual_sums <- raw_df |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      pa_wtd_age_num = sum(age * pa, na.rm = TRUE),
      age_pa = sum(pa[is.finite(age)], na.rm = TRUE),
      dplyr::across(dplyr::all_of(RAW_COUNT_COLS), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  rolling_sums <- rolling_prior_years(
    annual_sums,
    value_cols = c("pa_wtd_age_num", "age_pa", RAW_COUNT_COLS)
  )

  league_averages <- rolling_sums |>
    dplyr::mutate(pa_wtd_age = safe_divide(pa_wtd_age_num, age_pa)) |>
    add_hitter_rates() |>
    dplyr::select(year, lvl, lg, pa_wtd_age, dplyr::all_of(RATE_COLS)) |>
    dplyr::rename_with(~paste0("lg_avg_", .x), dplyr::all_of(RATE_COLS))

  annual_sd <- raw_df |>
    dplyr::filter(is.finite(sb_runs)) |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(RATE_COLS), ~stats::sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      .groups = "drop"
    )

  league_sd <- rolling_prior_means(
    annual_sd,
    value_cols = paste0("sd_", RATE_COLS)
  )

  dplyr::left_join(league_averages, league_sd, by = c("year", "lvl", "lg"))
}

# ==============================================================================
# PHASE 3: Z-SCORE CALCULATION — BAYESIAN REGRESSION AND STANDARDIZATION
# ==============================================================================
# Two-step transformation:
#   apply_regression()      Shrinks raw rates toward league mean using pseudo-PA/AB
#   center_speed_zscores()  Centers speed components within era and combines them

# Step 3a: Apply Bayesian shrinkage to walk/K/ISO/OBP/SLG, then z-score all rates.
#
# Shrinkage coefficients (from REGRESSION_SHRINKAGE):
#   bb_pa / k_pa : +65 / +35 pseudo-PA
#   iso          : +85 pseudo-AB
#   obp          : +197 pseudo-PA
#   slg          : +137 pseudo-AB
# Higher coefficient = stronger regularization toward league mean.
apply_regression <- function(df) {
  dplyr::mutate(
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
    # Raw speed z-scores (will be centered in step 3b)
    zsbp_raw = safe_divide(sbp - lg_avg_sbp, sd_sbp),
    zsba_raw = safe_divide(sba - lg_avg_sba, sd_sba),
    zsb_trip_raw = safe_divide(sb_trip - lg_avg_sb_trip, sd_sb_trip),
    zsb_runs_raw = safe_divide(sb_runs - lg_avg_sb_runs, sd_sb_runs)
  )
}

# Step 3b: Center speed z-scores within each (year, level, league) group to
# remove era-specific base-running patterns, then combine into a composite
# scout-scale speed score.
#
# Formula: speed = SPEED_BASE + SPEED_COEFFICIENT * (zsbp + zsba + zsb_trip + zsb_runs)
# Typical output range: 40-60 (scout scale)
center_speed_zscores <- function(df) {
  speed_means <- df |>
    dplyr::group_by(year, lvl, lg) |>
    dplyr::summarise(
      zsbp_mean = mean(zsbp_raw, na.rm = TRUE),
      zsba_mean = mean(zsba_raw, na.rm = TRUE),
      zsb_trip_mean = mean(zsb_trip_raw, na.rm = TRUE),
      zsb_runs_mean = mean(zsb_runs_raw, na.rm = TRUE),
      .groups = "drop"
    )

  df |>
    dplyr::left_join(speed_means, by = c("year", "lvl", "lg")) |>
    dplyr::mutate(
      zsbp = zsbp_raw - zsbp_mean,
      zsba = zsba_raw - zsba_mean,
      zsb_trip = zsb_trip_raw - zsb_trip_mean,
      zsb_runs = zsb_runs_raw - zsb_runs_mean,
      speed = SPEED_BASE + SPEED_COEFFICIENT * (zsbp + zsba + zsb_trip + zsb_runs)
    ) |>
    dplyr::select(-dplyr::ends_with("_raw"), -dplyr::ends_with("_mean"))
}

# Combined entry point for phases 3a + 3b.
add_regressed_zscores <- function(raw_df, league_context) {
  raw_df |>
    dplyr::left_join(league_context, by = c("year", "lvl", "lg")) |>
    apply_regression() |>
    center_speed_zscores()
}

# ==============================================================================
# PHASE 4: RECENCY WEIGHTS AND SPEED DENOMINATORS
# ==============================================================================
# Attach per-season recency weights and pre-compute speed-related denominators
# used as weights during career-profile aggregation.
add_recency_and_denominators <- function(player_seasons) {
  player_seasons |>
    dplyr::group_by(mlbid) |>
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
}

# ==============================================================================
# PHASE 5: METADATA ASSEMBLY
# ==============================================================================
build_metadata <- function(input_feather, top_n, min_pa) {
  data.frame(
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
      SCORING_VERSION,
      "historical minor league hitters only; MLB rows retained in player_seasons but excluded from career profiles",
      paste0(
        "PA/denominator weighted season z-scores with recency weights: ",
        "latest ", RECENCY_WEIGHTS["current"], ", ",
        "previous ", RECENCY_WEIGHTS["prior_1"], ", ",
        "two years prior ", RECENCY_WEIGHTS["prior_2"], ", ",
        "older ", RECENCY_WEIGHTS["older"]
      )
    )
  )
}

build_scoring_features <- function() {
  data.frame(
    feature = names(SIMILARITY_WEIGHTS),
    weight = unname(SIMILARITY_WEIGHTS),
    transform = c(
      "absolute z-score delta",
      "absolute z-score delta",
      "absolute z-score delta",
      "absolute speed delta divided by 10"
    ),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# MAIN ORCHESTRATION
# ==============================================================================
# Execute the complete pipeline: Load -> Engineer -> Aggregate -> Score -> Export
#
# Usage:
#   result <- build_hitter_similarity_database()
#   print(result$tables)   # row counts for each output table
#
# Output databases:
#   private_db  Full database with all 8 tables (~110 MB)
#   public_db   Copy of private_db for distribution
#
# Call build_site_database.R separately to create the smaller browser-friendly version.
build_hitter_similarity_database <- function(
  input_feather = file.path("data", "raw", "tbc_minor_batting_history.feather"),
  private_db = file.path("data", "db", "private_prospect_comps.sqlite"),
  public_db = file.path("output", "db", "prospect_comps.sqlite"),
  top_n = TOP_N_COMPS,
  min_pa = MIN_PA_SIMILARITY
) {
  # Phase 1 & 2: Load raw data, compute rates, build league context
  raw_df <- arrow::read_feather(input_feather, col_select = dplyr::all_of(RAW_INPUT_COLS)) |>
    dplyr::mutate(
      year = as.integer(year),
      age = as.numeric(age),
      level_numeric = level_value(lvl)
    ) |>
    add_hitter_rates()

  league_context <- build_league_context(raw_df)

  # Phase 3 & 4: Z-scores, regression, recency weights
  player_seasons <- add_regressed_zscores(raw_df, league_context) |>
    add_recency_and_denominators()

  # Phase 5: Aggregate into profiles and score
  player_levels <- build_player_levels(player_seasons)
  player_profiles <- build_player_profiles(player_seasons)
  hitter_similarity <- build_similarity_scores(player_levels, top_n = top_n, min_pa = min_pa)
  hitter_career_similarity <- score_player_profiles(player_profiles, top_n = top_n, min_pa = min_pa)

  # Phase 6: Assemble exports
  player_seasons_export <- dplyr::select(player_seasons, dplyr::all_of(PLAYER_SEASONS_EXPORT_COLS))

  tables <- list(
    metadata = build_metadata(input_feather, top_n, min_pa),
    scoring_features = build_scoring_features(),
    league_context = league_context,
    player_seasons = player_seasons_export,
    player_levels = player_levels,
    player_profiles = player_profiles,
    hitter_similarity = hitter_similarity,
    hitter_career_similarity = hitter_career_similarity
  )

  # Phase 7: Write databases
  write_database(private_db, tables)
  copy_database(private_db, public_db)

  list(
    private_db = private_db,
    public_db = public_db,
    tables = vapply(tables, nrow, integer(1))
  )
}
