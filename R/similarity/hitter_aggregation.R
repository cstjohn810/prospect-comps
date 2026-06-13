# ==============================================================================
# PLAYER AGGREGATION: LEVELS AND PROFILES
# ==============================================================================
# Aggregate season-by-season data into player-level and career profiles.

# Aggregate all seasons for a player at a specific level into one row.
# Groups by: mlbid + lvl
# Output: one row per (player, level) — e.g. separate AAA and AA rows per player.
build_player_levels <- function(player_seasons) {
  player_seasons |>
    dplyr::filter(!is.na(lvl), lvl != "") |>
    dplyr::group_by(mlbid, lvl) |>
    dplyr::summarise(
      player_level_id = paste(dplyr::first(mlbid), dplyr::first(lvl), sep = "::"),
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
      w_speed = SPEED_BASE + SPEED_COEFFICIENT * (w_zsbp + w_zsba + w_zsb_trip + w_zsb_runs)
    )
}

# Aggregate all MILB seasons for a single player into one career-wide profile row.
#
# @param player_seasons_df  Data frame of seasons for ONE player (already filtered
#                           to MILB only and augmented with season_age_weight,
#                           level_numeric, level_weight columns).
# @return A single-row data frame of career aggregates.
#
# Extracted from the group_modify() closure in build_player_profiles() so it can
# be tested and reasoned about independently.
aggregate_player_profile <- function(player_seasons_df) {
  .x <- player_seasons_df  # alias to match original closure variable name

  latest_year <- max(.x$year, na.rm = TRUE)
  latest_rows <- .x[.x$year == latest_year, , drop = FALSE]

  # Guard against all-NA level_numeric (lvl value not in LEVEL_SCALE).
  # which.max returns integer(0) on all-NA, so fall back to first row's lvl.
  valid_level <- !is.na(latest_rows$level_numeric)
  if (any(valid_level)) {
    latest_level_numeric <- max(latest_rows$level_numeric, na.rm = TRUE)
    latest_level <- latest_rows$lvl[which.max(latest_rows$level_numeric)]
  } else {
    latest_level_numeric <- NA_real_
    latest_level <- dplyr::first(latest_rows$lvl)
    warning("[aggregate_player_profile] Unknown lvl value(s) for player: ",
            dplyr::first(.x$name), " — lvl: ",
            paste(unique(latest_rows$lvl), collapse = ", "),
            ". Add to LEVEL_SCALE in constants.R.", call. = FALSE)
  }

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
}

# Aggregate all MILB seasons for every player into one career-wide profile row.
#
# Filters to MILB only (lvl != "MLB") and applies recency weighting before
# delegating per-player aggregation to aggregate_player_profile().
#
# Recency weights (from RECENCY_WEIGHTS):
#   Current = 1.0x, Prior = 0.75x, 2-Prior = 0.55x, Older = 0.35x
#
# Output: one row per player (career-wide MILB profile), including latest
# season/level tracking for context in similarity results.
build_player_profiles <- function(player_seasons) {
  milb_seasons <- player_seasons |>
    dplyr::filter(lvl != "MLB") |>
    dplyr::mutate(
      season_age_weight = pa * recency_weight,
      level_numeric = level_value(lvl),
      level_weight = pa * recency_weight
    )

  milb_seasons |>
    dplyr::filter(!is.na(lvl), lvl != "") |>
    dplyr::group_by(mlbid) |>
    dplyr::group_modify(~aggregate_player_profile(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      min_age = ifelse(is.infinite(min_age), NA_real_, min_age),
      max_age = ifelse(is.infinite(max_age), NA_real_, max_age),
      latest_level_numeric = ifelse(is.infinite(latest_level_numeric), NA_real_, latest_level_numeric),
      w_speed = SPEED_BASE + SPEED_COEFFICIENT * (w_zsbp + w_zsba + w_zsb_trip + w_zsb_runs)
    )
}
