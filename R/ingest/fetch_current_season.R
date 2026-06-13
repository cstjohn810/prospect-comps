# ==============================================================================
# CURRENT SEASON DATA FETCHER
# ==============================================================================
# Pulls batting stats for a given season from the MLB Stats API via baseballr
# and returns a data frame shaped to match RAW_INPUT_COLS exactly, ready to
# bind_rows() onto the historical feather data before pipeline processing.
#
# Usage (called automatically by build_hitter_similarity_database() when
# append_current_season = TRUE):
#
#   df <- fetch_current_season(season = 2026)
#
# Column mapping (MLB API -> pipeline schema):
#   player_id          -> mlbid
#   player_full_name   -> name
#   season             -> year
#   sport_id (lookup)  -> lvl
#   league_name        -> lg        (normalized to short codes)
#   team_name          -> team
#   team_name          -> franchise (same source; no franchise mapping in API)
#   position_abbreviation -> pos
#   player_age / birth -> age       (calculated from season + birth year)
#   plate_appearances  -> pa
#   at_bats            -> ab
#   singles (derived)  -> x1b
#   doubles            -> x2b
#   triples            -> x3b
#   home_runs          -> hr
#   base_on_balls      -> bb
#   strike_outs        -> so
#   hit_by_pitch       -> hbp
#   stolen_bases       -> sb
#   caught_stealing    -> cs
#   runs               -> r
# ==============================================================================

source("R/utils/constants.R")

# ------------------------------------------------------------------------------
# LEVEL NORMALIZATION
# ------------------------------------------------------------------------------
# The MLB API returns a sport_id integer; map it back to the lvl abbreviations
# used in the historical feather.  Invert MLB_SPORT_IDS for the lookup.
SPORT_ID_TO_LVL <- setNames(names(MLB_SPORT_IDS), MLB_SPORT_IDS)

# The MLB API may return either verbose league names or abbreviations in the
# league_name field depending on the season and endpoint version. Both forms
# are mapped here. Extend if new codes appear in warning output during a fetch.
LEAGUE_NAME_TO_CODE <- c(
  # --- Verbose names ---
  # Triple-A
  "International League"        = "IL",
  "Pacific Coast League"        = "PCL",
  # Double-A
  "Eastern League"              = "EL",
  "Southern League"             = "SL",
  "Texas League"                = "TL",
  # High-A / A+
  "South Atlantic League"       = "SAL",
  "California League"           = "CAL",
  "Carolina League"             = "CL",
  "Midwest League"              = "MWL",
  "Northwest League"            = "NWL",
  "New York-Penn League"        = "NYPL",
  # Low-A
  "Florida State League"        = "FSL",
  # MLB
  "American League"             = "AL",
  "National League"             = "NL",
  # Post-2021 reorganised league names
  "Triple-A East"               = "IL",
  "Triple-A West"               = "PCL",
  "Double-A Central"            = "TL",
  "Double-A Northeast"          = "EL",
  "Double-A South"              = "SL",
  "High-A Central"              = "MWL",
  "High-A East"                 = "SAL",
  "High-A West"                 = "NWL",
  "Low-A East"                  = "SAL",
  "Low-A Southeast"             = "FSL",
  "Low-A West"                  = "NWL",
  # --- Abbreviations (API sometimes returns these in league_name field) ---
  "INT"                         = "IL",
  "PCL"                         = "PCL",
  "EAS"                         = "EL",
  "SOU"                         = "SL",
  "TEX"                         = "TL",
  "SAL"                         = "SAL",
  "CAL"                         = "CAL",
  "CAR"                         = "CL",
  "MID"                         = "MWL",
  "NWL"                         = "NWL",
  "NYP"                         = "NYPL",
  "FSL"                         = "FSL",
  "AL"                          = "AL",
  "NL"                          = "NL",
  # Rookie complex leagues
  "DSL"                         = "DSL",
  "FCL"                         = "Rk",
  "ACL"                         = "Rk",
  "AZL"                         = "Rk",
  "GCL"                         = "Rk"
)

# Normalize a vector of league name strings to short codes.
# Unknown names are kept as-is and a warning is emitted so the mapping above
# can be extended.
normalize_league <- function(league_names) {
  result <- LEAGUE_NAME_TO_CODE[league_names]
  unknown <- is.na(result)
  if (any(unknown)) {
    unique_unknown <- unique(league_names[unknown])
    warning(
      "[fetch_current_season] Unmapped league name(s) — add to LEAGUE_NAME_TO_CODE in fetch_current_season.R:\n  ",
      paste(unique_unknown, collapse = "\n  "),
      call. = FALSE
    )
    result[unknown] <- league_names[unknown]  # fall back to raw string
  }
  unname(result)
}

# ------------------------------------------------------------------------------
# SINGLE-LEVEL FETCH
# ------------------------------------------------------------------------------
# Fetch one sport_id for one season and return a raw API data frame, or NULL
# on failure.  Separated from normalization so failures per level are isolated.
fetch_one_level <- function(sport_id, lvl_label, season) {
  message("  Fetching ", lvl_label, " (sport_id ", sport_id, ") ...")
  result <- tryCatch(
    baseballr::mlb_stats(
      stat_type  = "season",
      stat_group = "hitting",
      season     = season,
      sport_id   = sport_id
    ),
    error = function(e) {
      warning("[fetch_current_season] Failed to fetch ", lvl_label, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(result) || nrow(result) == 0) {
    message("    -> no data returned for ", lvl_label)
    return(NULL)
  }
  message("    -> ", nrow(result), " rows")
  result
}

# ------------------------------------------------------------------------------
# COLUMN NORMALIZATION
# ------------------------------------------------------------------------------
# Takes a raw baseballr response data frame for one level and returns a data
# frame with exactly the columns in RAW_INPUT_COLS.
normalize_api_response <- function(df, lvl_label, season) {

  # Helper: extract a column if it exists, otherwise return a default vector
  col_or <- function(data, col, default) {
    if (col %in% names(data)) data[[col]] else default
  }

  n <- nrow(df)

  # Derived: singles are not returned directly by the API.
  # Coerce to integer first; any NA propagates through subtraction as NA (correct).
  hits    <- as.integer(col_or(df, "hits",         rep(NA_integer_, n)))
  doubles <- as.integer(col_or(df, "doubles",      rep(NA_integer_, n)))
  triples <- as.integer(col_or(df, "triples",      rep(NA_integer_, n)))
  hr      <- as.integer(col_or(df, "home_runs",    rep(NA_integer_, n)))
  x1b     <- hits - doubles - triples - hr
  x1b     <- pmax(x1b, 0L, na.rm = FALSE)  # guard against rounding errors; NA stays NA

  # Age: API returns "age" directly as confirmed by names(df).
  # Fall back to player_age, then birth_date derivation if the column ever changes.
  age <- if ("age" %in% names(df)) {
    as.numeric(df$age)
  } else if ("player_age" %in% names(df)) {
    as.numeric(df$player_age)
  } else if ("birth_date" %in% names(df)) {
    birth_years <- as.numeric(format(as.Date(df$birth_date), "%Y"))
    as.numeric(season) - birth_years
  } else {
    rep(NA_real_, n)
  }

  # League: prefer league_abbreviation (always a short code) over league_name
  # which the API sometimes populates with abbreviations and sometimes full names.
  league_raw <- if ("league_abbreviation" %in% names(df)) {
    df$league_abbreviation
  } else if ("league_name" %in% names(df)) {
    df$league_name
  } else {
    rep(NA_character_, n)
  }

  data.frame(
    mlbid     = as.integer(col_or(df, "player_id",              rep(NA_integer_, n))),
    name      = as.character(col_or(df, "player_full_name",     rep(NA_character_, n))),
    year      = as.integer(season),
    lvl       = lvl_label,
    lg        = normalize_league(as.character(league_raw)),
    team      = as.character(col_or(df, "team_name",            rep(NA_character_, n))),
    franchise = as.character(col_or(df, "team_name",            rep(NA_character_, n))),
    pos       = as.character(col_or(df, "position_abbreviation",rep(NA_character_, n))),
    age       = age,
    pa        = as.integer(col_or(df, "plate_appearances",      rep(NA_integer_, n))),
    ab        = as.integer(col_or(df, "at_bats",                rep(NA_integer_, n))),
    x1b       = as.integer(x1b),
    x2b       = as.integer(doubles),
    x3b       = as.integer(triples),
    hr        = as.integer(hr),
    bb        = as.integer(col_or(df, "base_on_balls",          rep(NA_integer_, n))),
    so        = as.integer(col_or(df, "strike_outs",            rep(NA_integer_, n))),
    hbp       = as.integer(col_or(df, "hit_by_pitch",           rep(NA_integer_, n))),
    sb        = as.integer(col_or(df, "stolen_bases",           rep(NA_integer_, n))),
    cs        = as.integer(col_or(df, "caught_stealing",        rep(NA_integer_, n))),
    r         = as.integer(col_or(df, "runs",                   rep(NA_integer_, n))),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# PUBLIC ENTRY POINT
# ------------------------------------------------------------------------------
#' Fetch a full season of batting stats from the MLB Stats API.
#'
#' @param season   Integer season year (default: current calendar year).
#' @param sport_ids Named integer vector of sport IDs to fetch. Defaults to
#'                  MLB_SPORT_IDS from constants.R, which covers MLB + all full-
#'                  season MiLB levels. Pass a subset to fetch only specific levels,
#'                  e.g. \code{MLB_SPORT_IDS[c("AA", "AAA")]} for Double-A and Triple-A.
#' @param min_pa   Drop rows with fewer than this many plate appearances. Set to
#'                 0 to keep everyone (pipeline's own MIN_PA_SIMILARITY filters
#'                 later, but dropping junk early speeds up processing).
#'
#' @return Data frame with columns matching RAW_INPUT_COLS, ready for
#'         \code{dplyr::bind_rows()} onto the historical feather data.
fetch_current_season <- function(
    season    = as.integer(format(Sys.Date(), "%Y")),
    sport_ids = MLB_SPORT_IDS,
    min_pa    = 10L
) {
  season <- as.integer(season)
  message("[fetch_current_season] Fetching ", season, " season across ",
          length(sport_ids), " level(s): ", paste(names(sport_ids), collapse = ", "))

  # Build raw_list with an explicit loop — mapply/Map drop names when used
  # with mixed scalar/data-frame arguments across iterations.
  raw_list <- vector("list", length(sport_ids))
  names(raw_list) <- names(sport_ids)
  for (i in seq_along(sport_ids)) {
    raw_list[[i]] <- fetch_one_level(
      sport_id  = unname(sport_ids[[i]]),
      lvl_label = names(sport_ids)[[i]],
      season    = season
    )
  }

  # Drop levels that returned nothing
  raw_list <- Filter(Negate(is.null), raw_list)

  if (length(raw_list) == 0) {
    stop("[fetch_current_season] No data returned for any level. Check your internet connection and the season year.", call. = FALSE)
  }

  # Normalize each level's response and combine.
  # Iterate by index so the full data frame is passed as a single object —
  # Map/mapply iterate over list elements which for data frames means columns.
  level_names <- names(raw_list)
  normalized <- vector("list", length(raw_list))
  names(normalized) <- level_names

  for (i in seq_along(raw_list)) {
    lvl_label <- level_names[[i]]
    normalized[[i]] <- tryCatch(
      normalize_api_response(raw_list[[i]], lvl_label, season),
      error = function(e) {
        warning("[fetch_current_season] normalize_api_response failed for ", lvl_label,
                ": ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  normalized <- Filter(Negate(is.null), normalized)

  if (length(normalized) == 0) {
    stop("[fetch_current_season] normalize_api_response failed for all levels.", call. = FALSE)
  }

  result <- dplyr::bind_rows(normalized)

  # Basic sanity checks
  missing_id <- sum(is.na(result$mlbid) | result$mlbid == 0)
  if (missing_id > 0) {
    warning("[fetch_current_season] ", missing_id, " rows have missing/zero mlbid and will be excluded.", call. = FALSE)
    result <- result[!is.na(result$mlbid) & result$mlbid != 0, ]
  }

  if (min_pa > 0) {
    before <- nrow(result)
    result <- result[!is.na(result$pa) & result$pa >= min_pa, ]
    message("[fetch_current_season] Dropped ", before - nrow(result),
            " rows below ", min_pa, " PA threshold")
  }

  # Confirm output columns exactly match RAW_INPUT_COLS
  missing_cols <- setdiff(RAW_INPUT_COLS, names(result))
  if (length(missing_cols) > 0) {
    stop("[fetch_current_season] Output is missing expected columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  result <- result[, RAW_INPUT_COLS]  # enforce column order

  message("[fetch_current_season] Done: ", nrow(result), " rows returned for ", season)
  result
}
