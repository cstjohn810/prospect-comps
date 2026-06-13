# ==============================================================================
# MATH UTILITIES
# ==============================================================================
# Small, reusable numeric helpers used throughout the pipeline.

# Safe division that handles zero and NA values (returns NA_real_ instead of Inf/NaN)
safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

# Calculate weighted mean ignoring NA and non-positive weight values.
# Returns NA_real_ if no valid observations.
weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Wrapper for weighted_mean_safe that references columns by name in a data frame
weighted_mean_by_col <- function(data, value_col, weight_col) {
  weighted_mean_safe(data[[value_col]], data[[weight_col]])
}

# Calculate weighted mean of z-scores with compounded weights (denominator * recency).
# Used in career profile aggregation where weights reflect both sample size and
# season recency.
weighted_z_by_season <- function(data, value_col, denominator_col, recency_col) {
  weighted_mean_safe(data[[value_col]], data[[denominator_col]] * data[[recency_col]])
}

# ==============================================================================
# ROLLING WINDOW AGGREGATION
# ==============================================================================
# Calculate rolling aggregates (sums/means) within groups for a specified window.
# Used to stabilize league statistics across years.
#
# Both functions share the same structure:
#   1. Order rows by group columns then year.
#   2. For each position within a group, collect the indices of rows whose year
#      falls within [current_year - window + 1, current_year].
#   3. Apply the aggregation (sum or mean) over those indices.
#
# group_cols is passed to interaction() with lex.order = TRUE so that group
# membership is determined by the combination of all grouping columns.

# Calculate rolling sum of columns within groups (default: 3-year window).
# Groups by interaction of group_cols (default: lvl, lg).
# Window sums current year + prior (window - 1) years.
rolling_prior_years <- function(
    df,
    value_cols,
    group_cols = c("lvl", "lg"),
    window = ROLLING_WINDOW_YEARS) {

  df <- df[order(df$lvl, df$lg, df$year), ]
  out <- df
  for (col in value_cols) out[[col]] <- NA_real_

  groups <- split(
    seq_len(nrow(df)),
    interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  )

  for (idx in groups) {
    for (pos in seq_along(idx)) {
      current_year <- df$year[idx[pos]]
      prior_idx <- idx[
        df$year[idx] >= current_year - window + 1 &
          df$year[idx] <= current_year
      ]
      for (col in value_cols) {
        out[[col]][idx[pos]] <- sum(df[[col]][prior_idx], na.rm = TRUE)
      }
    }
  }

  out
}

# Calculate rolling mean of columns within groups (default: 3-year window).
# Used to smooth standard deviations and other statistics over time.
rolling_prior_means <- function(
    df,
    value_cols,
    group_cols = c("lvl", "lg"),
    window = ROLLING_WINDOW_YEARS) {

  df <- df[order(df$lvl, df$lg, df$year), ]
  out <- df

  groups <- split(
    seq_len(nrow(df)),
    interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  )

  for (idx in groups) {
    for (pos in seq_along(idx)) {
      current_year <- df$year[idx[pos]]
      prior_idx <- idx[
        df$year[idx] >= current_year - window + 1 &
          df$year[idx] <= current_year
      ]
      for (col in value_cols) {
        out[[col]][idx[pos]] <- mean(df[[col]][prior_idx], na.rm = TRUE)
      }
    }
  }

  out
}
