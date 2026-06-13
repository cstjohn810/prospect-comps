# ==============================================================================
# SIMILARITY SCORING
# ==============================================================================
# Find similar players based on weighted z-score deltas in 4-feature space.
#
# Scoring formula: 1000 - 125 * weighted_delta  (0-1000 scale, higher = more similar)
# Distance metric: weighted Euclidean delta = sum(|Δfeature| * weight) / sum(weights)
#
# Features compared (with weights from SIMILARITY_WEIGHTS):
#   w_zbb   Walk rate z-score             (weight 1.4)
#   w_zk    Strikeout rate z-score        (weight 1.2)
#   w_ziso  Isolated power z-score        (weight 1.5, highest)
#   w_speed Composite speed score         (weight 0.8, scaled /10 for distance)

# Compute pairwise weighted absolute deltas for a feature matrix.
#
# @param features  Numeric matrix, rows = players, cols = features. The
#                  w_speed column must already be divided by SPEED_DISTANCE_SCALE
#                  before this call.
# @param weights   Named numeric vector, length == ncol(features). Names must
#                  match colnames(features).
# @return Numeric matrix of shape [nrow(features), nrow(features)] where
#         entry [i, j] is the weighted mean absolute delta between player i and j.
calculate_weighted_deltas <- function(features, weights) {
  n <- nrow(features)
  weight_vec <- weights[colnames(features)]
  weight_sum <- sum(weight_vec)

  # For each player i: abs(features - features[i, ]) %*% weight_vec / weight_sum
  # Vectorised across all i simultaneously via broadcasting.
  # Result[i, j] = weighted delta from player i to player j.
  vapply(seq_len(n), function(i) {
    deltas <- abs(t(t(features) - features[i, ]))
    as.numeric(deltas %*% weight_vec) / weight_sum
  }, numeric(n))
}

# Prepare the feature matrix shared by both scoring functions.
# Scales w_speed by SPEED_DISTANCE_SCALE and names columns to match SIMILARITY_WEIGHTS.
.build_feature_matrix <- function(candidates) {
  features <- as.matrix(candidates[, c("w_zbb", "w_zk", "w_ziso", "w_speed")])
  features[, "w_speed"] <- features[, "w_speed"] / SPEED_DISTANCE_SCALE
  features
}

# Identify the top-N comparables for player i and assemble the output row-list entry.
# Shared by score_one_level() and score_player_profiles(); extra_cols is a
# function(candidates, i, keep) -> named list of additional columns to include.
.top_n_comps <- function(weighted_deltas_col, i, top_n, candidates_n) {
  total_delta <- weighted_deltas_col
  total_delta[i] <- Inf
  order(total_delta)[seq_len(min(top_n, candidates_n - 1))]
}

# ==============================================================================
# WITHIN-LEVEL SCORING
# ==============================================================================

# Calculate within-level similarity: find top-N most similar players at the
# same level.
#
# @param level_df   Data frame of player-level aggregates for ONE level.
# @param top_n      Number of comparables to retain per player.
# @param min_pa     Minimum PA threshold; players below are excluded as both
#                   subjects and candidates.
# @return Data frame of similarity pairs, or an empty data frame if fewer
#         than 2 eligible candidates exist.
score_one_level <- function(level_df, top_n = TOP_N_COMPS, min_pa = MIN_PA_SIMILARITY) {
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

  features <- .build_feature_matrix(candidates)
  delta_matrix <- calculate_weighted_deltas(features, SIMILARITY_WEIGHTS)

  out <- vector("list", nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    keep <- .top_n_comps(delta_matrix[, i], i, top_n, nrow(candidates))

    out[[i]] <- data.frame(
      player_level_id = candidates$player_level_id[i],
      mlbid = candidates$mlbid[i],
      name = candidates$name[i],
      lvl = candidates$lvl[i],
      comp_player_level_id = candidates$player_level_id[keep],
      comp_mlbid = candidates$mlbid[keep],
      comp_name = candidates$name[keep],
      comp_lvl = candidates$lvl[keep],
      rank = seq_along(keep),
      similarity_score = round(
        pmax(0, SCORING_BASE - SCORING_DISTANCE_MULTIPLIER * delta_matrix[keep, i]), 1
      ),
      weighted_delta = round(delta_matrix[keep, i], 4),
      pa = candidates$pa[i],
      comp_pa = candidates$pa[keep],
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out)
}

# Orchestrate within-level similarity scoring: split by level and score each
# group separately, then combine.
build_similarity_scores <- function(player_levels, top_n = TOP_N_COMPS, min_pa = MIN_PA_SIMILARITY) {
  split(player_levels, player_levels$lvl) |>
    lapply(score_one_level, top_n = top_n, min_pa = min_pa) |>
    dplyr::bind_rows()
}

# ==============================================================================
# CAREER-WIDE (CROSS-LEVEL) SCORING
# ==============================================================================

# Calculate career-wide similarity: find top-N most similar players across all
# MILB levels.
#
# Same algorithm as score_one_level() but:
#   - Compares across all MILB levels (not restricted to same level).
#   - Uses career_pa threshold instead of per-level PA.
#   - Returns additional context columns (levels, latest_level, individual z-scores).
#
# @param player_profiles  Output of build_player_profiles().
# @param top_n            Number of comparables to retain per player.
# @param min_pa           Minimum career PA threshold.
score_player_profiles <- function(player_profiles, top_n = TOP_N_COMPS, min_pa = MIN_PA_SIMILARITY) {
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

  features <- .build_feature_matrix(candidates)
  delta_matrix <- calculate_weighted_deltas(features, SIMILARITY_WEIGHTS)

  out <- vector("list", nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    keep <- .top_n_comps(delta_matrix[, i], i, top_n, nrow(candidates))

    out[[i]] <- data.frame(
      mlbid = candidates$mlbid[i],
      name = candidates$name[i],
      comp_mlbid = candidates$mlbid[keep],
      comp_name = candidates$name[keep],
      rank = seq_along(keep),
      similarity_score = round(
        pmax(0, SCORING_BASE - SCORING_DISTANCE_MULTIPLIER * delta_matrix[keep, i]), 1
      ),
      weighted_delta = round(delta_matrix[keep, i], 4),
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
