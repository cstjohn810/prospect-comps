# ==============================================================================
# PIPELINE CONSTANTS
# ==============================================================================
# All tunable parameters and magic numbers organized for easy maintenance.
# Modify these to adjust scoring behavior across the entire pipeline.

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

# Baseball level numeric scale for sorting and filtering
# 1=Rookie (FRk, Rk, R, DSL, FCL, ACL), 2=Single-A (A-), 3=Single-A (A),
# 4=High-A (A+, int, Wnt), 5=Double-A (AA), 6=Triple-A (AAA), 7=MLB
# FCL = Florida Complex League, ACL = Arizona Complex League (post-2020 restructuring)
LEVEL_SCALE <- c(
  "FRk" = 1,
  "Rk"  = 1,
  "R"   = 1,
  "DSL" = 1,
  "FCL" = 1,   # Florida Complex League (replaced GCL after 2020)
  "ACL" = 1,   # Arizona Complex League (replaced AZL after 2020)
  "GCL" = 1,   # Gulf Coast League (historical)
  "AZL" = 1,   # Arizona League (historical)
  "A-"  = 2,
  "A"   = 3,
  "A+"  = 4,
  "int" = 4,
  "Wnt" = 4,
  "AA"  = 5,
  "AAA" = 6,
  "MLB" = 7
)

# Scoring version tag written to the metadata table for audit tracking
SCORING_VERSION <- "hitter_career_to_date_weighted_delta_v2"

# Raw count columns summed to build league context (must all be numeric)
RAW_COUNT_COLS <- c("pa", "ab", "x1b", "x2b", "x3b", "hr", "bb", "so", "hbp", "sb", "cs", "r")

# Columns loaded from the raw feather input
RAW_INPUT_COLS <- c(
  "mlbid", "name", "year", "lvl", "lg", "team", "franchise", "pos",
  "age", "pa", "ab", "x1b", "x2b", "x3b", "hr", "bb", "so", "hbp", "sb", "cs", "r"
)

# Batting rate columns used throughout league context / z-score calculation
RATE_COLS <- c("bb_pa", "k_pa", "obp", "slg", "ops", "iso", "sbp", "sba", "sb_trip", "sb_runs")

# Columns carried forward in the final player_seasons export table
PLAYER_SEASONS_EXPORT_COLS <- c(
  "mlbid", "name", "year", "lvl", "lg", "team", "franchise", "pos", "age",
  "pa", "ab", "x1b", "x2b", "x3b", "hr", "bb", "so", "hbp", "sb", "cs", "r",
  "bb_pa", "k_pa", "obp", "slg", "ops", "iso", "speed",
  "zbb", "zk", "zobp", "zslg", "zops", "ziso", "recency_weight"
)

# MLB Stats API sport IDs used by baseballr::mlb_stats()
# These map to the levels in LEVEL_SCALE; DSL/foreign rookie leagues are
# excluded — they are not present in the historical feather.
# A- (sport_id 15) is included but the MLB API rarely returns it; rows will
# appear only in seasons where short-season ball was active.
MLB_SPORT_IDS <- c(
  "MLB" = 1,
  "AAA" = 11,
  "AA"  = 12,
  "A+"  = 13,
  "A"   = 14,
  "A-"  = 15,
  "Rk"  = 16
)
