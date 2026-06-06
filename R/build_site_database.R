required_packages <- c("DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the site database build.",
    call. = FALSE
  )
}

source_db <- file.path("output", "db", "prospect_comps.sqlite")
site_db <- file.path("output", "db", "prospect_comps_site.sqlite")

if (!file.exists(source_db)) {
  stop("Source database not found: ", source_db, call. = FALSE)
}

if (file.exists(site_db)) {
  unlink(site_db)
}

source_con <- DBI::dbConnect(RSQLite::SQLite(), source_db)
site_con <- DBI::dbConnect(RSQLite::SQLite(), site_db)
on.exit(DBI::dbDisconnect(source_con), add = TRUE)
on.exit(DBI::dbDisconnect(site_con), add = TRUE)

copy_query <- function(table_name, query) {
  DBI::dbWriteTable(site_con, table_name, DBI::dbGetQuery(source_con, query), overwrite = TRUE)
}

copy_query("metadata", "SELECT * FROM metadata")
copy_query("scoring_features", "SELECT * FROM scoring_features")

copy_query(
  "player_profiles",
  "
  SELECT
    batter, name, career_pa, career_ab, min_year, max_year,
    min_age, max_age, avg_age, latest_year, latest_age, latest_level,
    latest_level_numeric, avg_level_numeric, levels, team, franchise, pos,
    mlb_pa, vorp, w_zbb, w_zk, w_zobp, w_zslg, w_zops, w_ziso, w_speed
  FROM player_profiles
  WHERE career_pa >= 100
  "
)

copy_query(
  "hitter_career_similarity",
  "
  SELECT
    batter, name, comp_batter, comp_name, rank, similarity_score,
    weighted_delta, context_delta, age_delta, latest_level_delta,
    avg_level_delta, pa_delta, career_pa, comp_career_pa,
    selected_levels, comp_levels, selected_latest_level, comp_latest_level,
    selected_avg_age, comp_avg_age, selected_zbb, comp_zbb, selected_zk,
    comp_zk, selected_ziso, comp_ziso, selected_zops, comp_zops,
    selected_speed, comp_speed
  FROM hitter_career_similarity
  WHERE rank <= 10
  "
)

DBI::dbExecute(site_con, "CREATE INDEX idx_site_player_profiles_batter ON player_profiles(batter)")
DBI::dbExecute(site_con, "CREATE INDEX idx_site_player_profiles_name ON player_profiles(name)")
DBI::dbExecute(site_con, "CREATE INDEX idx_site_career_similarity_batter ON hitter_career_similarity(batter, rank)")

DBI::dbExecute(site_con, "VACUUM")

message("Wrote ", site_db)
print(file.info(site_db)[, c("size", "mtime")])
