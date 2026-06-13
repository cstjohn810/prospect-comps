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

build_site_database <- function(
  source_db      = file.path("output", "db", "prospect_comps.sqlite"),
  site_db_output = file.path("output", "db", "prospect_comps_site.sqlite"),
  site_db_served = file.path("docs", "db", "prospect_comps_site.sqlite")
) {
  
  if (!file.exists(source_db)) {
    stop("Source database not found: ", source_db, call. = FALSE)
  }

  # Create directories
  dir.create(dirname(site_db_output), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(site_db_served), showWarnings = FALSE, recursive = TRUE)

  # Remove old versions
  if (file.exists(site_db_output)) unlink(site_db_output)
  if (file.exists(site_db_served)) unlink(site_db_served)

  source_con <- DBI::dbConnect(RSQLite::SQLite(), source_db)
  on.exit(DBI::dbDisconnect(source_con), add = TRUE)

  site_con <- DBI::dbConnect(RSQLite::SQLite(), site_db_output)
  on.exit(DBI::dbDisconnect(site_con), add = TRUE)

  stopifnot(DBI::dbIsValid(source_con), DBI::dbIsValid(site_con))

  copy_query <- function(table_name, query) {
    result <- tryCatch(
      DBI::dbGetQuery(source_con, query),
      error = function(e) stop("Query failed for table '", table_name, "':\n", conditionMessage(e), call. = FALSE)
    )
    DBI::dbWriteTable(site_con, table_name, result, overwrite = TRUE)
    message("  copied ", table_name, " (", nrow(result), " rows)")
  }

  message("Building site database...")

  copy_query("metadata",         "SELECT * FROM metadata")
  copy_query("scoring_features", "SELECT * FROM scoring_features")

  copy_query(
    "player_profiles",
    "
    SELECT
      mlbid, name,
      career_pa, career_ab,
      min_year, max_year,
      min_age, max_age, avg_age,
      latest_year, latest_age, latest_level, latest_level_numeric, avg_level_numeric,
      levels, team, franchise, pos,
      w_zbb, w_zk, w_zobp, w_zslg, w_zops, w_ziso, w_speed
    FROM player_profiles
    WHERE career_pa >= 100
    "
  )

  copy_query(
    "hitter_career_similarity",
    "
    SELECT
      mlbid, name,
      comp_mlbid, comp_name,
      rank, similarity_score, weighted_delta,
      career_pa, comp_career_pa,
      selected_levels, comp_levels,
      selected_latest_level, comp_latest_level,
      selected_zbb, comp_zbb,
      selected_zk,  comp_zk,
      selected_ziso, comp_ziso,
      selected_speed, comp_speed
    FROM hitter_career_similarity
    WHERE rank <= 10
    "
  )

  # Indexes for performance
  DBI::dbExecute(site_con, "CREATE INDEX idx_site_player_profiles_mlbid ON player_profiles(mlbid)")
  DBI::dbExecute(site_con, "CREATE INDEX idx_site_player_profiles_name ON player_profiles(name)")
  DBI::dbExecute(site_con, "CREATE INDEX idx_site_career_similarity_mlbid ON hitter_career_similarity(mlbid, rank)")

  DBI::dbExecute(site_con, "VACUUM")

  message("✅ Wrote site database: ", site_db_output)
  print(file.info(site_db_output)[, c("size", "mtime")])

  # === Copy to the location served by GitHub Pages ===
  file.copy(site_db_output, site_db_served, overwrite = TRUE)
  message("✅ Copied to served location: ", site_db_served)
  print(file.info(site_db_served)[, c("size", "mtime")])

  invisible(site_db_served)
}

# Run it
build_site_database()