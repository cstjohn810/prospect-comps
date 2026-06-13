# ==============================================================================
# DATABASE I/O
# ==============================================================================
# Export results to SQLite with indexes for efficient querying.

# Create SQLite database with all analysis tables and efficient lookup indexes.
#
# @param db_path  Destination file path (created, or overwritten if it exists).
# @param tables   Named list of data frames to write as tables.
#
# Indexes created cover the most common lookup patterns used by the API:
#   - player_levels   by player_level_id and name
#   - player_profiles by mlbid and name
#   - hitter_similarity by player_level_id and comp_player_level_id
#   - hitter_career_similarity by mlbid+rank and comp_mlbid
#   - player_seasons by mlbid+lvl
write_database <- function(db_path, tables) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(db_path)) {
    unlink(db_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (table_name in names(tables)) {
    DBI::dbWriteTable(con, table_name, as.data.frame(tables[[table_name]]), overwrite = TRUE)
  }

  DBI::dbExecute(con, "CREATE INDEX idx_player_levels_player_level_id ON player_levels(player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_levels_name ON player_levels(name)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_profiles_mlbid ON player_profiles(mlbid)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_profiles_name ON player_profiles(name)")
  DBI::dbExecute(con, "CREATE INDEX idx_similarity_player_level_id ON hitter_similarity(player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_similarity_comp_player_level_id ON hitter_similarity(comp_player_level_id)")
  DBI::dbExecute(con, "CREATE INDEX idx_career_similarity_mlbid ON hitter_career_similarity(mlbid, rank)")
  DBI::dbExecute(con, "CREATE INDEX idx_career_similarity_comp_mlbid ON hitter_career_similarity(comp_mlbid)")
  DBI::dbExecute(con, "CREATE INDEX idx_player_seasons_mlbid_level ON player_seasons(mlbid, lvl)")

  invisible(db_path)
}

# Copy a database file from one location to another.
# Typically used to promote a freshly-built private database to a public /
# deployable path.
copy_database <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  file.copy(source, destination, overwrite = TRUE)
}
