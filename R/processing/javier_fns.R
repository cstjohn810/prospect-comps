# ISO = ((2B) + (2\*3B) + (3\*HR)) / AB
# 
# Speed: 
#   Stolen Base Percentage = (SB+3)/(SB+CS+7)
# Stolen Base Attempts = (SB+CS)/(1B+BB+HBP)
# Triples = 3B/(AB-HR-K)
# Runs = (R-HR)/(1B+2B+3B+BB+HBP)
# Add in some +1 for SBA and SBR due to infinity issues in some seasons
# Each underlying calculation is based off a z-score using the three-year average from that year, league, and level.
# After z-scores are created for each statistic, the final speed score is created.
# Speed = 50+4.25*(zSBP+zSBA+zT+zR)
# 
# OPS = (AB* (H + BB + HBP) + TB\*(AB + BB + SF + HBP))/(AB*(AB + BB + SF + HBP))
# -Don't have SF from old data, use PA as denominator in OBP instead of AB + BB + SF + HBP
# -Calculate OBP and SLG, then regress each individually and add that together for regressed OPS
#   -OBP 460 PA, SLG = 320 AB
#   -460/.7-460 = 197 PA added to OBP, 320/.7-320 = 137 AB added to SLG


obp_fn <- function(df) {
  df %>% 
    mutate(h = x1b + x2b + x3b + hr,
      obp = (h + bb + hbp) / pa)
}

slg_fn <- function(df) {
  df %>% 
    mutate(tb = x1b + 2*x2b + 3*x3b + 4*hr,
           slg = tb / ab)
}

ops_fn <- function(df) {
  df %>%
    mutate(ops = obp + slg)
}

iso_fn <- function(df){
  df %>% 
    mutate(iso = (x2b + 2*x3b + 3*hr)/ab)
}

speed_component_fn <- function(df){
  df %>% 
    mutate(sbp = (sb + 3)/(sb + cs + 7),
           sba = (sb + cs)/(x1b + bb + hbp + 1),
           sb_trip = x3b/(ab - hr - so),
           sb_runs = (r - hr)/(x1b + x2b + x3b + bb + hbp + 1))
}

# Function to calculate rolling averages
rolling_mean <- function(x, width, ...) {
  rollapply(x, width = width, FUN = mean, fill = NA, align = 'right', na.rm = TRUE)
}

# Function to calculate weighted rolling mean for age
rolling_weighted_mean <- function(age, pa, width) {
  rollapply(1:length(age), width = width, FUN = function(i) weighted.mean(age[i], pa[i], na.rm = TRUE), fill = NA, align = 'right')
}

# Regression Function
# These are the league regression equations that I used
# Regressed Walk Rate = (BB+LgBBPA*65)/(PA+65)
# Regressed Strikeout Rate = (K+LgKPA*35)/(PA+35)
# Regressed ISO = (ISO\*AB+LgISO\*85)/(AB+85)
# Regressed OBP = (OBP\*PA+LgOBP\*197)/(PA+197)
# Regressed SLG = (SLG\*AB+LgSLG\*137)/(SLG+137)

regress_fn <- function(df){
  df %>% 
    mutate(reg_bb_pa = (bb + lg_avg_bb_pa*65)/(pa+65),
           reg_k_pa = (so + lg_avg_k_pa*35)/(pa + 35),
           reg_iso = (iso*ab + lg_avg_iso*85)/(ab + 85),
           reg_obp = (obp*pa + lg_avg_obp*197)/(pa + 197),
           reg_slg = (slg*ab + lg_avg_slg*137)/(ab + 137),
           reg_ops = reg_obp+reg_slg)
}

# Helper function to calculate z-scores
calculate_zscores <- function(df, bb_pa_col, k_pa_col, obp_col, slg_col, ops_col, iso_col, sbp_col, sba_col, sb_trip_col, sb_runs_col) {
  df %>%
    mutate(zbb = (!!sym(bb_pa_col) - lg_avg_bb_pa) / sd_bb_pa,
           zk = (!!sym(k_pa_col) - lg_avg_k_pa) / sd_k_pa,
           zobp = (!!sym(obp_col) - lg_avg_obp) / sd_obp,
           zslg = (!!sym(slg_col) - lg_avg_slg) / sd_slg,
           zops = (!!sym(ops_col) - lg_avg_ops) / sd_ops,
           ziso = (!!sym(iso_col) - lg_avg_iso) / sd_iso,
           zsbp = (!!sym(sbp_col) - lg_avg_sbp) / sd_sbp,
           zsba = (!!sym(sba_col) - lg_avg_sba) / sd_sba,
           zsb_trip = (!!sym(sb_trip_col) - lg_avg_sb_trip) / sd_sb_trip,
           zsb_runs = (!!sym(sb_runs_col) - lg_avg_sb_runs) / sd_sb_runs) %>%
    group_by(year, lvl, lg) %>%
# This line scales the stolen base numbers due to the issue Patriot noticed
    mutate(across(zsbp:zsb_runs, ~scale(.x, scale = FALSE))) %>%
    ungroup()
}

# Z-scores function
zscore_fn <- function(df, regress = TRUE) {
  if (regress) {
    df <- calculate_zscores(df, "reg_bb_pa", "reg_k_pa", "reg_obp", "reg_slg", "reg_ops", "reg_iso", "sbp", "sba", "sb_trip", "sb_runs")
  } else {
    df <- calculate_zscores(df, "bb_pa", "k_pa", "obp", "slg", "ops", "iso", "sbp", "sba", "sb_trip", "sb_runs")
  }
  
  df %>%
    mutate(speed = 50 + 4.25 * (zsbp + zsba + zsb_trip + zsb_runs))
}


baseball_reference_search <- function(player_name) {
  
  # Encode the player name for use in a URL
  encoded_name <- URLencode(player_name)
  
  # Construct the search URL
  url <- paste0("https://www.baseball-reference.com/search/search.fcgi?search=", encoded_name)
  
  # Full path to Firefox executable
  firefox_path <- "C:/Program Files/Mozilla Firefox/firefox.exe"  # Update this path as necessary
  
  # Open the URL in Firefox
  browseURL(url, browser = firefox_path)
}