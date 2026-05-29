# utils.R — Core utility functions for CATOPT
# All functions are pure (no side effects) and work with any config object.

# --- Throughput lookup ---
# Returns claims/day for a given skill level and severity.
# Returns NA if the adjuster cannot handle that severity (hard constraint).
get_throughput <- function(skill, severity, cfg) {
  skill_row <- paste0("PL", skill)
  sev_col   <- paste0("Sev", severity)
  if (!skill_row %in% rownames(cfg$throughput)) return(NA_real_)
  if (!sev_col   %in% colnames(cfg$throughput)) return(NA_real_)
  cfg$throughput[skill_row, sev_col]
}

# Vectorized version — returns a vector given vectors of skill and severity
get_throughput_v <- function(skills, severities, cfg) {
  mapply(get_throughput, skills, severities, MoreArgs = list(cfg = cfg))
}


# --- SLA deadline ---
# Returns the SLA deadline day number given a claim's arrival day and severity.
get_sla_deadline <- function(arrival_day, severity, cfg) {
  sla_window <- cfg$sla_days[as.character(severity)]
  if (is.na(sla_window)) stop(paste("Unknown severity:", severity))
  arrival_day + sla_window - 1
}


# --- Days remaining until SLA ---
# Returns days left. Floored at 0 (never negative for weighting purposes).
days_until_sla <- function(current_day, sla_deadline) {
  pmax(0L, as.integer(sla_deadline - current_day))
}


# --- Drive penalty ---
# Returns the productivity MULTIPLIER (0.10 to 1.00) given distance to centroid.
# Virtual claims always return 1.0 (no penalty).
# Formula:
#   drive_hours = distance_km / speed_kmh
#   extra_hours = max(0, drive_hours - baseline_hours)
#   extra_steps = extra_hours / (step_minutes / 60)
#   penalty     = extra_steps * penalty_step
#   multiplier  = max(floor, 1 - penalty)
drive_multiplier <- function(distance_km, is_virtual, cfg) {
  if (is_virtual) return(1.0)
  drive_hours  <- distance_km / cfg$drive_speed_kmh
  extra_hours  <- pmax(0, drive_hours - cfg$drive_baseline_hours)
  extra_steps  <- extra_hours / (cfg$drive_step_minutes / 60)
  penalty      <- extra_steps * cfg$drive_penalty_step
  pmax(cfg$drive_penalty_floor, 1 - penalty)
}

# Vectorized version
drive_multiplier_v <- function(distance_km, is_virtual, cfg) {
  mapply(drive_multiplier, distance_km, is_virtual, MoreArgs = list(cfg = cfg))
}


# --- Effective throughput ---
# Throughput adjusted for drive penalty and optional PL0 boost.
effective_throughput <- function(skill, severity, distance_km, is_virtual,
                                  has_pl0_boost = FALSE, cfg) {
  base <- get_throughput(skill, severity, cfg)
  if (is.na(base)) return(NA_real_)
  mult  <- drive_multiplier(distance_km, is_virtual, cfg)
  boost <- if (has_pl0_boost) cfg$pl0_boost_factor else 1.0
  base * mult * boost
}


# --- Days to complete a claim ---
# Given effective throughput, how many work days does the claim take?
# Uses ceiling: a 0.25 claims/day rate means 4 days to complete 1 claim.
days_to_complete <- function(effective_rate) {
  if (is.na(effective_rate) || effective_rate <= 0) return(Inf)
  ceiling(1 / effective_rate)
}


# --- Priority weight ---
# MIP objective weight for a claim.
# Higher severity + fewer SLA days remaining = higher weight.
priority_weight <- function(severity, current_day, sla_deadline, cfg) {
  sev_weight    <- cfg$severity_weight[as.character(severity)]
  days_left     <- pmax(1L, as.integer(sla_deadline - current_day))
  sev_weight * (1 / days_left)
}


# --- Eligibility check ---
# Returns TRUE if adjuster with given skill CAN handle a claim of given severity.
is_eligible <- function(skill, severity, cfg) {
  if (skill == 0) return(FALSE)  # PL0 never assigned claims
  required <- cfg$min_skill[as.character(severity)]
  skill >= required
}

# Vectorized version
is_eligible_v <- function(skills, severities, cfg) {
  mapply(is_eligible, skills, severities, MoreArgs = list(cfg = cfg))
}


# --- Qtr Rank bonus ---
get_rank_bonus <- function(qtr_rank, cfg) {
  key <- as.character(qtr_rank)
  bonus <- cfg$qtr_rank_bonus[key]
  if (is.na(bonus)) return(1.00)  # default for unexpected values
  bonus
}


# --- Cost calculation ---
# Returns the daily cost for an adjuster based on assignment type.
# assignment_type: "local", "deployed", or "contractor"
daily_cost <- function(assignment_type, cfg) {
  switch(assignment_type,
    local      = cfg$cost_local,
    deployed   = cfg$cost_deployed,
    contractor = cfg$cost_contractor,
    stop(paste("Unknown assignment_type:", assignment_type))
  )
}


# --- Mobilization delay ---
# Returns days before an adjuster can START WORKING after assignment.
# Contractors: travel delay is already baked into day_arrives via
#   day_arrives = day_requested + cfg$delay_contractor in contractor_logic.R.
# After arrival they are ready immediately — same as local staff.
mobilization_delay <- function(assignment_type, cfg) {
  switch(assignment_type,
    local      = cfg$delay_local,
    deployed   = cfg$delay_deployed,
    contractor = cfg$delay_local,   # travel already paid; 0 delay after arrival
    stop(paste("Unknown assignment_type:", assignment_type))
  )
}


# --- Assignment type ---
# Determines if an adjuster is local, deployed, or contractor.
# home_cluster: adjuster's home cluster (NA for contractors)
# target_cluster: the cluster where the work will be done
assignment_type <- function(home_cluster, target_cluster) {
  if (is.na(home_cluster)) return("contractor")
  if (home_cluster == target_cluster) return("local")
  return("deployed")
}
