# config.R — Default configuration object for CATOPT
# All parameters are read from here. Nothing hardcoded elsewhere.

default_config <- list(

  # --- Throughput rates: claims per day (adjuster skill x claim severity) ---
  # NA = cannot handle (hard constraint)
  throughput = matrix(
    c(
      0.25,  1.00,  2.00,  2.50,  3.00,  # PL5
      0.125, 0.75,  2.00,  2.50,  3.00,  # PL4
      NA,    0.125, 2.00,  2.50,  3.00,  # PL3
      NA,    NA,    1.50,  2.00,  3.00,  # PL2
      NA,    NA,    NA,    0.50,  3.00,  # PL1
      NA,    NA,    NA,    NA,    NA     # PL0 (never assigned claims)
    ),
    nrow     = 6,
    ncol     = 5,
    byrow    = TRUE,
    dimnames = list(
      paste0("PL", 5:0),   # rows: PL5 down to PL0
      paste0("Sev", 5:1)   # cols: Sev5 down to Sev1
    )
  ),

  # --- SLA windows (days from NOL date) ---
  sla_days = c(
    "5" = 7,
    "4" = 14,
    "3" = 21,
    "2" = 30,
    "1" = 45
  ),

  # --- Handle type mix (on-site proportion per severity) ---
  # Remainder is virtual. Randomized once during data prep.
  onsite_prob = c(
    "5" = 1.00,
    "4" = 1.00,
    "3" = 0.75,
    "2" = 0.50,
    "1" = 0.00
  ),

  # --- Severity weights for MIP objective ---
  severity_weight = c(
    "5" = 100,
    "4" = 50,
    "3" = 20,
    "2" = 10,
    "1" = 5
  ),

  # --- Qtr Rank bonus (soft tiebreaker in MIP objective) ---
  qtr_rank_bonus = c(
    "0" = 1.00,  # unranked
    "1" = 1.02,  # send first
    "2" = 1.01,
    "3" = 1.00,
    "4" = 0.99   # send last
  ),

  # --- Minimum skill level required per severity ---
  min_skill = c(
    "5" = 4,
    "4" = 3,
    "3" = 2,
    "2" = 1,
    "1" = 1
  ),

  # --- Cost rates ($/day) — full rates for reference / reporting ---
  cost_local      = 290,   # in-cluster staff (sunk cost, tracked for reference)
  cost_deployed   = 440,   # cross-cluster staff ($290 + $150 per diem)
  cost_contractor = 500,   # contractor daily rate

  # --- Incremental cost rates (new money only) ---
  # Local staff are salaried — the insurer pays them whether a CAT happens or not.
  # Only per diem for deployed staff and full contractor cost are incremental.
  cost_local_incremental      = 0L,   # $0 — already on salary, sunk cost
  cost_deployed_incremental   = 150L, # $150/day per diem only (the $290 base is sunk)
  cost_contractor_incremental = 500L, # $500/day — 100% new spend

  # --- SLA failure exposure rates (estimated cost per missed claim) ---
  # NOT contractual penalties. Modeled risk: customer retention, complaint
  # escalation, regulatory exposure, and potential bad-faith litigation.
  # All rates configurable — see dashboard display.
  sla_failure_cost = c(
    "5" = 5000,  # Total loss / bad-faith litigation risk
    "4" = 2500,  # Major damage, high churn risk
    "3" = 1000,  # Moderate damage, escalation risk
    "2" =  500,  # Minor damage, dissatisfaction
    "1" =  100   # Cosmetic, minimal impact
  ),

  # --- Mobilization delays (days before adjuster can start working) ---
  delay_local      = 0,
  delay_deployed   = 0,   # pre-positioned before storm: deployed staff on site Day 1
  delay_contractor = 2,   # reactive mid-event hires only (pre-deployment uses day_arrives=1L)

  # --- PL0 boost ---
  pl0_boost_factor = 1.20,   # 20% throughput multiplier for paired mentor
  pl0_min_mentor_skill = 4,  # only PL4+ can be PL0 mentors

  # --- Drive penalty ---
  # Baseline: up to 2.5 hours drive = no penalty
  # After baseline: -20% per additional 30 min
  # Floor: 10% of base rate (penalty capped at 0.90)
  drive_speed_kmh       = 105,   # assumed average speed (65 mph)
  drive_baseline_hours  = 2.5,   # hours before penalty kicks in
  drive_penalty_step    = 0.20,  # penalty per 30-min increment
  drive_step_minutes    = 30,    # increment size in minutes
  drive_penalty_floor   = 0.10,  # minimum remaining productivity (10%)

  # --- Over-qualification penalty ---
  # Discourages PL5 adjusters from taking Sev-4 and below work.
  # Preserves PL5 capacity for Sev-5 claims (where only PL4+ are eligible).
  # PL5 still takes lower-severity work as a last resort — penalty, not ban.
  # Applied when adj$skill == 5 and claim severity < 5.
  over_qualify_penalty = 0.40,

  # --- Virtual claim soft penalty in MIP objective ---
  # Strongly discourages in-cluster adjusters from taking virtual claims.
  # OUT pool (434 adjusters) has more than enough capacity for all ~209 virtual claims.
  # In-cluster adjusters should be preserved for on-site work in their cluster.
  virtual_incluster_penalty = 0.60,

  # --- Will Travel = N soft penalty ---
  # Real event data shows 57% of non-travelers were deployed anyway during CAT 82.
  # Will Travel = N is a PREFERENCE, not a hard constraint. The MIP can still use
  # non-travelers for cross-cluster on-site work — it just costs them priority weight.
  # Applied only when: on-site claim, different cluster, will_travel = FALSE.
  # No penalty for: same cluster (local), virtual claims, contractors.
  will_travel_penalty = 0.30,

  # --- Contractor trigger ---
  # Request contractors when unassigned claim is within N days of SLA deadline
  contractor_trigger_days = 3,

  # --- Supplementary state-to-cluster mapping ---
  # States that have no usable claims (zero claims or all NA severity) cannot be
  # derived from the claims data at runtime. List them here so their adjusters are
  # assigned to the correct cluster instead of the OUT pool.
  #
  # For CAT 82: SC/LA/DC/DE/NH/VT/ME/RI all had zero usable claims after NA filter.
  # Louisiana had 7 rows but ALL had NA Final_Severity — filtered before geography runs.
  #
  # For a new event: add any states whose adjusters belong to a cluster but whose
  # claims are missing from the file. Leave empty (list()) if not needed.
  #
  # Claims-derived mapping always takes precedence — supplementary only fills gaps.
  supplementary_state_cluster = c(
    "SC" = "0",   # Carolinas cluster (with NC)
    "LA" = "5",   # Gulf cluster (with MS) — 7 claims but all NA severity
    "DC" = "4",   # Mid-Atlantic cluster (with MD/VA)
    "DE" = "4",   # Mid-Atlantic cluster
    "NH" = "6",   # New England cluster (with CT/MA)
    "VT" = "6",   # New England cluster
    "ME" = "6",   # New England cluster
    "RI" = "6"    # New England cluster
  ),

  # --- Simulation seed (for handle type randomization) ---
  seed = 7900,

  # --- Claims file column name mapping ---
  # Change these when running a different CAT event whose CSV has different column names.
  # All other logic reads from these mappings — nothing else in the codebase is hardcoded.
  claims_cols = list(
    claim_id       = "Claim Number",
    loss_date      = "Loss Date",
    nol_date       = "NOL Date",
    state          = "Accident State",
    city           = "Accident City",
    zip            = "Accident Zip",
    peril          = "Peril Group",
    weather_text   = "Weather Text",
    cluster_id     = "7_cluster_cluster_id",          # UPDATE for different K-means runs
    distance_km    = "7_cluster_distance_to_centroid_km",  # UPDATE for different K-means runs
    severity       = "Final_Severity",                # UPDATE if column is named differently
    severity_source= "Severity_Source"                # optional — used for reporting only
  ),

  # --- Roster file column name mapping ---
  roster_cols = list(
    adj_id        = "Adjuster Id",
    location      = "Location",
    skill         = "PL Skill Level",
    will_travel   = "Will Travel",
    tour_length   = "Preferred Tour Length",
    resource_type = "WFM Resource Type Desc",
    org_group     = "Org Group",
    event_id      = "Event Id",
    qtr_rank      = "Qtr Rank Id"
  ),

  # --- MIP solver settings ---
  mip_gap       = 0.02,   # 2% optimality gap (speed vs. quality trade-off)
  mip_time_limit = 60,    # seconds per daily solve
  decompose_by_cluster = TRUE  # solve each cluster's MIP independently (recommended)
)


# Validate config — call this after loading any custom config
validate_config <- function(cfg) {
  stopifnot(
    is.matrix(cfg$throughput),
    nrow(cfg$throughput) == 6,
    ncol(cfg$throughput) == 5,
    all(names(cfg$sla_days)       %in% c("1","2","3","4","5")),
    all(names(cfg$onsite_prob)    %in% c("1","2","3","4","5")),
    all(names(cfg$severity_weight)%in% c("1","2","3","4","5")),
    all(names(cfg$min_skill)      %in% c("1","2","3","4","5")),
    cfg$pl0_boost_factor  >= 1.0,
    cfg$drive_penalty_floor > 0,
    cfg$drive_penalty_floor <= 1,
    # Column mappings — required keys must be present
    !is.null(cfg$claims_cols$claim_id),
    !is.null(cfg$claims_cols$nol_date),
    !is.null(cfg$claims_cols$state),
    !is.null(cfg$claims_cols$cluster_id),
    !is.null(cfg$claims_cols$distance_km),
    !is.null(cfg$claims_cols$severity),
    !is.null(cfg$roster_cols$adj_id),
    !is.null(cfg$roster_cols$location),
    !is.null(cfg$roster_cols$skill)
  )
  invisible(TRUE)
}
