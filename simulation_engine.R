rm(list = ls())
# ============================================================
# DS 7900 - CAT Claims Allocation Simulation
# CAT Code 82 | December 2023 Storm Event
# ============================================================
# v6 REWRITES:
#   1. Multi-cluster adjuster eligibility (state -> multiple clusters)
#   2. Two-tier deployment: PL4/5 -> Sev-5 gaps, PL3 -> Sev-4 gaps
#   3. match_claims() complete rewrite:
#      a. Phase 1: On-site by cluster (sev 5->4->3->2), in-cluster only
#      b. Phase 2: Virtual -> OUT pool + surplus only
#      c. In-cluster adjusters NEVER do virtual
#      d. Hard routing table per PL level
#      e. Time-based cascade unlock (Day 8/15/22)
#   4. Contractor tour default 8 days (configurable)
#   5. Deployed adjusters start Day 2 (travel time)
#   6. Virtual routing: Sev-1->OUT PL1/2, Sev-2->OUT PL1/2, Sev-3->OUT PL3+
#   7. Cost formula: contractor billed tour_length not sim_days
# ============================================================

library(readxl)
library(dplyr)

ROSTER_PATH <- Sys.getenv("ROSTER_PATH", "path/to/roster.xlsx")
CLAIMS_PATH <- Sys.getenv("CLAIMS_PATH", "path/to/claims.csv")

# ============================================================
# CONSTANTS
# ============================================================

COMPLETION_DAYS <- list(
  "5" = c("5"=4L, "4"=8L),
  "4" = c("5"=1L, "4"=2L, "3"=8L),
  "3" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L),
  "2" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L, "1"=2L),
  "1" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L, "1"=1L)
)

# PL0 booster adds 20% speed: floor(days * 0.8), min 1
# Key flip: PL4 Sev-5 goes from 8 -> 6 days (SLA=7, now clears)
COMPLETION_DAYS_BOOSTED <- list(
  "5" = c("5"=3L, "4"=6L),
  "4" = c("5"=1L, "4"=1L, "3"=6L),
  "3" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L),
  "2" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L, "1"=1L),
  "1" = c("5"=1L, "4"=1L, "3"=1L, "2"=1L, "1"=1L)
)

MIN_SKILL   <- c("5"=4L, "4"=3L, "3"=2L, "2"=1L, "1"=1L)
SLA_DAYS    <- c("5"=7L, "4"=14L, "3"=21L, "2"=28L, "1"=28L)
PENALTY     <- c("5"=4000, "4"=2000, "3"=1000, "2"=500, "1"=200)

COST_LOCAL      <- 290
COST_DEPLOYED   <- 440
COST_CONTRACTOR <- 650

DEFAULT_CONTRACTOR_TOUR <- 8L   # Sev-5 focused: 7-day SLA + 1 buffer

# ============================================================
# LOCATION -> STATE (unchanged)
# ============================================================

location_to_state <- c(
  "Charlotte-Carmel Commons"="NC", "Raleigh"="NC",
  "South Carolina - Virtual"="SC", "Durham - Emperor Blvd"="NC",
  "Alpharetta-Atlanta"="GA", "Birmingham-Hoover"="AL",
  "Wyomissing-Reading"="PA", "Blue Bell-Phil-Lakeview Dr"="PA",
  "Philadelphia-Market St"="PA", "Pittsburgh-Washington Pl"="PA",
  "Exton-Philadelphia-Eagleview"="PA", "Morristown"="NJ",
  "Edison-343 Thornall"="NJ", "Marlton-Camden Law"="NJ",
  "New York City - Lexington Ave"="NY", "Melville NY Corp Center Dr"="NY",
  "Buffalo"="NY", "Rochester - Linden Oaks"="NY",
  "Syracuse-Plainfield Rd"="NY", "Albany - Park Pl"="NY",
  "White Plains-Law1-Liability"="NY", "Glens Falls - Queensbury"="NY",
  "Franklin-Nashville"="TN", "Knoxville"="TN",
  "Memphis-6750 Poplar"="TN", "Louisville-Hurstbourne Pkwy"="KY",
  "Hunt Valley-Baltimore-Schilling"="MD", "Rosedale-Baltimore"="MD",
  "Chantilly-WashingtnDC-PkMeadow"="VA", "Richmond-Mayland Dr"="VA",
  "Washington, DC - 13th Street"="DC", "Delaware - Virtual"="DE",
  "Flowood-Jackson-Canebrake"="MS", "Metairie-New Orleans-3900 Cswy"="LA",
  "Hartford-Field"="CT", "Windsor-Htfd-99 Lambrtn-ClaimU"="CT",
  "Hamden Law"="CT", "Hartford - Tower"="CT",
  "West Bridgewater"="MA", "Massachusetts - Virtual"="MA",
  "Hudson - Boston"="MA", "Boston-Financial Center"="MA",
  "Bedford - Commerce"="NH", "South Portland"="ME",
  "Vermont - Virtual"="VT", "Providence Law"="RI",
  "Naperville-Chicago"="IL", "Maryland Heights-St. Louis"="MO",
  "Centennial-Denver-Geddes"="CO", "Overland Pk-Kansas City-132 St"="KS",
  "Phoenix-Tatum Blvd"="AZ", "Richardson-Dallas"="TX",
  "Houston - Westway"="TX", "San Antonio-McAllister Fwy"="TX",
  "Austin-901 Mo-Pac"="TX", "Farmers Brnch-Dallas"="TX",
  "St. Paul"="MN", "Las Vegas-Arroyo Crossing"="NV",
  "Walnut Creek-San Francisco-401"="CA", "RanchoCordova-Sacramento-11090"="CA",
  "Diamond Bar-Los Angeles"="CA", "Glendale-LosAngeles-655NCentrl"="CA",
  "Irvine-Los Angeles-Michelson"="CA", "Fresno"="CA",
  "Walnut Creek-Pleasant Hill"="CA", "San Diego"="CA",
  "Orange-Los Angeles-Law"="CA", "Indianapolis - 96th St"="IN",
  "Albuquerque-Lang Avenue"="NM", "Omaha-Dodge Rd"="NE",
  "Brookfield-Milwaukee"="WI", "Oklahoma City Law"="OK",
  "Tulsa-5100 Skelly"="OK", "Independence-Cleveland"="OH",
  "Columbus-Easton Oval"="OH", "Cincinnati-Elsinore"="OH",
  "Salt Lake City-1100 E 6600"="UT", "West Des Moines-Jordan Creek"="IA",
  "Troy-Detroit"="MI", "Portland"="OR",
  "Federal Way-Seattle-6th Ave"="WA", "Spokane"="WA",
  "Orlando"="FL", "Lake Mary-Orlando"="FL",
  "Tampa - Dale Mabry"="FL", "Jacksonville Law"="FL",
  "Boise-Meridian"="ID", "North Dakota - Virtual"="ND",
  "Little Rock"="AR", "Honolulu"="HI"
)

# ============================================================
# STATE -> CLUSTER (from claims K-means — states cross clusters)
# ============================================================

STATE_TO_CLUSTERS <- list(
  NC = "C0", SC = "C0",
  VA = c("C4", "C0"),        # primary C4 (44 VA claims in C4 vs 2 in C0)
  GA = c("C1", "C0"),        # primary C1 (9 GA claims in C1 vs 2 in C0)
  AL = c("C1", "C3"),        # primary C1 (Birmingham)
  TN = c("C3", "C1", "C5"), # primary C3 (Nashville/Knoxville/Memphis = TN/KY cluster)
  PA = c("C2", "C4"),        # primary C2 correct
  NJ = "C2",
  NY = c("C2", "C6"),        # primary C2 correct
  MD = c("C4", "C2"),        # primary C4 (56 MD claims in C4 vs 2 in C2)
  KY = "C3",
  DC = "C4", DE = "C4",
  MS = "C5", LA = "C5",
  CT = "C6", MA = "C6", NH = "C6",
  VT = "C6", ME = "C6", RI = "C6"
)

get_primary_cluster <- function(state) {
  cls <- STATE_TO_CLUSTERS[[state]]
  if (is.null(cls)) return("OUT")
  cls[1]
}

get_eligible_clusters <- function(state) {
  cls <- STATE_TO_CLUSTERS[[state]]
  if (is.null(cls)) return("OUT")
  cls
}

# ============================================================
# LOAD & BUILD
# ============================================================

roster_raw <- read_excel(ROSTER_PATH)
claims_raw <- read.csv(CLAIMS_PATH)
cat(sprintf("Loaded: %d adjusters, %d claims\n", nrow(roster_raw), nrow(claims_raw)))

adjusters <- roster_raw %>%
  transmute(
    Adjuster_Id    = as.integer(`Adjuster Id`),
    Skill      = as.integer(`PL Skill Level`),
    Location   = Location,
    Tour_Length = ifelse(is.na(`Preferred Tour Length`), 21L, as.integer(`Preferred Tour Length`))
  ) %>%
  mutate(
    State = location_to_state[Location],
    Home_Cluster = sapply(State, get_primary_cluster, USE.NAMES = FALSE),
    Eligible_Clusters = I(lapply(State, get_eligible_clusters)),
    Tour_Start = 1L,
    Tour_End = Tour_Start + Tour_Length,
    Deployed_To = NA_character_,
    Is_Deployed = FALSE,
    Is_Contractor = FALSE,
    Daily_Cost = COST_LOCAL,
    Status = "Active",
    Current_Claim = NA_integer_,
    Claims_Assigned = 0L,
    Working_Cluster = Home_Cluster,
    Has_Boost  = FALSE,
    Boosted_By = NA_integer_,
    Boosting   = NA_integer_
  )

unmapped <- adjusters %>% filter(is.na(State)) %>% distinct(Location)
if (nrow(unmapped) > 0) { cat("WARNING - Unmapped:\n"); print(unmapped$Location) }

set.seed(42)
claims <- claims_raw %>%
  filter(!is.na(Final_Severity)) %>%
  transmute(
    Claim_Number = as.integer(Claim.Number),
    Severity = as.integer(Final_Severity),
    Cluster = paste0("C", X7_cluster_cluster_id),
    Distance_km = X7_cluster_distance_to_centroid_km,
    State = Accident.State
  ) %>%
  mutate(
    Handle_Type = case_when(
      Severity >= 4 ~ "On-site",
      Severity == 3 ~ sample(c("On-site","Virtual"), n(), TRUE, c(0.75, 0.25)),
      Severity == 2 ~ sample(c("On-site","Virtual"), n(), TRUE, c(0.50, 0.50)),
      TRUE ~ "Virtual"
    ),
    Distance_Penalty = as.integer(Handle_Type == "On-site" & Distance_km > 250),
    SLA_Deadline = as.integer(SLA_DAYS[as.character(Severity)]),
    Status = "Unassigned", Assigned_To = NA_integer_,
    Day_Assigned = NA_integer_, Day_Completed = NA_integer_,
    Completion_Day = NA_integer_, Priority = 0, Priority_Adjusted = 0
  )

cat(sprintf("Claims: %d | Adjusters: %d\n", nrow(claims), nrow(adjusters)))

get_completion_days <- function(severity, skill, boosted = FALSE) {
  tbl <- if (isTRUE(boosted)) COMPLETION_DAYS_BOOSTED[[as.character(severity)]]
         else                 COMPLETION_DAYS[[as.character(severity)]]
  if (is.null(tbl)) return(NA_integer_)
  val <- tbl[as.character(skill)]
  if (is.na(val)) return(NA_integer_)
  as.integer(val)
}

# ============================================================
# DEPLOYMENT — TWO TIER
# Tier 1: PL4/5 from OUT -> Sev-5 gaps
# Tier 2: PL3 from OUT -> Sev-4 gaps
# Remaining OUT: virtual queue
# ============================================================

deploy_tier <- function(adjusters, claims, target_sev, min_skill, work_days_factor) {
  demand <- claims %>%
    filter(Severity == target_sev, Handle_Type == "On-site") %>%
    count(Cluster, name = "Demand")

  capacity <- adjusters %>%
    filter(Home_Cluster != "OUT", !Is_Deployed, Skill >= min_skill) %>%
    # Exclude multi-cluster adjusters from the gap calculation.
    # These adjusters (VA, GA, TN, MD, PA, NY, AL) are shared resources —
    # they can serve multiple clusters via Eligible_Clusters, so crediting
    # their capacity to one cluster would reduce that cluster's apparent gap
    # and starve it of OUT-pool deployments it still needs.
    filter(sapply(Eligible_Clusters, length) == 1L) %>%
    mutate(Cap = sapply(Skill, function(sk) {
      cd <- get_completion_days(target_sev, sk, boosted = FALSE)
      if (is.na(cd)) 0 else (1 / cd) * work_days_factor
    })) %>%
    group_by(Cluster = Home_Cluster) %>%
    summarise(Capacity = sum(Cap, na.rm = TRUE), .groups = "drop")

  gap_table <- data.frame(Cluster = paste0("C", 0:6)) %>%
    left_join(demand, by = "Cluster") %>%
    left_join(capacity, by = "Cluster") %>%
    mutate(Demand = coalesce(Demand, 0), Capacity = coalesce(Capacity, 0),
           Gap = pmax(Demand - Capacity, 0)) %>%
    arrange(desc(Gap))

  deployable <- adjusters %>%
    filter(Home_Cluster == "OUT", !Is_Deployed, Skill >= min_skill) %>%
    arrange(desc(Skill))
  n_dep <- nrow(deployable)
  total_gap <- sum(gap_table$Gap)

  if (total_gap <= 0 || n_dep == 0)
    return(list(adjusters = adjusters, deployed = 0L))

  alloc <- gap_table %>% filter(Gap > 0) %>%
    mutate(Share = Gap / total_gap,
           Alloc_Base = floor(Share * n_dep),
           Remainder = (Share * n_dep) - Alloc_Base)
  leftover <- n_dep - sum(alloc$Alloc_Base)
  alloc <- alloc %>% arrange(desc(Remainder), desc(Gap)) %>%
    mutate(Alloc_Final = Alloc_Base + as.integer(row_number() <= leftover)) %>%
    arrange(desc(Gap))

  assigned <- rep(NA_character_, n_dep); ptr <- 1L
  for (i in seq_len(nrow(alloc))) {
    slots <- alloc$Alloc_Final[i]; if (slots == 0L) next
    for (j in seq_len(slots)) {
      if (ptr > n_dep) break
      assigned[ptr] <- alloc$Cluster[i]; ptr <- ptr + 1L
    }
  }

  lookup <- data.frame(Adjuster_Id = deployable$Adjuster_Id, AC = assigned, stringsAsFactors = FALSE)
  adjusters <- adjusters %>%
    left_join(lookup, by = "Adjuster_Id") %>%
    mutate(
      Deployed_To = coalesce(AC, Deployed_To),
      Is_Deployed = !is.na(AC) | Is_Deployed,
      Daily_Cost = ifelse(!is.na(AC), COST_DEPLOYED, Daily_Cost),
      Tour_Start = ifelse(!is.na(AC), 2L, Tour_Start),       # Day 2 start (travel)
      Status = ifelse(!is.na(AC), "Waiting", Status),
      Working_Cluster = ifelse(!is.na(AC), AC, Working_Cluster)
    ) %>%
    select(-AC)

  list(adjusters = adjusters, deployed = sum(!is.na(assigned)))
}

# Tier 1: PL4/5 for Sev-5
t1 <- deploy_tier(adjusters, claims, target_sev = 5L, min_skill = 4L, work_days_factor = 6)
adjusters <- t1$adjusters
cat(sprintf("Tier 1 Deployed (PL4/5 for Sev-5): %d\n", t1$deployed))

# Tier 2: PL3 for Sev-4
t2 <- deploy_tier(adjusters, claims, target_sev = 4L, min_skill = 3L, work_days_factor = 12)
adjusters <- t2$adjusters
cat(sprintf("Tier 2 Deployed (PL3 for Sev-4): %d\n", t2$deployed))

n_out <- sum(adjusters$Working_Cluster == "OUT")
cat(sprintf("OUT pool remaining (virtual): %d\n", n_out))
set.seed(NULL)  # Reset RNG — claims construction used seed(42), scenario runs should not inherit fixed position

# ============================================================
# INITIALIZE STATE
# ============================================================

sla_tracker <- data.frame(
  Severity = 5:1,
  Total_Claims = sapply(5:1, function(s) sum(claims$Severity == s)),
  SLA_Deadline = c(7L, 14L, 21L, 28L, 28L),
  Penalty_Rate = as.numeric(PENALTY[as.character(5:1)]),
  Completed = 0L, Missed = 0L, Pct_Complete = 0, Target_Met = FALSE)

daily_log <- data.frame(
  Day=integer(), Active_Adjusters=integer(), Gone_Adjusters=integer(),
  Claims_Completed_Today=integer(), Claims_Completed_Cumulative=integer(),
  Claims_In_Progress=integer(), Claims_Unassigned=integer(),
  Claims_Needs_Contractor=integer(),
  Sev5_Backlog=integer(), Sev4_Backlog=integer(), Sev3_Backlog=integer(),
  Sev2_Backlog=integer(), Sev1_Backlog=integer(),
  Utilization_PL5=numeric(), Utilization_PL4=numeric(), Utilization_PL3=numeric(),
  Sev5_SLA_Miss=integer(), Sev4_SLA_Miss=integer(), Sev3_SLA_Miss=integer(),
  Sev2_SLA_Miss=integer(), Sev1_SLA_Miss=integer(),
  Deploy_Cost_Daily=numeric(), Contractor_Cost_Daily=numeric(),
  Penalty_Cost_Daily=numeric(), Total_Cost_Daily=numeric(),
  Total_Cost_Cumulative=numeric())

# ============================================================
# HELPERS (unchanged)
# ============================================================

calc_priority <- function(severity, day, pct_complete) {
  deadline <- SLA_DAYS[as.character(severity)]
  pen <- PENALTY[as.character(severity)]
  days_left <- pmax(deadline - day, 0.1)
  base_p <- (1 / days_left) * pen
  mult <- ifelse(pct_complete >= 0.90, 0.5, ifelse(pct_complete < 0.70, 1.5, 1.0))
  round(base_p * mult, 2)
}

# ============================================================
# pair_pl0_boosters — pre-sim pairing of PL0 assistants with PL4/5
# Called inside run_scenario() after all modifications, before run_simulation()
# Matching rule: same Working_Cluster covers:
#   in-cluster PL4/5 <-> in-cluster PL0
#   OUT-pool PL4/5   <-> OUT-pool PL0 (unlocks all 88 OUT PL0s)
#   deployed PL4/5   <-> in-cluster PL0 at target cluster
# ============================================================

pair_pl0_boosters <- function(adjusters) {
  # Reset all pairing columns
  adjusters$Has_Boost  <- FALSE
  adjusters$Boosted_By <- NA_integer_
  adjusters$Boosting   <- NA_integer_

  pl0_pool   <- adjusters %>% filter(Skill == 0L, Status %in% c("Active", "Waiting"))
  high_skill <- adjusters %>% filter(Skill >= 4L, Status %in% c("Active", "Waiting")) %>%
    arrange(desc(Skill))  # PL5 first — they gain most from 3-day Sev-5

  pl0_used <- integer(0)

  # PASS 1: same Working_Cluster (in-cluster PL0 -> in-cluster/deployed PL4/5)
  for (i in seq_len(nrow(high_skill))) {
    adj_id <- high_skill$Adjuster_Id[i]
    wc     <- high_skill$Working_Cluster[i]

    match_idx <- which(
      !(pl0_pool$Adjuster_Id %in% pl0_used) &
      pl0_pool$Working_Cluster == wc
    )
    if (length(match_idx) == 0L) next

    pl0_id   <- pl0_pool$Adjuster_Id[match_idx[1]]
    pl0_used <- c(pl0_used, pl0_id)

    adjusters$Has_Boost[adjusters$Adjuster_Id == adj_id]  <- TRUE
    adjusters$Boosted_By[adjusters$Adjuster_Id == adj_id] <- pl0_id
    adjusters$Boosting[adjusters$Adjuster_Id == pl0_id]   <- adj_id
  }

  # PASS 2: OUT-pool PL0s pair with any remaining unmatched PL4/5 (virtual assist, any cluster)
  # After deployment, all PL4/5 have Working_Cluster = a real cluster (not "OUT"),
  # so OUT PL0s can't match in Pass 1. This pass lets them assist remotely.
  out_pl0_ids <- pl0_pool$Adjuster_Id[pl0_pool$Working_Cluster == "OUT" &
                                   !(pl0_pool$Adjuster_Id %in% pl0_used)]
  if (length(out_pl0_ids) > 0L) {
    unmatched_pl45 <- high_skill$Adjuster_Id[!(high_skill$Adjuster_Id %in%
                        adjusters$Adjuster_Id[adjusters$Has_Boost == TRUE])]
    for (adj_id in unmatched_pl45) {
      if (length(out_pl0_ids) == 0L) break
      pl0_id      <- out_pl0_ids[1]
      out_pl0_ids <- out_pl0_ids[-1]
      pl0_used    <- c(pl0_used, pl0_id)

      adjusters$Has_Boost[adjusters$Adjuster_Id == adj_id]  <- TRUE
      adjusters$Boosted_By[adjusters$Adjuster_Id == adj_id] <- pl0_id
      adjusters$Boosting[adjusters$Adjuster_Id == pl0_id]   <- adj_id
    }
  }

  paired <- sum(adjusters$Has_Boost, na.rm = TRUE)
  cat(sprintf("PL0 pairings: %d / %d PL4/5 boosted | %d / %d PL0s used\n",
              paired, nrow(high_skill), length(pl0_used), nrow(pl0_pool)))
  adjusters
}

# ============================================================
# CORE SIM FUNCTIONS (handle_tour_end, complete_claims, score, check_sla, record_log)
# ============================================================

handle_tour_end <- function(claims, adjusters, day) {
  ended_ids <- adjusters$Adjuster_Id[adjusters$Tour_End == day - 1L]
  if (length(ended_ids) == 0L)
    return(list(claims = claims, adjusters = adjusters, gone_count = 0L))

  # Strip boost from PL4/5 when their paired PL0 departs
  ended_skills <- adjusters$Skill[match(ended_ids, adjusters$Adjuster_Id)]
  pl0_departing <- ended_ids[
    ended_skills == 0L &
    !is.na(adjusters$Boosting[match(ended_ids, adjusters$Adjuster_Id)])
  ]
  for (pl0_id in pl0_departing) {
    target_id <- adjusters$Boosting[adjusters$Adjuster_Id == pl0_id]
    if (!is.na(target_id)) {
      adjusters$Has_Boost[adjusters$Adjuster_Id == target_id]  <- FALSE
      adjusters$Boosted_By[adjusters$Adjuster_Id == target_id] <- NA_integer_
      adjusters$Boosting[adjusters$Adjuster_Id == pl0_id]      <- NA_integer_
    }
  }

  # PL0s have no Current_Claim in new design; non-PL0s get tour-end extension if busy
  non_pl0_ended <- ended_ids[ended_skills != 0L]
  busy_ids <- non_pl0_ended[
    !is.na(adjusters$Current_Claim[match(non_pl0_ended, adjusters$Adjuster_Id)])
  ]
  if (length(busy_ids) > 0L) {
    for (aid in busy_ids) {
      comp_day <- claims$Completion_Day[claims$Claim_Number == adjusters$Current_Claim[adjusters$Adjuster_Id == aid]]
      if (!is.na(comp_day))
        adjusters$Tour_End[adjusters$Adjuster_Id == aid] <- as.integer(comp_day)
    }
  }
  free_ids <- ended_ids[is.na(adjusters$Current_Claim[match(ended_ids, adjusters$Adjuster_Id)])]
  if (length(free_ids) > 0L) {
    adjusters$Status[adjusters$Adjuster_Id %in% free_ids] <- "Gone"
    adjusters$Current_Claim[adjusters$Adjuster_Id %in% free_ids] <- NA_integer_
  }
  list(claims = claims, adjusters = adjusters, gone_count = length(free_ids))
}

complete_claims <- function(claims, adjusters, sla_tracker, day) {
  ready_ids <- claims$Claim_Number[claims$Status == "In Progress" &
    !is.na(claims$Completion_Day) & claims$Completion_Day <= day]
  if (length(ready_ids) == 0L)
    return(list(claims=claims, adjusters=adjusters, sla_tracker=sla_tracker, completed_count=0L))
  claims$Status[claims$Claim_Number %in% ready_ids] <- "Completed"
  claims$Day_Completed[claims$Claim_Number %in% ready_ids] <- day
  freed_ids <- claims$Assigned_To[claims$Claim_Number %in% ready_ids & !is.na(claims$Assigned_To)]
  if (length(freed_ids) > 0L)
    adjusters$Current_Claim[adjusters$Adjuster_Id %in% freed_ids] <- NA_integer_
  comp_by_sev <- claims %>% filter(Claim_Number %in% ready_ids) %>% count(Severity, name = "n")
  for (i in seq_len(nrow(comp_by_sev))) {
    idx <- which(sla_tracker$Severity == comp_by_sev$Severity[i])
    sla_tracker$Completed[idx] <- sla_tracker$Completed[idx] + comp_by_sev$n[i]
    sla_tracker$Pct_Complete[idx] <- sla_tracker$Completed[idx] / sla_tracker$Total_Claims[idx]
    sla_tracker$Target_Met[idx] <- sla_tracker$Pct_Complete[idx] >= 0.90
  }
  list(claims=claims, adjusters=adjusters, sla_tracker=sla_tracker, completed_count=length(ready_ids))
}

score_claims <- function(claims, sla_tracker, day) {
  claims <- claims %>% select(-any_of(c("Priority","Priority_Adjusted","Pct_Complete")))
  pct_lookup <- setNames(sla_tracker$Pct_Complete, sla_tracker$Severity)
  claims %>%
    mutate(Pct_Complete = pct_lookup[as.character(Severity)],
           Priority = mapply(calc_priority, Severity, day, Pct_Complete),
           Priority_Adjusted = ifelse(Status %in% c("Unassigned","Needs_Contractor"), Priority, 0)) %>%
    select(-Pct_Complete)
}

check_sla <- function(claims, sla_tracker, day) {
  sla_misses <- data.frame(Severity=integer(), Missed=integer(), Penalty=numeric())
  for (sev in 5:1) {
    dl <- sla_tracker$SLA_Deadline[sla_tracker$Severity == sev]
    pen <- sla_tracker$Penalty_Rate[sla_tracker$Severity == sev]
    if (day == dl) {
      missed <- sum(claims$Severity == sev & claims$Status != "Completed")
      sla_tracker$Missed[sla_tracker$Severity == sev] <- missed
      sla_misses <- rbind(sla_misses, data.frame(Severity=sev, Missed=missed, Penalty=missed*pen))
    }
  }
  list(sla_tracker=sla_tracker, sla_misses=sla_misses, total_penalty=sum(sla_misses$Penalty))
}

record_daily_log <- function(daily_log, claims, adjusters, sla_tracker, day, sla_misses) {
  active <- sum(adjusters$Status == "Active"); gone <- sum(adjusters$Status == "Gone")
  gb <- function(s) as.integer(sum(claims$Severity==s & claims$Status %in% c("Unassigned","In Progress","Needs_Contractor")))
  gm <- function(s) { v <- sla_misses$Missed[sla_misses$Severity==s]; if(length(v)==0L) 0L else as.integer(v[1]) }
  ut <- function(sk) {
    tot <- sum(adjusters$Status=="Active" & adjusters$Skill==sk & !adjusters$Is_Contractor)
    bsy <- sum(adjusters$Status=="Active" & adjusters$Skill==sk & !adjusters$Is_Contractor & !is.na(adjusters$Current_Claim))
    if (tot == 0L) 0 else round(bsy/tot, 3)
  }
  dep_cost <- sum(adjusters$Daily_Cost[adjusters$Status=="Active" & adjusters$Is_Deployed & !adjusters$Is_Contractor])
  ctr_cost <- sum(adjusters$Daily_Cost[adjusters$Status=="Active" & adjusters$Is_Contractor])
  pen_cost <- sum(sla_misses$Penalty)
  tot_day <- dep_cost + ctr_cost + pen_cost
  prev_cum <- if(nrow(daily_log)>0L) tail(daily_log$Total_Cost_Cumulative,1L) else 0
  rbind(daily_log, data.frame(
    Day=day, Active_Adjusters=active, Gone_Adjusters=gone,
    Claims_Completed_Today=sum(claims$Day_Completed==day, na.rm=TRUE),
    Claims_Completed_Cumulative=sum(claims$Status=="Completed"),
    Claims_In_Progress=sum(claims$Status=="In Progress"),
    Claims_Unassigned=sum(claims$Status=="Unassigned"),
    Claims_Needs_Contractor=sum(claims$Status=="Needs_Contractor"),
    Sev5_Backlog=gb(5L), Sev4_Backlog=gb(4L), Sev3_Backlog=gb(3L),
    Sev2_Backlog=gb(2L), Sev1_Backlog=gb(1L),
    Utilization_PL5=ut(5L), Utilization_PL4=ut(4L), Utilization_PL3=ut(3L),
    Sev5_SLA_Miss=gm(5L), Sev4_SLA_Miss=gm(4L), Sev3_SLA_Miss=gm(3L),
    Sev2_SLA_Miss=gm(2L), Sev1_SLA_Miss=gm(1L),
    Deploy_Cost_Daily=dep_cost, Contractor_Cost_Daily=ctr_cost,
    Penalty_Cost_Daily=pen_cost, Total_Cost_Daily=tot_day,
    Total_Cost_Cumulative=prev_cum+tot_day))
}

# ============================================================
# match_claims() — v6 COMPLETE REWRITE
# ============================================================
# PHASE 1: ON-SITE by cluster (sev 5->4->3->2)
#   - In-cluster + multi-cluster eligible adjusters ONLY
#   - Time-based cascade:
#       Day 1-7:  PL4/5 locked to Sev-5 (while backlog exists)
#       Day 8-14: PL4/5 unlock for Sev-4
#       Day 15-21: PL3 unlock for Sev-3
#       Day 22+:  everyone works anything they're skilled for
#   - High sev -> pick highest skill; Low sev -> pick lowest skill
#
# PHASE 2: VIRTUAL -> OUT pool + surplus ONLY
#   - Sev-3 virtual -> OUT PL3+, then surplus PL3+
#   - Sev-2 virtual -> OUT PL1/2, then surplus PL1/2
#   - Sev-1 virtual -> OUT PL1/2, then any OUT
#   - In-cluster adjusters NEVER do virtual
# ============================================================

match_claims <- function(claims, adjusters, day) {

  actionable <- claims %>%
    filter(Status %in% c("Unassigned","Needs_Contractor")) %>%
    arrange(desc(Priority_Adjusted))
  if (nrow(actionable) == 0L)
    return(list(claims=claims, adjusters=adjusters, assigned=0L))

  available <- adjusters %>% filter(Status == "Active", is.na(Current_Claim), Skill >= 1L)
  if (nrow(available) == 0L) {
    overdue <- actionable$Claim_Number[actionable$SLA_Deadline <= day]
    if (length(overdue) > 0)
      claims$Status[claims$Claim_Number %in% overdue & claims$Status != "Needs_Contractor"] <- "Needs_Contractor"
    return(list(claims=claims, adjusters=adjusters, assigned=0L))
  }

  max_n <- min(nrow(actionable), nrow(available))
  res_claim <- integer(max_n); res_adj <- integer(max_n); res_compd <- integer(max_n)
  n_assigned <- 0L
  adj_used <- setNames(rep(FALSE, nrow(available)), as.character(available$Adjuster_Id))

  sev5_backlog <- sum(actionable$Severity == 5L & actionable$Handle_Type == "On-site")

  # ---- PHASE 1: ON-SITE BY CLUSTER ----
  onsite_q <- actionable %>% filter(Handle_Type == "On-site")

  for (sev in c(5L, 4L, 3L, 2L)) {
    sev_claims <- onsite_q %>% filter(Severity == sev)
    if (nrow(sev_claims) == 0L) next

    for (cl_name in unique(sev_claims$Cluster)) {
      cl_claims <- sev_claims %>% filter(Cluster == cl_name) %>% arrange(desc(Priority_Adjusted))

      # Eligible: Working_Cluster matches OR state gives multi-cluster access
      cl_avail_idx <- which(
        !adj_used[as.character(available$Adjuster_Id)] &
        available$Working_Cluster != "OUT" &
        (available$Working_Cluster == cl_name |
         sapply(available$Eligible_Clusters, function(ec) cl_name %in% ec))
      )
      if (length(cl_avail_idx) == 0L) {
        overdue <- cl_claims$Claim_Number[cl_claims$SLA_Deadline <= day]
        if (length(overdue) > 0)
          claims$Status[claims$Claim_Number %in% overdue & claims$Status != "Needs_Contractor"] <- "Needs_Contractor"
        next
      }
      cl_avail <- available[cl_avail_idx, ]

      for (i in seq_len(nrow(cl_claims))) {
        claim <- cl_claims[i, ]
        eligible_ptr <- integer(0)

        if (sev == 5L) {
          # PL4/5 — PL0 boost makes PL4 viable (8d × 0.8 = 6d < 7d SLA)
          eligible_ptr <- which(!adj_used[as.character(cl_avail$Adjuster_Id)] & cl_avail$Skill >= 4L)

        } else if (sev == 4L) {
          # Per-cluster Sev-5 backlog check (not global).
          # PL4/5 locked to Sev-5 while this cluster still has backlog.
          cluster_sev5_left <- sum(
            onsite_q$Severity == 5L &
            onsite_q$Cluster == cl_name &
            !(onsite_q$Claim_Number %in% res_claim[seq_len(n_assigned)])
          )
          if (day <= 7L && cluster_sev5_left > 0L) {
            # PL3 only — PL4/5 locked to Sev-5
            eligible_ptr <- which(!adj_used[as.character(cl_avail$Adjuster_Id)] & cl_avail$Skill == 3L)
          } else {
            # This cluster's Sev-5 is clear OR Day 8+: PL3/4/5 all eligible
            eligible_ptr <- which(!adj_used[as.character(cl_avail$Adjuster_Id)] & cl_avail$Skill >= 3L)
          }

        } else if (sev == 3L) {
          # PL2+ can work Sev-3 anytime. Sev-4 pass already gave PL3 first priority.
          # If PL3 is still free after Sev-4, put them on Sev-3 immediately.
          eligible_ptr <- which(!adj_used[as.character(cl_avail$Adjuster_Id)] & cl_avail$Skill >= 2L)

        } else if (sev == 2L) {
          # PL1+ can work Sev-2 anytime. Sev-3 pass already gave PL2 first priority.
          eligible_ptr <- which(!adj_used[as.character(cl_avail$Adjuster_Id)] & cl_avail$Skill >= 1L)
        }

        if (length(eligible_ptr) == 0L) {
          if (claim$SLA_Deadline <= day)
            claims$Status[claims$Claim_Number == claim$Claim_Number] <- "Needs_Contractor"
          next
        }

        # High sev -> highest skill first; Low sev -> lowest skill first
        if (sev >= 4L) {
          best <- eligible_ptr[which.max(cl_avail$Skill[eligible_ptr])]
        } else {
          best <- eligible_ptr[which.min(cl_avail$Skill[eligible_ptr])]
        }

        adj <- cl_avail[best, ]
        raw_days <- get_completion_days(claim$Severity, adj$Skill, boosted = adj$Has_Boost)
        if (is.na(raw_days)) next

        comp_day <- day + raw_days + claim$Distance_Penalty - 1L
        n_assigned <- n_assigned + 1L
        res_claim[n_assigned] <- claim$Claim_Number
        res_adj[n_assigned] <- adj$Adjuster_Id
        res_compd[n_assigned] <- comp_day
        adj_used[as.character(adj$Adjuster_Id)] <- TRUE
        if (sev == 5L) sev5_backlog <- sev5_backlog - 1L
      }
    }
  }

  # ---- PHASE 2: VIRTUAL -> OUT + SURPLUS ONLY ----
  virtual_q <- actionable %>%
    filter(Handle_Type == "Virtual") %>%
    arrange(desc(Severity), desc(Priority_Adjusted))

  if (nrow(virtual_q) > 0L) {
    # OUT pool
    out_avail <- available %>% filter(Working_Cluster == "OUT", !adj_used[as.character(Adjuster_Id)])

    # Surplus = clusters with 0 unassigned on-site
    onsite_left <- claims %>%
      filter(Handle_Type == "On-site", Status %in% c("Unassigned","Needs_Contractor")) %>%
      count(Cluster, name = "Rem")
    surplus_cls <- setdiff(paste0("C", 0:6), onsite_left$Cluster[onsite_left$Rem > 0])
    surplus_avail <- available %>%
      filter(Working_Cluster %in% surplus_cls, Working_Cluster != "OUT",
             !adj_used[as.character(Adjuster_Id)])

    for (sev in c(3L, 2L, 1L)) {
      sev_virt <- virtual_q %>%
        filter(Severity == sev, !(Claim_Number %in% res_claim[seq_len(n_assigned)]))
      if (nrow(sev_virt) == 0L) next

      if (sev == 3L) {
        # Sev-3 virtual -> OUT PL3+, then surplus PL3+
        pool <- bind_rows(
          out_avail %>% filter(Skill >= 3L),
          surplus_avail %>% filter(Skill >= 3L)
        ) %>% filter(!adj_used[as.character(Adjuster_Id)]) %>%
          distinct(Adjuster_Id, .keep_all = TRUE) %>% arrange(desc(Skill))

      } else if (sev == 2L) {
        # Sev-2 virtual -> OUT PL1/2 first, then surplus, then PL3+
        pool <- bind_rows(
          out_avail %>% filter(Skill <= 2L),
          surplus_avail %>% filter(Skill <= 2L),
          out_avail %>% filter(Skill >= 3L),
          surplus_avail %>% filter(Skill >= 3L)
        ) %>% filter(!adj_used[as.character(Adjuster_Id)]) %>%
          distinct(Adjuster_Id, .keep_all = TRUE)

      } else {
        # Sev-1 virtual -> OUT PL1/2, then any OUT, then surplus
        pool <- bind_rows(
          out_avail %>% filter(Skill <= 2L),
          out_avail %>% filter(Skill >= 3L),
          surplus_avail
        ) %>% filter(!adj_used[as.character(Adjuster_Id)]) %>%
          distinct(Adjuster_Id, .keep_all = TRUE)
      }
      if (nrow(pool) == 0L) next

      for (i in seq_len(nrow(sev_virt))) {
        claim <- sev_virt[i, ]
        min_sk <- MIN_SKILL[as.character(claim$Severity)]
        eligible_ptr <- which(!adj_used[as.character(pool$Adjuster_Id)] & pool$Skill >= min_sk)
        if (length(eligible_ptr) == 0L) {
          if (claim$SLA_Deadline <= day)
            claims$Status[claims$Claim_Number == claim$Claim_Number] <- "Needs_Contractor"
          next
        }
        best <- eligible_ptr[which.min(pool$Skill[eligible_ptr])]
        adj <- pool[best, ]
        raw_days <- get_completion_days(claim$Severity, adj$Skill, boosted = adj$Has_Boost)
        if (is.na(raw_days)) next
        comp_day <- day + raw_days - 1L
        n_assigned <- n_assigned + 1L
        res_claim[n_assigned] <- claim$Claim_Number
        res_adj[n_assigned] <- adj$Adjuster_Id
        res_compd[n_assigned] <- comp_day
        adj_used[as.character(adj$Adjuster_Id)] <- TRUE
      }
    }
  }

  # ---- APPLY ASSIGNMENTS ----
  if (n_assigned > 0L) {
    adf <- data.frame(Claim_Number=res_claim[1:n_assigned],
                      Adjuster_Id=res_adj[1:n_assigned],
                      New_Comp_Day=res_compd[1:n_assigned])
    claims <- claims %>% left_join(adf, by="Claim_Number") %>%
      mutate(Status = ifelse(!is.na(Adjuster_Id), "In Progress", Status),
             Assigned_To = ifelse(!is.na(Adjuster_Id), Adjuster_Id, Assigned_To),
             Day_Assigned = ifelse(!is.na(Adjuster_Id), day, Day_Assigned),
             Completion_Day = ifelse(!is.na(Adjuster_Id), New_Comp_Day, Completion_Day)) %>%
      select(-any_of(c("Adjuster_Id","New_Comp_Day")))
    adjusters <- adjusters %>%
      left_join(adf %>% select(Adjuster_Id, New_Claim=Claim_Number), by="Adjuster_Id") %>%
      mutate(Current_Claim = ifelse(!is.na(New_Claim), New_Claim, Current_Claim),
             Claims_Assigned = ifelse(!is.na(New_Claim), Claims_Assigned+1L, Claims_Assigned)) %>%
      select(-New_Claim)
  }

  list(claims=claims, adjusters=adjusters, assigned=n_assigned)
}

# ============================================================
# RUN SIMULATION
# ============================================================

run_simulation <- function(claims, adjusters, sla_tracker, daily_log,
                           sim_days=45L, verbose=TRUE) {
  if (verbose) cat(sprintf("SIM: %d days | %d claims | %d adjusters\n",
                           sim_days, nrow(claims), nrow(adjusters)))
  for (day in seq_len(sim_days)) {
    te <- handle_tour_end(claims, adjusters, day)
    claims <- te$claims; adjusters <- te$adjusters
    adjusters <- adjusters %>% mutate(Status = case_when(
      Tour_End < day ~ "Gone", Status == "Gone" ~ "Gone",
      day < Tour_Start ~ "Waiting", TRUE ~ "Active"))
    cr <- complete_claims(claims, adjusters, sla_tracker, day)
    claims <- cr$claims; adjusters <- cr$adjusters; sla_tracker <- cr$sla_tracker
    claims <- score_claims(claims, sla_tracker, day)
    mr <- match_claims(claims, adjusters, day)
    claims <- mr$claims; adjusters <- mr$adjusters
    sr <- check_sla(claims, sla_tracker, day)
    sla_tracker <- sr$sla_tracker
    daily_log <- record_daily_log(daily_log, claims, adjusters, sla_tracker, day, sr$sla_misses)
    if (verbose) {
      comp <- sum(claims$Status == "Completed")
      cat(sprintf("Day %2d | Comp: %4d | Rem: %4d | Sev5: %3d | Cost: $%s\n",
                  day, comp, nrow(claims)-comp,
                  sum(claims$Severity==5L & claims$Status!="Completed"),
                  format(tail(daily_log$Total_Cost_Cumulative,1L), big.mark=",")))
    }
  }
  if (verbose) cat("SIM COMPLETE\n")
  list(claims=claims, adjusters=adjusters, sla_tracker=sla_tracker, daily_log=daily_log)
}

# ============================================================
# INFRASTRUCTURE
# ============================================================

lock_baseline <- function() {
  BASE_CLAIMS <<- claims; BASE_ADJUSTERS <<- adjusters
  BASE_SLA_TRACKER <<- sla_tracker; BASE_DAILY_LOG <<- daily_log
  cat(sprintf("\nBASELINE LOCKED: %d claims | %d adjusters\n", nrow(claims), nrow(adjusters)))
}

extract_metrics <- function(result, base_claims) {
  log <- result$daily_log; sla <- result$sla_tracker
  cl <- result$claims; adj <- result$adjusters
  gs <- function(sev, col) { v <- sla[[col]][sla$Severity==sev]; if(length(v)==0) NA else v }
  cluster_summary <- cl %>% group_by(Cluster) %>%
    summarise(Total=n(), Completed=sum(Status=="Completed"),
              Remaining=sum(Status!="Completed"),
              Sev5_Rem=sum(Severity==5L & Status!="Completed"),
              Sev4_Rem=sum(Severity==4L & Status!="Completed"),
              Sev3_Rem=sum(Severity==3L & Status!="Completed"),
              Needs_Contractor=sum(Status=="Needs_Contractor"), .groups="drop")
  adj_summary <- adj %>%
    group_by(Working_Cluster) %>%
    summarise(PL5=sum(Skill==5), PL4=sum(Skill==4), PL3=sum(Skill==3),
              PL2=sum(Skill==2), PL1=sum(Skill==1), PL0=sum(Skill==0),
              Total=n(), .groups="drop")
  list(
    sev5_completed=gs(5,"Completed"), sev5_missed=gs(5,"Missed"),
    sev5_pct=round(gs(5,"Pct_Complete"),4), sev5_target_met=gs(5,"Target_Met"),
    sev4_completed=gs(4,"Completed"), sev4_missed=gs(4,"Missed"),
    sev4_pct=round(gs(4,"Pct_Complete"),4), sev4_target_met=gs(4,"Target_Met"),
    sev3_completed=gs(3,"Completed"), sev3_missed=gs(3,"Missed"),
    sev3_pct=round(gs(3,"Pct_Complete"),4),
    sev2_completed=gs(2,"Completed"), sev2_missed=gs(2,"Missed"),
    sev2_pct=round(gs(2,"Pct_Complete"),4),
    sev1_completed=gs(1,"Completed"), sev1_missed=gs(1,"Missed"),
    sev1_pct=round(gs(1,"Pct_Complete"),4),
    total_completed=sum(cl$Status=="Completed"),
    total_remaining=sum(cl$Status!="Completed"),
    pct_completed=round(sum(cl$Status=="Completed")/nrow(base_claims),4),
    needs_contractor=sum(cl$Status=="Needs_Contractor"),
    total_cost=tail(log$Total_Cost_Cumulative,1L),
    deploy_cost=sum(log$Deploy_Cost_Daily),
    contractor_cost=sum(log$Contractor_Cost_Daily),
    penalty_cost=sum(log$Penalty_Cost_Daily),
    peak_sev5_backlog=max(log$Sev5_Backlog),
    peak_sev4_backlog=max(log$Sev4_Backlog),
    max_gone_day=log$Day[which.max(log$Gone_Adjusters)],
    max_gone_count=max(log$Gone_Adjusters),
    cluster_summary=cluster_summary, adj_summary=adj_summary,
    daily_log=log, sla_tracker=sla, claims=cl, adjusters=result$adjusters)
}

query_assignments <- function(result, cluster=NULL, severity=NULL, max_rows=30) {
  cl <- result$claims; adj <- result$adjusters
  active <- cl %>% filter(Status == "In Progress")
  if (!is.null(cluster))  active <- active %>% filter(Cluster == cluster)
  if (!is.null(severity)) active <- active %>% filter(Severity == severity)
  if (nrow(active) == 0L) return(data.frame(Message="No matching assignments"))
  active %>%
    left_join(adj %>% select(Adjuster_Id, Skill, Working_Cluster, Is_Contractor, Home_Cluster),
              by=c("Assigned_To"="Adjuster_Id")) %>%
    select(Claim_Number, Severity, Cluster, Handle_Type, Assigned_To, Skill,
           Is_Contractor, Home_Cluster, Day_Assigned, Completion_Day) %>%
    arrange(desc(Severity), Cluster) %>% head(max_rows)
}

# ============================================================
# run_scenario — v6 (contractor tour_length default 8)
# ============================================================

run_scenario <- function(params=list()) {
  p <- params
  sim_days <- as.integer(p$sim_days %||% 45L)
  claims <- BASE_CLAIMS; adjusters <- BASE_ADJUSTERS
  sla_tracker <- BASE_SLA_TRACKER; daily_log <- BASE_DAILY_LOG

  ctr_cost <- as.numeric(p$cost_overrides$contractor %||% COST_CONTRACTOR)
  dep_cost_rate <- as.numeric(p$cost_overrides$deployed %||% COST_DEPLOYED)
  lcl_cost <- as.numeric(p$cost_overrides$local %||% COST_LOCAL)
  if (!is.null(p$cost_overrides)) {
    adjusters$Daily_Cost[adjusters$Is_Deployed & !adjusters$Is_Contractor] <- dep_cost_rate
    adjusters$Daily_Cost[!adjusters$Is_Deployed & !adjusters$Is_Contractor] <- lcl_cost
  }

  # 1. CONTRACTORS — tour_length default 8 days
  # Contractors arrive Day 1 (pre-staged/pre-credentialed). Deployed staff arrive Day 2 (travel).
  if (length(p$contractors) > 0) {
    max_id <- max(adjusters$Adjuster_Id)
    for (grp in p$contractors) {
      n <- as.integer(grp$n %||% 0); cl <- as.character(grp$cluster %||% "C4")
      sk <- as.integer(grp$skill %||% 5); start <- as.integer(grp$start %||% 1)
      tour_len <- as.integer(grp$tour_length %||% DEFAULT_CONTRACTOR_TOUR)
      if (n <= 0) next
      new_adj <- data.frame(
        Adjuster_Id=max_id+seq_len(n), Skill=sk, Location="Contractor", Tour_Length=tour_len,
        State=NA_character_, Home_Cluster=cl,
        Eligible_Clusters=I(replicate(n, cl, simplify=FALSE)),
        Tour_Start=start, Tour_End=as.integer(start+tour_len),
        Deployed_To=NA_character_, Is_Deployed=FALSE, Is_Contractor=TRUE,
        Daily_Cost=ctr_cost, Status=ifelse(start<=1L,"Active","Waiting"),
        Current_Claim=NA_integer_, Claims_Assigned=0L, Working_Cluster=cl)
      adjusters <- bind_rows(adjusters, new_adj)
      max_id <- max(adjusters$Adjuster_Id)
    }
  }

  # 2. NEW CLAIMS
  if (length(p$new_claims) > 0) {
    set.seed(99L)
    for (batch in p$new_claims) {
      n <- as.integer(batch$n %||% 0); sev <- as.integer(batch$severity %||% 5)
      cl <- as.character(batch$cluster %||% "C4")
      if (n <= 0) next
      ht <- case_when(sev>=4L~"On-site", sev==3L~sample(c("On-site","Virtual"),n,TRUE,c(0.75,0.25)),
                       sev==2L~sample(c("On-site","Virtual"),n,TRUE,c(0.50,0.50)), TRUE~"Virtual")
      new_cl <- data.frame(
        Claim_Number=max(claims$Claim_Number)+seq_len(n), Severity=sev, Cluster=cl,
        Distance_km=150, State="TN", Handle_Type=ht, Distance_Penalty=0L,
        SLA_Deadline=as.integer(SLA_DAYS[as.character(sev)]),
        Status="Unassigned", Assigned_To=NA_integer_,
        Day_Assigned=NA_integer_, Day_Completed=NA_integer_,
        Completion_Day=NA_integer_, Priority=0, Priority_Adjusted=0)
      claims <- bind_rows(claims, new_cl)
      sla_tracker$Total_Claims[sla_tracker$Severity==sev] <-
        sla_tracker$Total_Claims[sla_tracker$Severity==sev]+n
    }
  }

  # 3. TOUR EXTENSIONS
  if (length(p$tour_extensions) > 0) {
    already_ext <- integer(0)
    for (grp in p$tour_extensions) {
      n <- as.integer(grp$n %||% 0); days <- as.integer(grp$days %||% 0)
      sk <- as.integer(grp$skill %||% 4)
      if (n<=0 || days<=0) next
      ext_ids <- adjusters %>% filter(Skill>=sk, !Is_Contractor, !(Adjuster_Id %in% already_ext)) %>%
        arrange(desc(Skill), Tour_End) %>% head(n) %>% pull(Adjuster_Id)
      adjusters$Tour_End[adjusters$Adjuster_Id %in% ext_ids] <-
        adjusters$Tour_End[adjusters$Adjuster_Id %in% ext_ids]+days
      already_ext <- c(already_ext, ext_ids)
    }
  }

  # 4. REDEPLOYMENTS
  if (length(p$redeployments) > 0) {
    already_moved <- integer(0)
    for (mv in p$redeployments) {
      n <- as.integer(mv$n %||% 0); from <- as.character(mv$from %||% "C0")
      to <- as.character(mv$to %||% "C0"); sk <- as.integer(mv$skill %||% 3)
      if (n<=0 || from==to) next
      red_ids <- adjusters %>%
        filter(Working_Cluster==from, Skill>=sk, !Is_Contractor, !(Adjuster_Id %in% already_moved)) %>%
        head(n) %>% pull(Adjuster_Id)
      adjusters$Working_Cluster[adjusters$Adjuster_Id %in% red_ids] <- to
      adjusters$Deployed_To[adjusters$Adjuster_Id %in% red_ids] <- to
      adjusters$Is_Deployed[adjusters$Adjuster_Id %in% red_ids] <- TRUE
      adjusters$Daily_Cost[adjusters$Adjuster_Id %in% red_ids] <- dep_cost_rate
      already_moved <- c(already_moved, red_ids)
    }
  }

  # 5. REMOVALS
  if (length(p$removals) > 0) {
    for (rm_grp in p$removals) {
      n <- as.integer(rm_grp$n %||% 0); sk <- as.integer(rm_grp$skill %||% 1)
      cl <- rm_grp$cluster; if (n<=0) next
      rem <- adjusters %>% filter(Skill>=sk, !Is_Contractor)
      if (!is.null(cl)) rem <- rem %>% filter(Working_Cluster==cl)
      rem_ids <- head(rem, n) %>% pull(Adjuster_Id)
      adjusters <- adjusters %>% filter(!(Adjuster_Id %in% rem_ids))
    }
  }

  # Pair PL0 boosters — runs after all scenario modifications so contractors
  # and redeployed adjusters are included in the pairing pass
  adjusters <- pair_pl0_boosters(adjusters)

  verbose <- as.logical(p$verbose %||% FALSE)
  result <- run_simulation(claims, adjusters, sla_tracker, daily_log, sim_days=sim_days, verbose=verbose)
  metrics <- extract_metrics(result, BASE_CLAIMS)
  metrics$params <- p
  metrics
}

# ============================================================
# compare_strategies / compare_scenarios
# ============================================================

compare_strategies <- function(strategies, baseline) {
  results <- list()
  for (i in seq_along(strategies)) {
    s <- strategies[[i]]; label <- s$label %||% sprintf("Strategy %d",i); s$label <- NULL
    cat(sprintf("\nRunning: %s\n", label))
    res <- run_scenario(s); compare_scenarios(baseline, res, label); results[[label]] <- res
  }
  invisible(results)
}

compare_scenarios <- function(baseline, scenario, label="Scenario") {
  d5 <- baseline$sev5_missed-scenario$sev5_missed
  d4 <- baseline$sev4_missed-scenario$sev4_missed
  d3 <- baseline$sev3_missed-scenario$sev3_missed
  d2 <- baseline$sev2_missed-scenario$sev2_missed
  d1 <- baseline$sev1_missed-scenario$sev1_missed
  saved <- d5*PENALTY["5"]+d4*PENALTY["4"]+d3*PENALTY["3"]+d2*PENALTY["2"]+d1*PENALTY["1"]
  extra_ops <- (scenario$deploy_cost + (scenario$contractor_cost %||% 0)) -
               (baseline$deploy_cost + (baseline$contractor_cost %||% 0))
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("  Sev-5: %d -> %d (%+d)\n", baseline$sev5_missed, scenario$sev5_missed, -d5))
  cat(sprintf("  Sev-4: %d -> %d (%+d)\n", baseline$sev4_missed, scenario$sev4_missed, -d4))
  cat(sprintf("  Sev-3: %d -> %d (%+d)\n", baseline$sev3_missed, scenario$sev3_missed, -d3))
  cat(sprintf("  Sev-2: %d -> %d (%+d)\n", baseline$sev2_missed, scenario$sev2_missed, -d2))
  cat(sprintf("  Sev-1: %d -> %d (%+d)\n", baseline$sev1_missed, scenario$sev1_missed, -d1))
  cat(sprintf("  Penalty saved: $%s | Extra ops: $%s | NET: $%s\n",
              format(saved,big.mark=","), format(extra_ops,big.mark=","),
              format(saved-extra_ops,big.mark=",")))
  invisible(list(delta_sev5=d5,delta_sev4=d4,delta_sev3=d3,delta_sev2=d2,delta_sev1=d1,
                 penalty_saved=saved,extra_ops=extra_ops,net_benefit=saved-extra_ops))
}

# ============================================================
# SANITY CHECK
# ============================================================

cat("\n--- SANITY CHECK ---\n")
test_adj <- data.frame(
  Adjuster_Id=1:5, Skill=5L, Location="Test", Tour_Length=45L,
  State="MD", Home_Cluster="C4",
  Eligible_Clusters=I(replicate(5, c("C2","C4"), simplify=FALSE)),
  Tour_Start=1L, Tour_End=46L,
  Deployed_To=NA_character_, Is_Deployed=TRUE, Is_Contractor=FALSE,
  Daily_Cost=COST_DEPLOYED, Status="Active",
  Current_Claim=NA_integer_, Claims_Assigned=0L, Working_Cluster="C4",
  Has_Boost=FALSE, Boosted_By=NA_integer_, Boosting=NA_integer_)
test_cl <- data.frame(
  Claim_Number=101:105, Severity=5L, Cluster="C4", Distance_km=100, State="MD",
  Handle_Type="On-site", Distance_Penalty=0L, SLA_Deadline=7L,
  Status="Unassigned", Assigned_To=NA_integer_,
  Day_Assigned=NA_integer_, Day_Completed=NA_integer_,
  Completion_Day=NA_integer_, Priority=1000, Priority_Adjusted=1000)
test_sla <- data.frame(
  Severity=5:1, Total_Claims=c(5L,0L,0L,0L,0L),
  SLA_Deadline=c(7L,14L,21L,28L,28L), Penalty_Rate=as.numeric(PENALTY[as.character(5:1)]),
  Completed=0L, Missed=0L, Pct_Complete=0, Target_Met=FALSE)
test_result <- run_simulation(test_cl, test_adj, test_sla, daily_log, sim_days=5L, verbose=FALSE)
n_comp <- sum(test_result$claims$Status=="Completed")
cat(sprintf("  5 PL5 vs 5 Sev-5: %d/5 | Days: %s | %s\n",
            n_comp, paste(test_result$claims$Day_Completed,collapse=","),
            ifelse(n_comp==5L && all(test_result$claims$Day_Completed==4L,na.rm=TRUE),"PASS","FAIL")))

# ============================================================
# BASELINE
# ============================================================

# Pair PL0 boosters now (all functions defined) so BASE_ADJUSTERS captures pairings
adjusters <- pair_pl0_boosters(adjusters)
lock_baseline()
cat("\nRunning baseline...\n")
baseline_result <- run_scenario(list(verbose=TRUE, sim_days=45L))

cat("\n--- BASELINE RESULTS ---\n")
print(baseline_result$sla_tracker %>% select(Severity,Total_Claims,Completed,Missed,Pct_Complete,Target_Met))
cat(sprintf("Completed: %d | Remaining: %d | Needs contractor: %d\n",
            baseline_result$total_completed, baseline_result$total_remaining, baseline_result$needs_contractor))
cat(sprintf("Cost: Deploy=$%s Contractor=$%s Penalty=$%s TOTAL=$%s\n",
            format(baseline_result$deploy_cost,big.mark=","),
            format(baseline_result$contractor_cost,big.mark=","),
            format(baseline_result$penalty_cost,big.mark=","),
            format(baseline_result$total_cost,big.mark=",")))
cat("\n--- CLUSTER SUMMARY ---\n")
print(baseline_result$cluster_summary)
cat("\n--- ADJUSTER SUMMARY ---\n")
print(baseline_result$adj_summary)

# ============================================================
# SCENARIO TESTS
# ============================================================

cat("\n--- REPRO CHECK ---\n")
r1 <- run_scenario(list(sim_days=45L)); r2 <- run_scenario(list(sim_days=45L))
cat(sprintf("  %s\n", ifelse(r1$sev5_missed==r2$sev5_missed,"PASS","FAIL")))

cat("\n--- 20 PL5 CONTRACTORS C3 (8-day tour) ---\n")
r_ctr <- run_scenario(list(contractors=list(list(n=20,cluster="C3",skill=5))))
compare_scenarios(baseline_result, r_ctr, "20 PL5 Contractors C3")

cat("\n--- MULTI-CLUSTER: 20 C3 + 15 C4 ---\n")
r_multi <- run_scenario(list(contractors=list(
  list(n=20,cluster="C3",skill=5), list(n=15,cluster="C4",skill=5))))
compare_scenarios(baseline_result, r_multi, "20 C3 + 15 C4")

cat("\n--- TOUR EXTEND 30 PL4+ +7d ---\n")
r_tour <- run_scenario(list(tour_extensions=list(list(n=30,days=7,skill=4))))
compare_scenarios(baseline_result, r_tour, "Tour Extension +7d")

cat("\n--- COMPARE ---\n")
compare_strategies(list(
  list(label="Contractors Only", contractors=list(list(n=20,cluster="C3",skill=5))),
  list(label="Tour Ext Only", tour_extensions=list(list(n=30,days=7,skill=4)))
), baseline_result)

cat("\n=== ENGINE v6 READY ===\n")
