# ============================================================
# noaa_monitor.R — V2
# NOAA Weather Alert Polling + Claim Estimate Lookup
# ============================================================
# Changes from V1:
#   1. Expanded NOAA_EVENT_MAP to cover tornado, hurricane, hail, flood
#   2. Replaced flat REFERENCE_TABLE with CLAIM_PROFILES (storm × cluster)
#   3. Added REPORTING_CURVES per cluster
#   4. Added REPORTING_BY_PERIL per peril type
#   5. Added STORM_TIMING for geographic progression
#   6. Added SEVERITY_BY_PERIL for peril-specific Sev-5 rates
#   7. Added HOTSPOTS and IMPUTATION_WARNINGS
#   8. Added SEVERITY_ESCALATION pattern
#   9. Rewrote format_weather_alert() to include full operational context
#  10. Added 4 mock scenarios for demo mode
#  11. Added get_cluster_summary() helper
# ============================================================
# Polls NOAA alerts API for active warnings in cluster states.
# Maps alerts to clusters. Returns estimated claim impact
# using CAT Code 82 as the historical reference anchor.
# No API key required. Rate limit: ~5 req/sec (polled every 5 min).
# ============================================================

library(httr2)
library(jsonlite)

# ============================================================
# CLUSTER STATE MAPPING
# ============================================================

CLUSTER_STATES <- list(
  C0 = c("NC", "SC"),
  C1 = c("AL", "GA"),
  C2 = c("PA", "NJ", "NY"),
  C3 = c("TN", "KY"),
  C4 = c("MD", "VA", "DC", "DE"),
  C5 = c("MS", "LA"),
  C6 = c("CT", "MA", "NH", "VT", "ME", "RI")
)

# Reverse lookup: state -> primary cluster
STATE_TO_PRIMARY <- setNames(
  rep(names(CLUSTER_STATES), sapply(CLUSTER_STATES, length)),
  unlist(CLUSTER_STATES)
)

# ============================================================
# EXPANDED NOAA EVENT MAP
# ============================================================
# V1 only had winter storms. Now covers all major peril types.
# Maps NOAA alert event names to our storm type categories.
# ============================================================

NOAA_EVENT_MAP <- c(
  # Winter
  "Winter Storm Warning"        = "Winter Storm",
  "Winter Storm Watch"          = "Winter Storm",
  "Blizzard Warning"            = "Winter Storm",
  "Ice Storm Warning"           = "Winter Storm",
  "Winter Weather Advisory"     = "Winter Storm",

  # Tornado
  "Tornado Warning"             = "Tornado",
  "Tornado Watch"               = "Tornado",

  # Hurricane / Tropical
  "Hurricane Warning"           = "Hurricane",
  "Hurricane Watch"             = "Hurricane",
  "Tropical Storm Warning"      = "Hurricane",
  "Tropical Storm Watch"        = "Hurricane",
  "Hurricane Local Statement"   = "Hurricane",

  # Severe Thunderstorm (wind + hail)
  "Severe Thunderstorm Warning" = "Severe Storm",
  "Severe Thunderstorm Watch"   = "Severe Storm",

  # Flood
  "Flash Flood Warning"         = "Flood",
  "Flash Flood Watch"           = "Flood",
  "Flood Warning"               = "Flood",
  "Flood Watch"                 = "Flood",

  # Wind
  "High Wind Warning"           = "Wind",
  "High Wind Watch"             = "Wind",
  "Wind Advisory"               = "Wind"
)

# ============================================================
# CLAIM PROFILES — STORM TYPE × CLUSTER
# ============================================================
# From CAT Code 82 actual data. Every number verified.
# storm_type: matches the peril breakdown from claims data
# ref_claims: count of claims for that peril in that cluster
# ref_sev5:   Sev-5 count for that peril in that cluster
# pct_sev5:   Sev-5 as % of claims for that peril
#
# For "Winter Storm" and "Severe Storm" NOAA alerts, we combine
# tornado + wind + hail profiles and let the AI weight them.
# ============================================================

CLAIM_PROFILES <- data.frame(
  storm_type = c(
    # Tornado claims by cluster
    rep("Tornado", 7),
    # Wind claims by cluster
    rep("Wind", 7),
    # Hail claims by cluster
    rep("Hail", 7),
    # Hurricane claims by cluster
    rep("Hurricane", 7)
  ),
  cluster = rep(paste0("C", 0:6), 4),
  ref_claims = c(
    # Tornado: C0-C6
    58, 114, 40, 325, 76, 5, 35,
    # Wind: C0-C6
    29, 20, 84, 12, 103, 2, 61,
    # Hail: C0-C6
    2, 11, 0, 9, 4, 45, 1,
    # Hurricane: C0-C6
    9, 0, 2, 6, 3, 0, 4
  ),
  ref_sev5 = c(
    # Tornado Sev-5: C0-C6
    3, 18, 6, 45, 7, 1, 2,
    # Wind Sev-5: C0-C6
    24, 5, 49, 2, 96, 0, 14,
    # Hail Sev-5: C0-C6
    0, 2, 0, 0, 0, 1, 0,
    # Hurricane Sev-5: C0-C6
    2, 0, 0, 0, 0, 0, 0
  ),
  ref_event = rep("CAT Code 82 (Dec 2023)", 28),
  stringsAsFactors = FALSE
)

# Add pct_sev5
CLAIM_PROFILES$pct_sev5 <- ifelse(
  CLAIM_PROFILES$ref_claims > 0,
  round(CLAIM_PROFILES$ref_sev5 / CLAIM_PROFILES$ref_claims * 100, 1),
  0
)

# ============================================================
# SEVERITY BY PERIL TYPE (overall, not per-cluster)
# ============================================================
# For the AI to explain WHY it's recommending high-skill vs volume

SEVERITY_BY_PERIL <- list(
  Tornado = list(
    pct_sev5 = 12.6,
    pct_sev3 = 52.1,
    note = "Tornado claims are mostly Sev-3 (structural). Deploy for volume, not high-skill."
  ),
  Wind = list(
    pct_sev5 = 61.1,
    pct_sev3 = 4.2,
    note = "Wind claims are 61% Sev-5. Deploy HIGH-SKILL adjusters immediately."
  ),
  Hail = list(
    pct_sev5 = 4.2,
    pct_sev3 = 65.3,
    note = "Hail claims are mostly Sev-3. Low severity but report very slowly (7-day avg lag)."
  ),
  Hurricane = list(
    pct_sev5 = 8.3,
    pct_sev3 = 50.0,
    note = "Hurricane claims are moderate severity. Mix of wind and water damage."
  )
)

# ============================================================
# REPORTING CURVES BY CLUSTER
# ============================================================
# % of claims reported by Day 1, Day 3, Day 7
# Critical for deployment timing recommendations

REPORTING_CURVES <- data.frame(
  cluster     = paste0("C", 0:6),
  pct_day1    = c(49, 48, 60, 37, 59, 4, 59),
  pct_day3    = c(71, 74, 80, 73, 75, 29, 84),
  pct_day7    = c(82, 85, 92, 87, 88, 56, 91),
  mean_lag    = c(4.4, 3.3, 2.4, 3.4, 2.8, 7.5, 2.3),
  stringsAsFactors = FALSE
)

# ============================================================
# REPORTING CURVES BY PERIL TYPE
# ============================================================

REPORTING_BY_PERIL <- data.frame(
  storm_type  = c("Tornado", "Wind", "Hail", "Hurricane", "Flood"),
  pct_day1    = c(44, 61, 12, 33, 20),
  pct_day3    = c(73, 81, 38, 75, 100),
  mean_lag    = c(3.4, 2.3, 7.0, 3.7, 1.8),
  stringsAsFactors = FALSE
)

# ============================================================
# STORM GEOGRAPHIC TIMING
# ============================================================
# Based on CAT 82: storm hit southern clusters first,
# moved northeast over 3 days.
# peak_day:  which day of the 3-day event saw the most claims
# hit_order: 1 = first hit, 2 = second wave, 3 = tail end

STORM_TIMING <- data.frame(
  cluster     = paste0("C", 0:6),
  peak_date   = c("Dec 10", "Dec 9-10", "Dec 10", "Dec 9", "Dec 10", "Dec 9", "Dec 10-11"),
  hit_order   = c(2, 1, 2, 1, 2, 1, 3),
  peak_claims = c(77, 74, 61, 340, 115, 52, 47),
  note = c(
    "Second wave — follows C1/C3 by 24hrs",
    "First wave — simultaneous with C3/C5",
    "Second wave — northeast impact Day 2",
    "FIRST HIT — 96% of claims on Day 1. Clarksville TN is epicenter (130 claims)",
    "Second wave — 62% of claims on Day 2. Maryland/Virginia corridor",
    "First wave — all 52 claims Day 1. But only 4% reported Day 1.",
    "Tail end — claims spread over Day 2-3"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# CONCENTRATION HOTSPOTS
# ============================================================
# Where claims actually cluster within each region

HOTSPOTS <- list(
  C0 = "Charlotte NC (12 claims)",
  C1 = "Birmingham AL (28), Mountain Brook (10)",
  C2 = "Philadelphia PA (7) — spread across PA/NJ/NY",
  C3 = "Clarksville TN (130 claims, 37% of cluster). Gallatin (73), Hendersonville (59), Madison (42)",
  C4 = "Richmond VA (12), Baltimore MD (8), Waldorf MD (6) — spread across MD/VA",
  C5 = "Clarksdale MS (13), Yazoo City MS (9) — rural, slow reporting",
  C6 = "Norwich CT (4) — evenly spread across CT/MA"
)

# ============================================================
# IMPUTATION WARNINGS
# ============================================================
# Clusters where severity data is unreliable

IMPUTATION_WARNINGS <- list(
  C0 = "57% of claims have imputed severity. Sev-5 count may be overstated.",
  C4 = paste0(
    "69% of claims have imputed severity. ",
    "66% of Sev-5 claims (68 of 103) are imputed — all water damage. ",
    "Observed water damage is 46.6% Sev-5 across all clusters. ",
    "True C4 Sev-5 may be 35-103. Deploy for the upper estimate but acknowledge uncertainty."
  )
)

# ============================================================
# SEVERITY ESCALATION PATTERN
# ============================================================
# Claims reported later tend to be higher severity

SEVERITY_ESCALATION <- paste0(
  "SEVERITY ESCALATION: Based on CAT 82, early reports undercount Sev-5. ",
  "Day 1: 14% Sev-5. Day 2: 33% Sev-5. Day 3: 42% Sev-5. ",
  "Do not base high-skill deployment on Day 1 severity mix alone."
)

# ============================================================
# fetch_noaa_alerts()
# ============================================================
# Calls NOAA alerts API for all cluster states.
# Returns a data frame of active alerts with:
#   event, storm_type, states, clusters,
#   headline, severity, urgency
# Returns empty data frame if no relevant alerts.
# ============================================================

fetch_noaa_alerts <- function() {

  all_states <- unique(unlist(CLUSTER_STATES))
  state_str  <- paste(all_states, collapse = ",")

  resp <- tryCatch({
    request(paste0("https://api.weather.gov/alerts/active?area=", state_str)) |>
      req_headers(`User-Agent` = "(DS7900 Capstone, etomiwa80@gmail.com)") |>
      req_timeout(10) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    message("NOAA API error: ", e$message)
    return(NULL)
  })

  empty_df <- data.frame(
    event      = character(0), storm_type = character(0),
    states     = character(0), clusters   = character(0),
    headline   = character(0), severity   = character(0),
    urgency    = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(resp) || length(resp$features) == 0) return(empty_df)

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  alerts <- lapply(resp$features, function(f) {
    props      <- f$properties
    event      <- props$event %||% ""
    storm_type <- NOAA_EVENT_MAP[event]
    if (is.na(storm_type)) return(NULL)

    # Extract state codes from UGC geocode
    state_codes <- tryCatch({
      geo <- props$geocode
      if (!is.null(geo$UGC)) unique(substr(geo$UGC, 1, 2))
      else character(0)
    }, error = function(e) character(0))

    # Fallback: parse two-letter codes from areaDesc
    if (length(state_codes) == 0 && !is.null(props$areaDesc)) {
      area        <- props$areaDesc
      state_codes <- intersect(
        unique(unlist(CLUSTER_STATES)),
        unlist(regmatches(area, gregexpr("\\b[A-Z]{2}\\b", area)))
      )
    }

    if (length(state_codes) == 0) return(NULL)

    affected_clusters <- unique(na.omit(STATE_TO_PRIMARY[state_codes]))
    if (length(affected_clusters) == 0) return(NULL)

    data.frame(
      event       = event,
      storm_type  = as.character(storm_type),
      states      = paste(state_codes,       collapse = ", "),
      clusters    = paste(affected_clusters, collapse = ", "),
      headline    = props$headline %||% "",
      severity    = props$severity %||% "",
      urgency     = props$urgency  %||% "",
      stringsAsFactors = FALSE
    )
  })

  alerts <- do.call(rbind, Filter(Negate(is.null), alerts))

  if (is.null(alerts) || nrow(alerts) == 0) return(empty_df)

  # Deduplicate by cluster — keep most severe
  severity_rank           <- c("Extreme" = 4, "Severe" = 3, "Moderate" = 2, "Minor" = 1)
  alerts$sev_rank         <- severity_rank[alerts$severity]
  alerts$sev_rank[is.na(alerts$sev_rank)] <- 0
  alerts                  <- alerts[order(-alerts$sev_rank), ]
  alerts                  <- alerts[!duplicated(alerts$clusters), ]
  alerts$sev_rank         <- NULL

  alerts
}

# ============================================================
# format_weather_alert()
# ============================================================
# MAJOR REWRITE from V1.
# Now includes: claim profile, severity context, reporting
# curve, storm timing, hotspot info, imputation warnings,
# and severity escalation pattern.
# ============================================================

format_weather_alert <- function(alert_row) {

  affected_clusters <- strsplit(alert_row$clusters, ", ")[[1]]
  storm <- alert_row$storm_type

  # Map NOAA storm type to claim profile peril(s)
  # "Winter Storm" and "Severe Storm" produce mixed perils
  peril_types <- switch(storm,
    "Tornado"       = "Tornado",
    "Wind"          = "Wind",
    "Hurricane"     = c("Hurricane", "Wind"),
    "Severe Storm"  = c("Tornado", "Wind", "Hail"),
    "Winter Storm"  = c("Tornado", "Wind"),
    "Flood"         = "Wind",  # Flood claims often coded as wind/water in CAT 82
    "Wind"          # default
  )

  sections <- character(0)

  # --- Header ---
  sections <- c(sections, paste0(
    "ACTIVE WEATHER ALERT\n",
    "Event: ", alert_row$event, "\n",
    "NOAA Severity: ", alert_row$severity, "\n",
    "Affected states: ", alert_row$states, "\n",
    "Mapped clusters: ", alert_row$clusters, "\n",
    "Headline: ", alert_row$headline, "\n"
  ))

  # --- Per-cluster profiles ---
  for (cl in affected_clusters) {

    cl_section <- paste0("\n--- ", cl, " PROFILE ---\n")

    # Claim counts by peril type
    cl_profiles <- CLAIM_PROFILES[
      CLAIM_PROFILES$cluster == cl & CLAIM_PROFILES$storm_type %in% peril_types, ]

    if (nrow(cl_profiles) > 0) {
      total_claims <- sum(cl_profiles$ref_claims)
      total_sev5   <- sum(cl_profiles$ref_sev5)

      cl_section <- paste0(cl_section,
        "Expected claims (based on CAT 82): ", total_claims, " total, ",
        total_sev5, " Sev-5\n"
      )

      # Breakdown by peril if multiple types
      if (nrow(cl_profiles) > 1) {
        for (i in seq_len(nrow(cl_profiles))) {
          p <- cl_profiles[i, ]
          if (p$ref_claims > 0) {
            cl_section <- paste0(cl_section,
              "  ", p$storm_type, ": ", p$ref_claims, " claims, ",
              p$ref_sev5, " Sev-5 (", p$pct_sev5, "%)\n"
            )
          }
        }
      }
    }

    # Severity context for dominant peril
    primary_peril <- peril_types[1]
    if (primary_peril %in% names(SEVERITY_BY_PERIL)) {
      sev_info <- SEVERITY_BY_PERIL[[primary_peril]]
      cl_section <- paste0(cl_section,
        "Severity profile: ", sev_info$note, "\n"
      )
    }

    # Reporting curve
    rc <- REPORTING_CURVES[REPORTING_CURVES$cluster == cl, ]
    if (nrow(rc) > 0) {
      cl_section <- paste0(cl_section,
        "Reporting speed: ", rc$pct_day1, "% within 24hrs, ",
        rc$pct_day3, "% within 3 days. Mean lag: ", rc$mean_lag, " days.\n"
      )
    }

    # Peril-level reporting warning for hail
    if ("Hail" %in% peril_types) {
      rp <- REPORTING_BY_PERIL[REPORTING_BY_PERIL$storm_type == "Hail", ]
      if (nrow(rp) > 0) {
        cl_section <- paste0(cl_section,
          "WARNING — Hail reporting is very slow: only ", rp$pct_day1,
          "% within 24hrs, mean lag ", rp$mean_lag, " days. ",
          "Do NOT deploy full capacity Day 1. Stagger over 7 days.\n"
        )
      }
    }

    # Storm timing
    st <- STORM_TIMING[STORM_TIMING$cluster == cl, ]
    if (nrow(st) > 0) {
      cl_section <- paste0(cl_section,
        "Storm timing (CAT 82 pattern): Hit order ", st$hit_order,
        " — ", st$note, "\n"
      )
    }

    # Hotspot
    if (cl %in% names(HOTSPOTS)) {
      cl_section <- paste0(cl_section,
        "Concentration: ", HOTSPOTS[[cl]], "\n"
      )
    }

    # Imputation warning
    if (cl %in% names(IMPUTATION_WARNINGS)) {
      cl_section <- paste0(cl_section,
        "DATA NOTE: ", IMPUTATION_WARNINGS[[cl]], "\n"
      )
    }

    sections <- c(sections, cl_section)
  }

  # --- Severity escalation (applies to all alerts) ---
  sections <- c(sections, paste0("\n", SEVERITY_ESCALATION, "\n"))

  # --- Multi-cluster storm tracking ---
  if (length(affected_clusters) > 1) {
    timing_rows <- STORM_TIMING[STORM_TIMING$cluster %in% affected_clusters, ]
    timing_rows <- timing_rows[order(timing_rows$hit_order), ]
    order_text  <- paste0(timing_rows$cluster, " (order ", timing_rows$hit_order, ")",
                          collapse = " \u2192 ")
    sections <- c(sections, paste0(
      "\nSTORM TRACKING: Based on CAT 82, expected hit order: ",
      order_text, "\n",
      "Pre-position for earliest cluster. Stage deployment for later clusters.\n"
    ))
  }

  # --- Instructions ---
  sections <- c(sections, paste0(
    "\nRECOMMENDATION INSTRUCTIONS:\n",
    "1. For each affected cluster, recommend contractor deployment.\n",
    "2. If the dominant peril is Wind, prioritize PL5 contractors (Sev-5 heavy).\n",
    "3. If the dominant peril is Tornado, prioritize volume (PL3-PL4 sufficient).\n",
    "4. If Hail, recommend staggered deployment over 7 days.\n",
    "5. Account for reporting lag — Day 1 volume underestimates total.\n",
    "6. If multiple clusters affected, recommend staging by hit order.\n",
    "7. Always state: 'Sponsor approval required before execution.'\n",
    "8. Always call run_cat_scenario with specific contractor numbers.\n"
  ))

  paste(sections, collapse = "")
}

# ============================================================
# format_weather_alert_with_frequency()
# ============================================================
# Enhances format_weather_alert() with FEMA frequency context
# for the affected clusters. Falls back to base alert if the
# ARIMA model file isn't present.
# ============================================================

format_weather_alert_with_frequency <- function(alert_row) {

  base_alert <- format_weather_alert(alert_row)

  arima_path <- "C:/Users/etomi/Downloads/fema_arima_models.rds"
  if (!file.exists(arima_path)) return(base_alert)

  arima_data <- tryCatch(readRDS(arima_path), error = function(e) NULL)
  if (is.null(arima_data)) return(base_alert)

  affected_clusters <- strsplit(alert_row$clusters, ", ")[[1]]

  freq_lines <- lapply(affected_clusters, function(cl) {
    if (!cl %in% names(arima_data$cluster_models)) return(NULL)
    m <- arima_data$cluster_models[[cl]]
    paste0(
      cl, ": ", m$next_year, " events/year forecasted",
      " (was ", m$historical_mean_5yr, " avg over last 5 years)"
    )
  })
  freq_lines <- Filter(Negate(is.null), freq_lines)

  if (length(freq_lines) == 0) return(base_alert)

  freq_section <- paste0(
    "\nDISASTER FREQUENCY TREND (FEMA ",
    arima_data$data_years[1], "-", arima_data$data_years[2], "):\n",
    paste(freq_lines, collapse = "\n"), "\n",
    "Consider this trend when recommending tour lengths ",
    "and contractor reserve levels.\n"
  )

  paste0(base_alert, freq_section)
}

# ============================================================
# MOCK SCENARIOS — 4 scenarios for demo mode
# ============================================================
# Use for demos. Swap fetch_noaa_alerts() with one of these
# in the reactivePoll for presentation.
# ============================================================

mock_tornado_c3 <- function() {
  data.frame(
    event      = "Tornado Warning",
    storm_type = "Tornado",
    states     = "TN, KY",
    clusters   = "C3",
    headline   = "Tornado Warning issued for Middle Tennessee including Clarksville",
    severity   = "Extreme",
    urgency    = "Immediate",
    stringsAsFactors = FALSE
  )
}

mock_wind_c4 <- function() {
  data.frame(
    event      = "High Wind Warning",
    storm_type = "Wind",
    states     = "MD, VA",
    clusters   = "C4",
    headline   = "High Wind Warning for Maryland and Virginia corridor",
    severity   = "Severe",
    urgency    = "Expected",
    stringsAsFactors = FALSE
  )
}

mock_winter_storm_multi <- function() {
  data.frame(
    event      = "Winter Storm Warning",
    storm_type = "Winter Storm",
    states     = "TN, KY, MD, VA, NC",
    clusters   = "C3, C4, C0",
    headline   = "Winter Storm Warning for Southeast and Mid-Atlantic states",
    severity   = "Severe",
    urgency    = "Expected",
    stringsAsFactors = FALSE
  )
}

mock_hail_c5 <- function() {
  data.frame(
    event      = "Severe Thunderstorm Warning",
    storm_type = "Severe Storm",
    states     = "MS, LA",
    clusters   = "C5",
    headline   = "Severe Thunderstorm Warning with large hail for Mississippi",
    severity   = "Severe",
    urgency    = "Immediate",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# get_cluster_summary()
# ============================================================
# Quick summary for a cluster — used by risk_monitor and
# build_context when they need cluster context.

get_cluster_summary <- function(cluster) {
  profiles <- CLAIM_PROFILES[CLAIM_PROFILES$cluster == cluster, ]
  rc       <- REPORTING_CURVES[REPORTING_CURVES$cluster == cluster, ]
  st       <- STORM_TIMING[STORM_TIMING$cluster == cluster, ]
  hs       <- HOTSPOTS[[cluster]]

  list(
    profiles           = profiles,
    reporting          = rc,
    timing             = st,
    hotspot            = hs,
    imputation_warning = IMPUTATION_WARNINGS[[cluster]]
  )
}
