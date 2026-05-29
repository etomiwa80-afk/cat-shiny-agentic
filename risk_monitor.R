# ============================================================
# risk_monitor.R
# PROACTIVE RISK ALERT SYSTEM
# ============================================================
# Scans simulation state against danger thresholds.
# Returns a character vector of alert messages.
# Empty vector = no alerts = stay quiet.
#
# Column names match travelers_sim_v6.R output:
#   claims:    Cluster, Severity, Assigned_To, Day_Assigned,
#              Day_Completed, SLA_Deadline
#   adjusters: Working_Cluster, Adjuster_Id, Tour_End
#   daily_log: Day
# ============================================================

RISK_THRESHOLDS <- list(
  sev5_compliance_floor = 0.50,   # Alert if any cluster Sev-5 SLA % drops below 50%
  unassigned_claims     = 10,     # Alert if any cluster has 10+ unassigned claims
  tour_ending_days      = 3,      # Alert if adjusters have tours ending within 3 days
  adjuster_utilization  = 0.90,   # (reserved for future use)
  sev5_queue_depth      = 5       # Alert if 5+ Sev-5 claims have no adjuster and no completion
)

# ============================================================
# check_risk_alerts()
# ============================================================
# sim_result: return value of run_scenario() or baseline_result
# day:        simulation day to evaluate (defaults to final day)
# Returns: character vector of alert strings. Empty = clean.
# ============================================================

check_risk_alerts <- function(sim_result, day = NULL) {

  alerts <- character(0)

  if (is.null(sim_result)) return(alerts)

  claims    <- sim_result$claims
  adjusters <- sim_result$adjusters
  daily_log <- sim_result$daily_log

  if (is.null(claims) || nrow(claims) == 0)    return(alerts)
  if (is.null(adjusters) || nrow(adjusters) == 0) return(alerts)

  # Current day: latest in daily log, or provided
  if (is.null(day)) {
    day <- if (!is.null(daily_log) && nrow(daily_log) > 0)
             max(daily_log$Day, na.rm = TRUE)
           else 7L
  }

  clusters <- paste0("C", 0:6)

  for (cl in clusters) {

    cl_claims <- claims[claims$Cluster == cl, ]
    if (nrow(cl_claims) == 0) next

    # ---- Sev-5 SLA compliance by cluster ----
    cl_sev5 <- cl_claims[cl_claims$Severity == 5L, ]
    if (nrow(cl_sev5) > 0) {
      # SLA met = completed on or before the SLA deadline
      sla_met <- sum(
        !is.na(cl_sev5$Day_Completed) &
        cl_sev5$Day_Completed <= cl_sev5$SLA_Deadline,
        na.rm = TRUE
      )
      pct <- sla_met / nrow(cl_sev5)

      if (pct < RISK_THRESHOLDS$sev5_compliance_floor) {
        alerts <- c(alerts, paste0(
          "WARNING: ", cl, " Sev-5 SLA compliance at ",
          round(pct * 100), "% (", sla_met, " of ", nrow(cl_sev5),
          " claims met). Below ",
          RISK_THRESHOLDS$sev5_compliance_floor * 100, "% threshold."
        ))
      }
    }

    # ---- Unassigned claims (no adjuster, not yet complete) ----
    cl_unassigned <- cl_claims[
      is.na(cl_claims$Assigned_To) & is.na(cl_claims$Day_Completed), ]
    if (nrow(cl_unassigned) >= RISK_THRESHOLDS$unassigned_claims) {
      alerts <- c(alerts, paste0(
        "WARNING: ", cl, " has ", nrow(cl_unassigned),
        " unassigned claims with no eligible adjuster."
      ))
    }

    # ---- Tour endings within N days (active adjusters only) ----
    cl_adj <- adjusters[
      !is.na(adjusters$Working_Cluster) &
      adjusters$Working_Cluster == cl &
      !adjusters$Is_Contractor, ]

    if (nrow(cl_adj) > 0 && "Tour_End" %in% names(cl_adj)) {
      ending_soon <- cl_adj[
        !is.na(cl_adj$Tour_End) &
        cl_adj$Tour_End <= day + RISK_THRESHOLDS$tour_ending_days &
        cl_adj$Tour_End > day, ]

      if (nrow(ending_soon) > 0) {
        # Count active claims held by these adjusters
        ending_ids    <- ending_soon$Adjuster_Id
        active_claims <- claims[
          !is.na(claims$Assigned_To) &
          claims$Assigned_To %in% ending_ids &
          is.na(claims$Day_Completed), ]

        alerts <- c(alerts, paste0(
          "WARNING: ", cl, " — ", nrow(ending_soon), " adjuster(s) have tours ",
          "ending within ", RISK_THRESHOLDS$tour_ending_days, " days. ",
          nrow(active_claims), " active claim(s) will lose their handler."
        ))
      }
    }

    # ---- Sev-5 queue depth (unassigned, incomplete) ----
    cl_sev5_queued <- cl_claims[
      cl_claims$Severity == 5L &
      is.na(cl_claims$Assigned_To) &
      is.na(cl_claims$Day_Completed), ]

    if (nrow(cl_sev5_queued) >= RISK_THRESHOLDS$sev5_queue_depth) {
      earliest_deadline <- min(cl_sev5_queued$SLA_Deadline, na.rm = TRUE)
      days_left         <- max(earliest_deadline - day, 0L)
      timing_msg <- if (days_left == 0L)
        "SLA deadline already passed — penalty locked in."
      else
        paste0("SLA breach in ", days_left, " day(s).")
      alerts <- c(alerts, paste0(
        "CRITICAL: ", cl, " has ", nrow(cl_sev5_queued),
        " Sev-5 claims queued with no adjuster. ", timing_msg
      ))
    }
  }

  alerts
}

# ============================================================
# format_risk_alerts()
# ============================================================
# Formats the alert vector into a prompt for the AI.
# Returns NULL if no alerts.
# ============================================================

format_risk_alerts <- function(alerts) {
  if (length(alerts) == 0) return(NULL)

  paste0(
    "PROACTIVE RISK SCAN — ", length(alerts), " alert(s) detected:\n\n",
    paste0(seq_along(alerts), ". ", alerts, collapse = "\n"),
    "\n\nFor each alert above, provide a specific recommended action. ",
    "Reference cluster names (C0=Carolinas, C1=AL/GA, C2=Northeast, ",
    "C3=TN/KY, C4=Maryland/DMV, C5=MS/LA, C6=New England) and numbers. ",
    "If multiple alerts affect the same cluster, combine into one recommendation. ",
    "Be direct — state the action, the cluster, and the expected impact."
  )
}
