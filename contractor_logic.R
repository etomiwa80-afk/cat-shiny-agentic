# contractor_logic.R — Fast contractor simulation for CATOPT
#
# simulate_contractors() runs in under 1 second — no MIP, pure greedy.
# Designed for the Shiny slider: sponsor picks contractors per cluster,
# results update instantly.
#
# Architecture: Run 1 and Run 2 are pre-computed and cached. This function
# layers contractors on top of Run 2's output without re-running the MIP.
# Valid because contractors only touch Sev-5 claims that Run 2 couldn't cover —
# they don't compete with staff for any other severity.

suppressPackageStartupMessages(library(dplyr))


# --- Fast contractor simulation ---
#
# run2_result   : output of simulate() for Run 2
# cluster_counts: named integer vector — contractors per cluster
#                 e.g. c("0"=5, "1"=5, "2"=10, "3"=20, "4"=15, "5"=2, "6"=5)
# cfg           : config object
#
# Returns a list in the same structure as simulate() so compare_runs() works.
# The daily_log is inherited from Run 2 (staff schedule unchanged).
simulate_contractors <- function(run2_result, cluster_counts, cfg) {

  pl5_tp   <- get_throughput(5L, 5L, cfg)               # 0.25 claims/day
  pl5_days <- as.integer(ceiling(1.0 / pl5_tp))          # 4 days per claim

  # All Sev-5 claims with Run 2 outcomes
  sev5_all <- run2_result$assignments %>%
    filter(severity == 5L) %>%
    arrange(day_arrived)

  # Contractor candidates: every Sev-5 claim Run 2 failed to meet.
  # Includes unassigned claims (SLA expired while waiting) and assigned claims
  # where the adjuster finishes after the deadline.
  # Staff-assigned claims that WILL meet SLA are not touched — contractor
  # assignment would be redundant and inflates cost.
  candidates <- sev5_all %>%
    filter(sla_met == FALSE | is.na(sla_met))

  # Start with a copy of Run 2 assignments — contractors will update rows in-place
  asgn <- as.data.frame(run2_result$assignments, stringsAsFactors = FALSE)
  if (!"eff_rate" %in% names(asgn)) asgn$eff_rate <- NA_real_

  contractor_log_rows <- list()

  if (nrow(candidates) > 0 && sum(unlist(cluster_counts), na.rm = TRUE) > 0L) {

    # Build contractor pool
    # Each contractor is a PL5, pre-positioned (arrives Day 1, no travel delay).
    contr_free    <- integer(0)   # next free day for each contractor
    contr_cluster <- character(0) # which cluster each belongs to
    contr_ids     <- integer(0)   # negative IDs mark contractors
    id_seq        <- 0L

    for (cl in names(cluster_counts)) {
      n <- as.integer(cluster_counts[[cl]])
      if (is.null(n) || is.na(n) || n <= 0L) next
      for (k in seq_len(n)) {
        id_seq        <- id_seq + 1L
        contr_ids     <- c(contr_ids,     -id_seq)
        contr_cluster <- c(contr_cluster, as.character(cl))
        contr_free    <- c(contr_free,    1L)       # available from Day 1
      }
    }

    # Greedy assignment: earliest-deadline-first within each cluster.
    # Optimal by exchange argument: assigning to earliest-finishing contractor
    # minimises SLA misses.
    for (ci in seq_len(nrow(candidates))) {
      cand <- candidates[ci, ]
      cl   <- as.character(cand$cluster)
      arr  <- as.integer(cand$day_arrived)
      sla  <- as.integer(cand$sla_deadline)

      elig_idx <- which(contr_cluster == cl)
      if (length(elig_idx) == 0) next

      start_days  <- pmax(arr, contr_free[elig_idx])
      finish_days <- start_days + pl5_days - 1L
      feasible    <- which(finish_days <= sla)
      if (length(feasible) == 0) next

      best_local  <- feasible[which.min(finish_days[feasible])]
      best_global <- elig_idx[best_local]
      start_day   <- start_days[best_local]
      finish_day  <- finish_days[best_local]

      contr_free[best_global] <- finish_day + 1L

      # Update the claim row in the assignments copy
      row_idx <- which(asgn$claim_id == cand$claim_id)
      if (length(row_idx) == 0) next

      asgn$assigned_adj[row_idx]       <- contr_ids[best_global]
      asgn$adj_skill[row_idx]          <- 5L
      asgn$adj_home[row_idx]           <- NA_character_
      asgn$adj_qtr_rank[row_idx]       <- 0L
      asgn$assignment_type[row_idx]    <- "contractor"
      asgn$mobilization_delay[row_idx] <- 0L
      asgn$day_assigned[row_idx]       <- start_day
      asgn$day_start_work[row_idx]     <- start_day
      asgn$days_needed[row_idx]        <- pl5_days
      asgn$eff_rate[row_idx]           <- pl5_tp
      asgn$progress[row_idx]           <- 1.0
      asgn$day_completed[row_idx]      <- finish_day
      asgn$sla_met[row_idx]            <- TRUE
      asgn$status[row_idx]             <- "Completed"

      contractor_log_rows[[length(contractor_log_rows) + 1L]] <- data.frame(
        cluster       = cl,
        adj_id        = contr_ids[best_global],
        skill         = 5L,
        day_assigned  = start_day,
        day_completed = finish_day,
        claim_id      = cand$claim_id,
        stringsAsFactors = FALSE
      )
    }
  }

  # ── Rebuild summary ────────────────────────────────────────────────────────
  n_sla_met    <- sum(asgn$sla_met == TRUE,  na.rm = TRUE)
  n_sla_missed <- sum(asgn$sla_met == FALSE, na.rm = TRUE)
  n_completed  <- sum(asgn$status  == "Completed")
  n_missed_st  <- sum(asgn$status  == "SLA Missed")

  sev_compliance <- lapply(1:5, function(s) {
    sub <- asgn[!is.na(asgn$severity) & asgn$severity == s, ]
    list(
      met    = sum(sub$sla_met == TRUE,  na.rm = TRUE),
      missed = sum(sub$sla_met == FALSE, na.rm = TRUE),
      pct    = round(sum(sub$sla_met == TRUE, na.rm = TRUE) /
                       max(1L, nrow(sub)) * 100, 1)
    )
  })
  names(sev_compliance) <- paste0("sev", 1:5)

  # Contractor cost: total working days × daily rate
  contr_log_df <- if (length(contractor_log_rows) > 0)
    bind_rows(contractor_log_rows) else NULL

  total_contr_days <- if (!is.null(contr_log_df))
    sum(contr_log_df$day_completed - contr_log_df$day_assigned + 1L) else 0L
  contractor_cost  <- total_contr_days * cfg$cost_contractor_incremental

  sla_exposure <- sum(sapply(1:5, function(s) {
    sev_compliance[[paste0("sev", s)]]$missed *
      as.numeric(cfg$sla_failure_cost[as.character(s)])
  }))

  r2cost <- run2_result$summary$cost

  summary_out <- list(
    total_claims       = nrow(asgn),
    total_completed    = n_completed,
    total_sla_missed   = n_missed_st,
    sla_met            = n_sla_met,
    sla_missed         = n_sla_missed,
    overall_compliance = round(n_sla_met / nrow(asgn) * 100, 1),
    days_to_complete   = run2_result$summary$days_to_complete,
    cost = list(
      local_cost            = r2cost$local_cost,
      deployed_cost         = r2cost$deployed_cost,
      contractor_cost       = contractor_cost,
      total                 = r2cost$local_cost + r2cost$deployed_cost + contractor_cost,
      incremental_operating = r2cost$incremental_operating + contractor_cost,
      sla_failure_exposure  = sla_exposure
    ),
    by_severity = sev_compliance
  )

  # Cluster summary
  cluster_summary <- asgn %>%
    group_by(cluster) %>%
    summarise(
      n_claims    = n(),
      n_sev5      = sum(severity == 5L),
      n_completed = sum(status == "Completed"),
      n_missed    = sum(status == "SLA Missed"),
      sla_met     = sum(sla_met == TRUE,  na.rm = TRUE),
      sla_missed  = sum(sla_met == FALSE, na.rm = TRUE),
      sla_pct     = round(sum(sla_met == TRUE, na.rm = TRUE) / n() * 100, 1),
      .groups     = "drop"
    )

  list(
    summary         = summary_out,
    daily_log       = run2_result$daily_log,   # staff schedule unchanged
    assignments     = asgn,
    cluster_summary = cluster_summary,
    contractor_log  = contr_log_df
  )
}
