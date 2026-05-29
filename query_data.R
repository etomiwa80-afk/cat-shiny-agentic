# ============================================================
# query_data.R — Read-only data lookup for AI query tool (v2)
# ============================================================
# query_data(active_result, query_type, filters, group_by)
#
# query_type options:
#   "adjusters"          — filter/browse the adjuster roster
#   "claims"             — filter/browse claims
#   "daily"              — day-level simulation log
#   "cluster_summary"    — per-cluster outcome summary
#   "workforce_overview" — cross-tab cluster x skill + cluster x type
#   "adjuster_profile"   — full profile for one adjuster (needs Adjuster_Id)
#   "sla_risk"           — claims approaching SLA deadline (needs day)
#   "tour_cliff"         — adjusters finishing tour soon (needs day)
#
# filters keys (all optional unless noted):
#   cluster, skill, severity, day, type, status,
#   pre_deployment, Adjuster_Id, assigned, day_range
# ============================================================

library(dplyr)

# ----------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------

safe_text <- function(x, max_chars = 3000) {
  if (nchar(x) <= max_chars) return(x)
  paste0(substr(x, 1, max_chars),
         sprintf("\n... [output truncated — %d chars total]", nchar(x)))
}

empty_msg <- function(what, filters_desc = "") {
  msg <- sprintf("No %s found", what)
  if (nzchar(filters_desc)) msg <- paste0(msg, " matching: ", filters_desc)
  paste0(msg, ".")
}

penalty_for <- function(sev) {
  c("5"=2000, "4"=1000, "3"=500, "2"=250, "1"=100)[as.character(sev)]
}

# ----------------------------------------------------------
# Main function
# ----------------------------------------------------------

query_data <- function(active_result, query_type, filters = list(), group_by = NULL) {

  adj <- active_result$adjusters
  cl  <- active_result$claims
  log <- active_result$daily_log

  # Validate data availability
  if (is.null(adj) || nrow(adj) == 0) return("Adjuster data not available in this result.")
  if (is.null(cl)  || nrow(cl)  == 0) return("Claims data not available in this result.")

  # Derive Type column on adjusters
  adj$Type <- ifelse(adj$Is_Contractor, "Contractor",
                ifelse(adj$Is_Deployed,  "Deployed", "Local"))

  # Coerce Tour_End to integer for day comparisons
  adj$Tour_End   <- as.integer(adj$Tour_End)
  adj$Tour_Start <- as.integer(adj$Tour_Start)

  # ----------------------------------------------------------
  # ADJUSTERS
  # ----------------------------------------------------------
  if (query_type == "adjusters") {
    df <- adj

    # Filter by Adjuster_Id
    if (!is.null(filters$Adjuster_Id)) {
      df <- df[df$Adjuster_Id == as.integer(filters$Adjuster_Id), ]
    }

    # Filter by cluster (pre-deployment uses Home_Cluster)
    if (!is.null(filters$cluster)) {
      col <- if (isTRUE(filters$pre_deployment)) "Home_Cluster" else "Working_Cluster"
      df  <- df[df[[col]] == filters$cluster, ]
    }

    # Filter by skill
    if (!is.null(filters$skill)) {
      df <- df[df$Skill == as.integer(filters$skill), ]
    }

    # Filter by type (local / deployed / contractor)
    if (!is.null(filters$type)) {
      type_map <- c("local"="Local", "deployed"="Deployed", "contractor"="Contractor")
      mapped   <- type_map[tolower(filters$type)]
      if (!is.na(mapped)) df <- df[df$Type == mapped, ]
    }

    # Filter by final sim Status (Active / Gone)
    if (!is.null(filters$status) && is.null(filters$day)) {
      df <- df[tolower(df$Status) == tolower(filters$status), ]
    }

    # Filter assigned/unassigned (has Current_Claim or not)
    if (!is.null(filters$assigned)) {
      if (isTRUE(filters$assigned)) {
        df <- df[!is.na(df$Current_Claim), ]
      } else {
        df <- df[is.na(df$Current_Claim) & df$Status == "Active", ]
      }
    }

    # Filter: tour ending within N days of current day (default window = 3)
    if (isTRUE(filters$tour_ending)) {
      # Determine current day: use provided day filter, or max day in log
      cur_day <- if (!is.null(filters$day)) {
        as.integer(filters$day)
      } else if (!is.null(log) && nrow(log) > 0) {
        max(log$Day, na.rm = TRUE)
      } else {
        max(adj$Tour_End, na.rm = TRUE)   # last resort
      }
      window  <- as.integer(filters$day_range %||% 3L)
      df <- df[!is.na(df$Tour_End) &
               df$Tour_End >  cur_day &
               df$Tour_End <= cur_day + window, ]
      if (nrow(df) == 0)
        return(sprintf("No active adjusters finishing their tour within %d days of Day %d.",
                       window, cur_day))
      df$Days_Until_Gone <- df$Tour_End - cur_day
    }

    # Compute per-day status when day filter provided
    if (!is.null(filters$day)) {
      day <- as.integer(filters$day)
      active_ids <- cl$Assigned_To[
        !is.na(cl$Assigned_To) & !is.na(cl$Day_Assigned) &
        cl$Day_Assigned <= day & (is.na(cl$Day_Completed) | cl$Day_Completed >= day)
      ]
      df$Status_Today <- ifelse(
        df$Tour_End < day, "Gone",
        ifelse(df$Adjuster_Id %in% active_ids, "Working", "Available")
      )
      if (!is.null(filters$status)) {
        df <- df[df$Status_Today == filters$status, ]
      }
    }

    if (nrow(df) == 0) return(empty_msg("adjusters"))

    # Group-by cross-tabs
    if (!is.null(group_by)) {
      if (group_by == "cluster_skill") {
        cross         <- as.data.frame.matrix(table(df$Working_Cluster, df$Skill))
        cross$Total   <- rowSums(cross)
        cross$Cluster <- rownames(cross)
        cross         <- cross[, c("Cluster", setdiff(names(cross), "Cluster"))]
        return(safe_text(paste(capture.output(print(cross, row.names = FALSE)), collapse = "\n")))
      }
      if (group_by == "cluster_type") {
        cross         <- as.data.frame.matrix(table(df$Working_Cluster, df$Type))
        cross$Total   <- rowSums(cross)
        cross$Cluster <- rownames(cross)
        cross         <- cross[, c("Cluster", setdiff(names(cross), "Cluster"))]
        return(safe_text(paste(capture.output(print(cross, row.names = FALSE)), collapse = "\n")))
      }
      grp_col <- switch(group_by,
        "cluster" = if (isTRUE(filters$pre_deployment)) "Home_Cluster" else "Working_Cluster",
        "skill"   = "Skill",
        "type"    = "Type",
        "Working_Cluster"
      )
      summary_df        <- as.data.frame(table(df[[grp_col]]))
      names(summary_df)  <- c(group_by, "Count")
      return(safe_text(paste(capture.output(print(summary_df, row.names = FALSE)), collapse = "\n")))
    }

    # Detail view
    result_text <- sprintf("Found %d adjusters matching filters.\n", nrow(df))
    if (nrow(df) <= 30) {
      show_cols <- intersect(
        c("Adjuster_Id","Skill","Type","Status","Home_Cluster","Working_Cluster",
          "Tour_Start","Tour_End","Days_Until_Gone","Status_Today","Current_Claim",
          "Has_Boost","Boosted_By","Boosting"),
        names(df)
      )
      result_text <- paste0(result_text,
        paste(capture.output(print(as.data.frame(df[, show_cols]), row.names = FALSE)),
              collapse = "\n"))
    } else {
      skill_counts        <- as.data.frame(table(df$Skill))
      names(skill_counts)  <- c("Skill", "Count")
      type_counts         <- as.data.frame(table(df$Type))
      names(type_counts)   <- c("Type", "Count")
      result_text <- paste0(result_text,
        "By skill level:\n",
        paste(capture.output(print(skill_counts, row.names = FALSE)), collapse = "\n"),
        "\n\nBy type:\n",
        paste(capture.output(print(type_counts, row.names = FALSE)), collapse = "\n"))
    }
    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # ADJUSTER PROFILE  (single adjuster deep-dive)
  # ----------------------------------------------------------
  if (query_type == "adjuster_profile") {
    if (is.null(filters$Adjuster_Id))
      return("adjuster_profile requires filters$Adjuster_Id. Pass the numeric adjuster ID.")

    tid <- as.integer(filters$Adjuster_Id)
    a   <- adj[adj$Adjuster_Id == tid, ]
    if (nrow(a) == 0)
      return(sprintf("No adjuster found with Adjuster_Id %d.", tid))
    a <- a[1, ]

    # All claims ever handled by this adjuster
    adj_claims  <- cl[!is.na(cl$Assigned_To) & cl$Assigned_To == tid, ]
    completed   <- adj_claims[!is.na(adj_claims$Day_Completed), ]
    in_progress <- adj_claims[is.na(adj_claims$Day_Completed), ]

    # Active claim on a specific day (if day filter provided)
    if (!is.null(filters$day)) {
      day_val <- as.integer(filters$day)
      active_on_day <- adj_claims[
        !is.na(adj_claims$Day_Assigned) & adj_claims$Day_Assigned <= day_val &
        (is.na(adj_claims$Day_Completed) | adj_claims$Day_Completed >= day_val), ]
      active_claim_str <- if (nrow(active_on_day) > 0) {
        r <- active_on_day[1, ]
        sprintf("Sev-%d claim %s in %s (assigned Day %d)",
                r$Severity, r$Claim_Number, r$Cluster, r$Day_Assigned)
      } else sprintf("None (no active claim on Day %d)", day_val)
    } else {
      # Use final Current_Claim from adjusters table
      active_claim_str <- if (!is.na(a$Current_Claim)) {
        ac <- cl[cl$Claim_Number == a$Current_Claim, ]
        if (nrow(ac) > 0)
          sprintf("Sev-%d claim %s in %s", ac$Severity[1], ac$Claim_Number[1], ac$Cluster[1])
        else as.character(a$Current_Claim)
      } else "None"
    }

    boost_str <- if (isTRUE(a$Has_Boost)) {
      sprintf("YES (paired with PL0 id=%d)", a$Boosted_By)
    } else if (!is.na(a$Boosting) && !is.null(a$Boosting)) {
      sprintf("PL0 booster for PL4/5 id=%d", a$Boosting)
    } else {
      "No"
    }

    result_text <- sprintf(
      paste0(
        "ADJUSTER PROFILE  Adjuster_Id: %d\n",
        "  Skill:          PL%d\n",
        "  Type:           %s\n",
        "  Home Cluster:   %s\n",
        "  Working Cluster:%s\n",
        "  Tour:           Day %d to Day %d\n",
        "  Final Status:   %s\n",
        "  PL0 Boost:      %s\n",
        "  Active Claim:   %s\n",
        "  Completed Claims: %d\n",
        "  In-Progress:    %d\n"
      ),
      tid, a$Skill, a$Type, a$Home_Cluster, a$Working_Cluster,
      a$Tour_Start, a$Tour_End, a$Status, boost_str, active_claim_str,
      nrow(completed), nrow(in_progress)
    )

    if (nrow(completed) > 0) {
      sev_break        <- as.data.frame(table(completed$Severity))
      names(sev_break)  <- c("Severity", "Claims_Completed")
      result_text <- paste0(result_text, "\nCompleted by severity:\n",
        paste(capture.output(print(sev_break, row.names = FALSE)), collapse = "\n"))
    }

    if (nrow(adj_claims) > 0 && nrow(adj_claims) <= 15) {
      show_cols <- intersect(
        c("Claim_Number","Severity","Cluster","Day_Assigned","Day_Completed","SLA_Deadline"),
        names(adj_claims)
      )
      result_text <- paste0(result_text, "\n\nAll assigned claims:\n",
        paste(capture.output(
          print(as.data.frame(adj_claims[order(adj_claims$Day_Assigned), show_cols]),
                row.names = FALSE)), collapse = "\n"))
    }

    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # CLAIMS
  # ----------------------------------------------------------
  if (query_type == "claims") {
    df <- cl

    # Filter by specific adjuster (Assigned_To)
    if (!is.null(filters$Adjuster_Id)) {
      df <- df[!is.na(df$Assigned_To) & df$Assigned_To == as.integer(filters$Adjuster_Id), ]
    }

    if (!is.null(filters$cluster))  df <- df[df$Cluster  == filters$cluster,             ]
    if (!is.null(filters$severity)) df <- df[df$Severity == as.integer(filters$severity), ]

    # Compute day-based status
    if (!is.null(filters$day)) {
      day <- as.integer(filters$day)
      df$Day_Status <- ifelse(
        is.na(df$Day_Assigned) | df$Day_Assigned > day, "Waiting",
        ifelse(!is.na(df$Day_Completed) & df$Day_Completed <= day, "Completed", "In Progress")
      )
      if (!is.null(filters$status)) {
        status_map <- c(
          "unassigned"  = "Waiting", "waiting"     = "Waiting",
          "in progress" = "In Progress", "in_progress" = "In Progress",
          "completed"   = "Completed"
        )
        target <- status_map[tolower(filters$status)]
        if (!is.na(target)) df <- df[df$Day_Status == target, ]
      }
    }

    # Join adjuster info for assigned claims
    if (nrow(df) > 0 && any(!is.na(df$Assigned_To))) {
      adj_info <- adj[, c("Adjuster_Id","Skill","Type","Home_Cluster")]
      names(adj_info)[1] <- "Assigned_To"
      df <- merge(df, adj_info, by = "Assigned_To", all.x = TRUE)
    }

    if (nrow(df) == 0) return(empty_msg("claims"))

    # Group-by
    if (!is.null(group_by)) {
      grp_col <- switch(group_by,
        "cluster"  = "Cluster", "severity" = "Severity", "Cluster")
      summary_df        <- as.data.frame(table(df[[grp_col]]))
      names(summary_df)  <- c(group_by, "Count")
      return(safe_text(paste(capture.output(print(summary_df, row.names = FALSE)), collapse = "\n")))
    }

    result_text <- sprintf("Found %d claims matching filters.\n", nrow(df))
    if (nrow(df) <= 20) {
      show_cols <- intersect(
        c("Claim_Number","Severity","Cluster","SLA_Deadline","Day_Status",
          "Assigned_To","Skill","Type"),
        names(df)
      )
      result_text <- paste0(result_text,
        paste(capture.output(print(as.data.frame(df[, show_cols]), row.names = FALSE)),
              collapse = "\n"))
    } else {
      sev_counts        <- as.data.frame(table(df$Severity))
      names(sev_counts)  <- c("Severity", "Count")
      result_text <- paste0(result_text, "By severity:\n",
        paste(capture.output(print(sev_counts, row.names = FALSE)), collapse = "\n"))
      if ("Day_Status" %in% names(df)) {
        status_counts        <- as.data.frame(table(df$Day_Status))
        names(status_counts)  <- c("Status", "Count")
        result_text <- paste0(result_text, "\n\nBy status:\n",
          paste(capture.output(print(status_counts, row.names = FALSE)), collapse = "\n"))
      }
    }
    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # SLA RISK  (claims approaching deadline)
  # ----------------------------------------------------------
  if (query_type == "sla_risk") {
    if (is.null(filters$day))
      return("sla_risk requires filters$day (current simulation day).")

    day    <- as.integer(filters$day)
    window <- as.integer(filters$day_range %||% 5L)

    # Claims not yet completed by this day
    unfinished <- cl[is.na(cl$Day_Completed) | cl$Day_Completed > day, ]
    if (!is.null(filters$cluster))  unfinished <- unfinished[unfinished$Cluster  == filters$cluster,             ]
    if (!is.null(filters$severity)) unfinished <- unfinished[unfinished$Severity == as.integer(filters$severity), ]

    unfinished$Days_Until_SLA <- unfinished$SLA_Deadline - day
    at_risk <- unfinished[unfinished$Days_Until_SLA <= window & unfinished$Days_Until_SLA >= -999L, ]

    if (nrow(at_risk) == 0)
      return(sprintf("No unfinished claims approaching SLA within %d days of Day %d.", window, day))

    at_risk$Penalty_Rate <- sapply(at_risk$Severity, penalty_for)
    at_risk$Risk <- ifelse(at_risk$Days_Until_SLA <= 0, "MISSED",
                     ifelse(at_risk$Days_Until_SLA <= 2, "CRITICAL", "AT RISK"))
    at_risk <- at_risk[order(at_risk$Days_Until_SLA, -at_risk$Penalty_Rate), ]

    total_exposure <- sum(at_risk$Penalty_Rate, na.rm = TRUE)
    missed_count   <- sum(at_risk$Risk == "MISSED")
    critical_count <- sum(at_risk$Risk == "CRITICAL")

    result_text <- sprintf(
      paste0("SLA RISK REPORT  Day %d  window=%d days\n",
             "%d claims at risk | MISSED: %d | CRITICAL (<= 2 days): %d\n",
             "Total penalty exposure: $%s\n\n"),
      day, window, nrow(at_risk), missed_count, critical_count,
      format(total_exposure, big.mark = ",")
    )

    show <- head(at_risk, 25)
    show_cols <- intersect(
      c("Claim_Number","Severity","Cluster","SLA_Deadline","Days_Until_SLA","Risk","Assigned_To"),
      names(show)
    )
    result_text <- paste0(result_text,
      paste(capture.output(print(as.data.frame(show[, show_cols]), row.names = FALSE)),
            collapse = "\n"))

    if (nrow(at_risk) > 25)
      result_text <- paste0(result_text,
        sprintf("\n... and %d more at-risk claims not shown.", nrow(at_risk) - 25))

    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # TOUR CLIFF  (adjusters finishing their tour soon)
  # ----------------------------------------------------------
  if (query_type == "tour_cliff") {
    if (is.null(filters$day))
      return("tour_cliff requires filters$day (current simulation day).")

    day    <- as.integer(filters$day)
    window <- as.integer(filters$day_range %||% 5L)

    leaving <- adj[
      adj$Tour_End >= day & adj$Tour_End <= (day + window - 1L) & adj$Status == "Active", ]

    if (!is.null(filters$cluster))
      leaving <- leaving[leaving$Working_Cluster == filters$cluster, ]

    if (nrow(leaving) == 0)
      return(sprintf("No active adjusters finishing their tour within %d days of Day %d.", window, day))

    leaving$Days_Until_Gone <- leaving$Tour_End - day + 1L

    by_cluster        <- as.data.frame(table(leaving$Working_Cluster))
    names(by_cluster)  <- c("Cluster", "Adjusters_Leaving")

    by_day        <- as.data.frame(table(leaving$Tour_End))
    names(by_day)  <- c("Day", "Count")

    result_text <- sprintf(
      "TOUR CLIFF REPORT  Day %d  window=%d days\n%d active adjusters finishing tour\n\nBy cluster:\n%s\n\nBy day:\n%s\n",
      day, window, nrow(leaving),
      paste(capture.output(print(by_cluster, row.names = FALSE)), collapse = "\n"),
      paste(capture.output(print(by_day,     row.names = FALSE)), collapse = "\n")
    )

    # High-skill (PL4+) departures — biggest capacity risk
    high_skill <- leaving[leaving$Skill >= 4L, ]
    if (nrow(high_skill) > 0) {
      hs_cluster        <- as.data.frame(table(high_skill$Working_Cluster))
      names(hs_cluster)  <- c("Cluster", "PL4_PL5_Leaving")
      result_text <- paste0(result_text,
        sprintf("\nHigh-skill departures (PL4+): %d total\n%s", nrow(high_skill),
          paste(capture.output(print(hs_cluster, row.names = FALSE)), collapse = "\n")))
    } else {
      result_text <- paste0(result_text, "\nNo PL4+ adjusters finishing in this window.")
    }

    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # DAILY LOG
  # ----------------------------------------------------------
  if (query_type == "daily") {
    if (is.null(log) || nrow(log) == 0) return("Daily log not available.")

    if (!is.null(filters$day)) {
      day_row <- log[log$Day == as.integer(filters$day), ]
      if (nrow(day_row) == 0)
        return(sprintf("No daily log entry for Day %d.", as.integer(filters$day)))
      return(safe_text(paste(capture.output(
        print(as.data.frame(t(day_row)), row.names = TRUE)
      ), collapse = "\n")))
    }

    # Day range filter
    if (!is.null(filters$day_range) && !is.null(filters$day)) {
      d1 <- as.integer(filters$day)
      d2 <- d1 + as.integer(filters$day_range) - 1L
      log <- log[log$Day >= d1 & log$Day <= d2, ]
    }

    summary_cols <- intersect(
      c("Day","Active_Adjusters","Gone_Adjusters","Claims_Completed_Today",
        "Claims_Completed_Cumulative","Claims_In_Progress","Claims_Unassigned",
        "Sev5_Backlog","Total_Cost_Cumulative"),
      names(log)
    )
    return(safe_text(paste(capture.output(
      print(as.data.frame(log[, summary_cols]), row.names = FALSE)
    ), collapse = "\n")))
  }

  # ----------------------------------------------------------
  # CLUSTER SUMMARY
  # ----------------------------------------------------------
  if (query_type == "cluster_summary") {
    cs <- active_result$cluster_summary
    if (is.null(cs) || nrow(cs) == 0) {
      # Build a minimal cluster summary from claims + adjusters
      cl_summary <- merge(
        as.data.frame(table(cl$Cluster), stringsAsFactors = FALSE),
        as.data.frame(table(adj$Working_Cluster), stringsAsFactors = FALSE),
        by.x = "Var1", by.y = "Var1", all = TRUE
      )
      names(cl_summary) <- c("Cluster", "Claims", "Adjusters")
      cl_summary[is.na(cl_summary)] <- 0L
      return(safe_text(paste(capture.output(print(cl_summary, row.names = FALSE)), collapse = "\n")))
    }
    if (!is.null(filters$cluster)) cs <- cs[cs$Cluster == filters$cluster, ]
    if (nrow(cs) == 0) return(empty_msg("cluster summary data"))
    return(safe_text(paste(capture.output(print(as.data.frame(cs), row.names = FALSE)), collapse = "\n")))
  }

  # ----------------------------------------------------------
  # WORKFORCE OVERVIEW
  # ----------------------------------------------------------
  if (query_type == "workforce_overview") {

    if (!is.null(filters$cluster)) {
      adj <- adj[adj$Working_Cluster == filters$cluster, ]
    }
    if (nrow(adj) == 0) return(empty_msg("adjusters", filters$cluster))

    # Cross-tab: cluster x skill
    cluster_skill         <- as.data.frame.matrix(table(adj$Working_Cluster, adj$Skill))
    cluster_skill$Total   <- rowSums(cluster_skill)
    cluster_skill$Cluster <- rownames(cluster_skill)
    cluster_skill         <- cluster_skill[, c("Cluster", setdiff(names(cluster_skill), "Cluster"))]

    result_text <- "WORKFORCE BY CLUSTER AND SKILL:\n"
    result_text <- paste0(result_text,
      paste(capture.output(print(cluster_skill, row.names = FALSE)), collapse = "\n"))

    # Cross-tab: cluster x type
    type_cluster         <- as.data.frame.matrix(table(adj$Working_Cluster, adj$Type))
    type_cluster$Total   <- rowSums(type_cluster)
    type_cluster$Cluster <- rownames(type_cluster)
    type_cluster         <- type_cluster[, c("Cluster", setdiff(names(type_cluster), "Cluster"))]

    result_text <- paste0(result_text, "\n\nWORKFORCE BY CLUSTER AND TYPE:\n",
      paste(capture.output(print(type_cluster, row.names = FALSE)), collapse = "\n"))

    # Optional severity capacity view
    if (!is.null(filters$severity)) {
      sev      <- as.integer(filters$severity)
      min_sk   <- switch(as.character(sev), "5"=4L, "4"=3L, "3"=2L, "2"=1L, "1"=1L, 1L)
      capable  <- adj[adj$Skill >= min_sk, ]

      cap_df        <- as.data.frame(table(capable$Working_Cluster), stringsAsFactors = FALSE)
      names(cap_df)  <- c("Cluster", "Capable_Adjusters")

      sev_cl        <- as.data.frame(table(cl$Cluster[cl$Severity == sev]), stringsAsFactors = FALSE)
      names(sev_cl)  <- c("Cluster", "Claims")

      cap_df <- merge(cap_df, sev_cl, by = "Cluster", all.x = TRUE)
      cap_df$Claims[is.na(cap_df$Claims)] <- 0L
      cap_df$Gap <- cap_df$Claims - cap_df$Capable_Adjusters

      result_text <- paste0(result_text,
        sprintf("\n\nSEV-%d CAPACITY (requires PL%d+):\n", sev, min_sk),
        paste(capture.output(print(cap_df, row.names = FALSE)), collapse = "\n"))
    }

    return(safe_text(result_text))
  }

  # ----------------------------------------------------------
  # BOOST PAIRINGS  (PL0 virtual assistant assignments)
  # ----------------------------------------------------------
  if (query_type == "boost_pairings") {
    if (!("Has_Boost" %in% names(adj)))
      return("Boost pairing data not available in this result. Re-run the scenario.")

    # PL4/5 with a PL0 paired
    boosted <- adj[isTRUE(adj$Has_Boost) | adj$Has_Boost == TRUE, ]
    boosted <- boosted[!is.na(boosted$Boosted_By), ]

    if (!is.null(filters$cluster))
      boosted <- boosted[boosted$Working_Cluster == filters$cluster, ]

    if (nrow(boosted) == 0)
      return(empty_msg("PL0 boost pairings", filters$cluster %||% ""))

    # Join PL0 details
    pl0_info <- adj[adj$Skill == 0L & !is.na(adj$Boosting), ]
    pl0_info <- pl0_info[, c("Adjuster_Id","Working_Cluster","Tour_Start","Tour_End")]
    names(pl0_info) <- c("PL0_Id","PL0_Cluster","PL0_Tour_Start","PL0_Tour_End")

    result_df <- boosted %>%
      left_join(pl0_info, by = c("Boosted_By" = "PL0_Id")) %>%
      select(PL4_5_Id = Adjuster_Id, Skill, Working_Cluster,
             PL0_Id = Boosted_By, PL0_Tour_End,
             Tour_End) %>%
      arrange(Working_Cluster, desc(Skill))

    result_text <- sprintf(
      "PL0 BOOST PAIRINGS\n%d PL4/5 adjusters paired with PL0 virtual assistants\n\n",
      nrow(result_df)
    )

    # Summary by cluster
    cluster_counts <- as.data.frame(table(boosted$Working_Cluster))
    names(cluster_counts) <- c("Cluster", "Paired_PL4_5")
    result_text <- paste0(result_text, "By cluster:\n",
      paste(capture.output(print(cluster_counts, row.names = FALSE)), collapse = "\n"),
      "\n\n")

    if (nrow(result_df) <= 30) {
      result_text <- paste0(result_text, "Pairing details:\n",
        paste(capture.output(print(as.data.frame(result_df), row.names = FALSE)), collapse = "\n"))
    }

    return(safe_text(result_text))
  }

  sprintf("Unknown query_type '%s'. Use: adjusters, adjuster_profile, claims, daily, cluster_summary, workforce_overview, sla_risk, tour_cliff, or boost_pairings.",
          query_type)
}
