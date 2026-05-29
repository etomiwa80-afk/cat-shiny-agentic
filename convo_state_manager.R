# ============================================================
# PHASE 15.4: convo_state_manager.R (v6 — per-scenario net)
# ============================================================
# v6 changes:
#   - Each scenario stores its own label + net_benefit
#   - get_cumulative_cost compares vs baseline (not incremental)
#   - get_stack_summary shows per-scenario net vs baseline
# ============================================================

MAX_HISTORY <- 10
MAX_REJECTIONS <- 3

# ============================================================
# DEFAULT PARAMS
# ============================================================

default_params <- function() {
  list(
    contractors=list(), tour_extensions=list(), redeployments=list(),
    removals=list(), new_claims=list(), cost_overrides=NULL, sim_days=45L
  )
}

# ============================================================
# INIT / RESET
# ============================================================

init_convo_state <- function(baseline_metrics) {
  list(
    baseline_state     = baseline_metrics,
    current_state      = baseline_metrics,
    scenario_stack     = list(),
    history            = list(),
    rejection_count    = 0L,
    accumulated_params = default_params()
  )
}

reset_state <- function(state) {
  state$current_state      <- state$baseline_state
  state$scenario_stack     <- list()
  state$history            <- list()
  state$rejection_count    <- 0L
  state$accumulated_params <- default_params()
  cat("State reset to baseline.\n")
  state
}

# ============================================================
# HISTORY
# ============================================================

add_to_history <- function(state, role, content) {
  state$history <- c(state$history, list(list(role=role, content=content)))
  if (length(state$history) > MAX_HISTORY)
    state$history <- tail(state$history, MAX_HISTORY)
  state
}

# ============================================================
# MERGE PARAMS (server-side chaining)
# ============================================================
# Rules:
#   contractors:     key by cluster. Same cluster = replace. New = append.
#   tour_extensions: key by skill. Same skill = replace. New = append.
#   redeployments:   key by from->to. Same pair = replace. New = append.
#   removals:        key by cluster (or "ALL" if NULL). Same = replace. New = append.
#   new_claims:      always append (surge is cumulative).
#   cost_overrides:  replace entirely.
#   sim_days:        take new if provided.

merge_params <- function(accumulated, new_params) {
  merged <- accumulated

  # Contractors: key by cluster + skill (so PL5@C3 and PL4@C3 are separate entries)
  if (length(new_params$contractors) > 0) {
    for (grp in new_params$contractors) {
      key <- paste0(grp$cluster, "_PL", grp$skill)
      idx <- which(sapply(merged$contractors, function(x) paste0(x$cluster, "_PL", x$skill)) == key)
      if (length(idx) > 0)
        merged$contractors[[idx[1]]] <- grp
      else
        merged$contractors <- c(merged$contractors, list(grp))
    }
  }

  # Tour extensions: key by skill
  if (length(new_params$tour_extensions) > 0) {
    for (grp in new_params$tour_extensions) {
      key <- grp$skill
      idx <- which(sapply(merged$tour_extensions, function(x) x$skill) == key)
      if (length(idx) > 0)
        merged$tour_extensions[[idx[1]]] <- grp
      else
        merged$tour_extensions <- c(merged$tour_extensions, list(grp))
    }
  }

  # Redeployments: key by from->to
  if (length(new_params$redeployments) > 0) {
    for (grp in new_params$redeployments) {
      key <- paste0(grp$from, "->", grp$to)
      idx <- which(sapply(merged$redeployments, function(x) paste0(x$from,"->",x$to)) == key)
      if (length(idx) > 0)
        merged$redeployments[[idx[1]]] <- grp
      else
        merged$redeployments <- c(merged$redeployments, list(grp))
    }
  }

  # Removals: key by cluster (NULL = "ALL")
  if (length(new_params$removals) > 0) {
    for (grp in new_params$removals) {
      key <- grp$cluster %||% "ALL"
      idx <- which(sapply(merged$removals, function(x) x$cluster %||% "ALL") == key)
      if (length(idx) > 0)
        merged$removals[[idx[1]]] <- grp
      else
        merged$removals <- c(merged$removals, list(grp))
    }
  }

  # New claims: always append
  if (length(new_params$new_claims) > 0)
    merged$new_claims <- c(merged$new_claims, new_params$new_claims)

  # Cost overrides: replace
  if (!is.null(new_params$cost_overrides))
    merged$cost_overrides <- new_params$cost_overrides

  # Sim days: take new if non-default
  if (!is.null(new_params$sim_days) && new_params$sim_days != 45L)
    merged$sim_days <- new_params$sim_days

  merged
}

# ============================================================
# APPROVE / REJECT / UNDO
# ============================================================

approve_scenario <- function(state, new_params, scenario_result,
                             tool_result, interpretation, label="") {
  state$accumulated_params <- merge_params(state$accumulated_params, new_params)

  # Per-scenario net vs baseline: penalty saved minus extra operational cost
  penalty_saved <- (state$baseline_state$penalty_cost %||% 0) -
                   (scenario_result$penalty_cost %||% 0)
  extra_ops <- (scenario_result$deploy_cost %||% 0) +
               (scenario_result$contractor_cost %||% 0) -
               (state$baseline_state$deploy_cost %||% 0)
  net_vs_baseline <- penalty_saved - extra_ops

  state$scenario_stack <- c(state$scenario_stack, list(list(
    params          = new_params,
    merged_params   = state$accumulated_params,
    scenario_result = scenario_result,
    result          = tool_result,
    interpretation  = interpretation,
    label           = label,
    net_vs_baseline = net_vs_baseline,
    status          = "APPROVED"
  )))
  state$current_state   <- scenario_result
  state$rejection_count <- 0L
  state
}

reject_scenario <- function(state, new_params, reason="Sponsor rejected", label="") {
  state$scenario_stack <- c(state$scenario_stack, list(list(
    params=new_params, status="REJECTED", reason=reason, label=label
  )))
  state$rejection_count <- state$rejection_count + 1L
  if (state$rejection_count >= MAX_REJECTIONS)
    cat("WARNING: 3 consecutive rejections. Consider resetting.\n")
  state
}

undo_last <- function(state) {
  n <- length(state$scenario_stack)
  if (n == 0) { cat("Nothing to undo.\n"); return(state) }

  # Find the last APPROVED entry (not a rejection)
  approved_indices <- which(sapply(state$scenario_stack, function(s) s$status == "APPROVED"))

  if (length(approved_indices) == 0) {
    cat("No approved scenarios to undo. Clearing rejection records.\n")
    state$scenario_stack <- list()
    state$accumulated_params <- default_params()
    state$current_state <- state$baseline_state
    return(state)
  }

  last_approved_idx <- approved_indices[length(approved_indices)]

  # Remove that approved entry (keep everything else including other rejections)
  state$scenario_stack <- state$scenario_stack[-last_approved_idx]

  # Rebuild accumulated_params from remaining approved scenarios
  state$accumulated_params <- default_params()
  for (s in state$scenario_stack) {
    if (s$status == "APPROVED")
      state$accumulated_params <- merge_params(state$accumulated_params, s$params)
  }

  # Revert current_state to the previous approved result (or baseline)
  remaining_approved <- Filter(function(s) s$status == "APPROVED", state$scenario_stack)
  if (length(remaining_approved) > 0)
    state$current_state <- remaining_approved[[length(remaining_approved)]]$scenario_result
  else
    state$current_state <- state$baseline_state

  cat("Last approved scenario undone.\n")
  state
}

# ============================================================
# HELPERS
# ============================================================

get_stack_summary <- function(state) {
  if (length(state$scenario_stack) == 0)
    return(data.frame(Scenario=character(), Status=character(),
                      Net_vs_Baseline=character(), stringsAsFactors=FALSE))
  do.call(rbind, lapply(seq_along(state$scenario_stack), function(i) {
    s <- state$scenario_stack[[i]]

    # Use stored label if available, otherwise build from params
    lbl <- s$label %||% ""
    if (nchar(lbl) == 0) {
      p <- s$params
      parts <- character(0)
      if (length(p$contractors) > 0)
        parts <- c(parts, paste(sapply(p$contractors, function(g) sprintf("%d PL%d@%s",g$n,g$skill,g$cluster)), collapse="+"))
      if (length(p$tour_extensions) > 0)
        parts <- c(parts, paste(sapply(p$tour_extensions, function(g) sprintf("ext%d+%dd",g$n,g$days)), collapse="+"))
      if (length(p$redeployments) > 0)
        parts <- c(parts, paste(sapply(p$redeployments, function(g) sprintf("mv%d %s->%s",g$n,g$from,g$to)), collapse="+"))
      if (length(p$removals) > 0)
        parts <- c(parts, paste(sapply(p$removals, function(g) sprintf("rm%d@%s",g$n,g$cluster%||%"all")), collapse="+"))
      if (length(p$new_claims) > 0)
        parts <- c(parts, paste(sapply(p$new_claims, function(g) sprintf("+%dSev%d@%s",g$n,g$severity,g$cluster)), collapse="+"))
      if (length(parts)==0) parts <- "baseline"
      lbl <- paste(parts, collapse=" | ")
    }

    # Net vs baseline for approved scenarios
    nvb <- if(!is.null(s$net_vs_baseline)) sprintf("$%s",format(s$net_vs_baseline,big.mark=",")) else "-"

    data.frame(Scenario=lbl, Status=s$status, Net_vs_Baseline=nvb, stringsAsFactors=FALSE)
  }))
}

# Cumulative: compare latest approved scenario total cost vs baseline total cost
get_cumulative_cost <- function(state) {
  approved <- Filter(function(s) s$status=="APPROVED", state$scenario_stack)
  n <- length(approved)
  if (n==0) return(list(n_approved=0, total_net_vs_baseline=0))
  latest <- approved[[n]]

  baseline_penalty <- state$baseline_state$penalty_cost %||% 0
  scenario_penalty <- latest$scenario_result$penalty_cost %||% 0
  penalty_saved <- baseline_penalty - scenario_penalty

  baseline_ops <- state$baseline_state$deploy_cost %||% 0
  scenario_ops <- (latest$scenario_result$deploy_cost %||% 0) +
                  (latest$scenario_result$contractor_cost %||% 0)
  extra_ops <- scenario_ops - baseline_ops

  list(
    n_approved = n,
    penalty_saved = penalty_saved,
    extra_ops_cost = extra_ops,
    total_net_vs_baseline = penalty_saved - extra_ops
  )
}
