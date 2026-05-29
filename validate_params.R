# ============================================================
# PHASE 15.1: validate_params.R (v6 — start=2, tour_length)
# ============================================================
# parse_scenario_json() — JSON string -> R list, with defaults
# validate_params()     — checks each item in each list
# format_validation_errors() — human-readable error output
# ============================================================

library(jsonlite)

VALID_CLUSTERS <- c("C0","C1","C2","C3","C4","C5","C6")

# ============================================================
# PARSE: JSON string -> R list with safe defaults
# ============================================================

parse_scenario_json <- function(json_str) {
  if (is.null(json_str) || nchar(trimws(json_str)) == 0)
    return(list(contractors=list(), tour_extensions=list(), redeployments=list(),
                removals=list(), new_claims=list(), cost_overrides=NULL, sim_days=45L))

  parsed <- tryCatch(fromJSON(json_str, simplifyVector=FALSE),
    error = function(e) { return(list(parse_error=e$message)) })

  if (!is.null(parsed$parse_error))
    return(list(parse_error=parsed$parse_error))

  # Normalize each group with defaults
  norm_contractors <- function(lst) {
    lapply(lst, function(x) list(
      n=as.integer(x$n %||% 0),
      cluster=as.character(x$cluster %||% "C4"),
      skill=as.integer(x$skill %||% 5),
      start=as.integer(x$start %||% 1),         # Day 1 arrival (pre-staged)
      tour_length=as.integer(x$tour_length %||% 8)  # 8-day default tour
    ))
  }
  norm_tour_ext <- function(lst) {
    lapply(lst, function(x) list(
      n=as.integer(x$n %||% 0), days=as.integer(x$days %||% 0),
      skill=as.integer(x$skill %||% 4)))
  }
  norm_redeploy <- function(lst) {
    lapply(lst, function(x) list(
      n=as.integer(x$n %||% 0), from=as.character(x$from %||% "C0"),
      to=as.character(x$to %||% "C0"), skill=as.integer(x$skill %||% 3)))
  }
  norm_removals <- function(lst) {
    lapply(lst, function(x) list(
      n=as.integer(x$n %||% 0), skill=as.integer(x$skill %||% 1),
      cluster=if(is.null(x$cluster)) NULL else as.character(x$cluster)))
  }
  norm_new_claims <- function(lst) {
    lapply(lst, function(x) list(
      n=as.integer(x$n %||% 0), severity=as.integer(x$severity %||% 5),
      cluster=as.character(x$cluster %||% "C4")))
  }

  result <- list(
    contractors     = norm_contractors(parsed$contractors %||% list()),
    tour_extensions = norm_tour_ext(parsed$tour_extensions %||% list()),
    redeployments   = norm_redeploy(parsed$redeployments %||% list()),
    removals        = norm_removals(parsed$removals %||% list()),
    new_claims      = norm_new_claims(parsed$new_claims %||% list()),
    cost_overrides  = parsed$cost_overrides,
    sim_days        = as.integer(parsed$sim_days %||% 45L)
  )

  # Pass through compare mode if present
  if (!is.null(parsed$compare)) result$compare <- parsed$compare

  result
}

# ============================================================
# VALIDATE
# ============================================================

validate_params <- function(params) {
  errors <- character(0)

  if (!is.null(params$parse_error))
    return(list(valid=FALSE, errors=paste("JSON parse error:", params$parse_error), params=params))

  # Contractors
  for (i in seq_along(params$contractors)) {
    g <- params$contractors[[i]]; tag <- sprintf("contractors[%d]", i)
    if (g$n < 0 || g$n > 50)  errors <- c(errors, sprintf(
      "%s: n must be 0-50 (got %d). If you need more than 50, split into two entries with the same cluster and skill — e.g. [{\"n\":50,...},{\"n\":20,...}]",
      tag, g$n))
    if (!g$cluster %in% VALID_CLUSTERS) errors <- c(errors, sprintf("%s: invalid cluster '%s'", tag, g$cluster))
    if (!g$skill %in% c(3L,4L,5L)) errors <- c(errors, sprintf("%s: skill must be 3/4/5", tag))
    if (g$start < 1 || g$start > 30) errors <- c(errors, sprintf("%s: start must be 1-30", tag))
    if (g$tour_length < 1 || g$tour_length > 45) errors <- c(errors, sprintf("%s: tour_length must be 1-45", tag))
  }

  # Tour extensions
  for (i in seq_along(params$tour_extensions)) {
    g <- params$tour_extensions[[i]]; tag <- sprintf("tour_extensions[%d]", i)
    if (g$n < 0)             errors <- c(errors, sprintf("%s: n cannot be negative", tag))
    if (g$days < 0 || g$days > 30) errors <- c(errors, sprintf("%s: days must be 0-30", tag))
  }

  # Redeployments
  for (i in seq_along(params$redeployments)) {
    g <- params$redeployments[[i]]; tag <- sprintf("redeployments[%d]", i)
    if (g$n < 0 || g$n > 200)  errors <- c(errors, sprintf("%s: n must be 0-200", tag))
    if (!g$from %in% VALID_CLUSTERS) errors <- c(errors, sprintf("%s: invalid from '%s'", tag, g$from))
    if (!g$to %in% VALID_CLUSTERS)   errors <- c(errors, sprintf("%s: invalid to '%s'", tag, g$to))
    if (g$from == g$to) errors <- c(errors, sprintf("%s: from and to must differ", tag))
  }

  # Removals
  for (i in seq_along(params$removals)) {
    g <- params$removals[[i]]; tag <- sprintf("removals[%d]", i)
    if (g$n < 0 || g$n > 200) errors <- c(errors, sprintf("%s: n must be 0-200", tag))
    if (!is.null(g$cluster) && !g$cluster %in% VALID_CLUSTERS)
      errors <- c(errors, sprintf("%s: invalid cluster '%s'", tag, g$cluster))
  }

  # New claims
  for (i in seq_along(params$new_claims)) {
    g <- params$new_claims[[i]]; tag <- sprintf("new_claims[%d]", i)
    if (g$n < 0 || g$n > 500) errors <- c(errors, sprintf("%s: n must be 0-500", tag))
    if (!g$severity %in% 1:5) errors <- c(errors, sprintf("%s: severity must be 1-5", tag))
    if (!g$cluster %in% VALID_CLUSTERS) errors <- c(errors, sprintf("%s: invalid cluster", tag))
  }

  # Cost overrides
  if (!is.null(params$cost_overrides)) {
    co <- params$cost_overrides
    if (!is.null(co$contractor) && (co$contractor < 100 || co$contractor > 2000))
      errors <- c(errors, "cost_overrides.contractor: must be 100-2000")
    if (!is.null(co$deployed) && (co$deployed < 100 || co$deployed > 2000))
      errors <- c(errors, "cost_overrides.deployed: must be 100-2000")
  }

  list(valid=length(errors)==0L, errors=errors, params=params)
}

format_validation_errors <- function(v) {
  if (v$valid) return(NULL)
  paste0("VALIDATION FAILED:\n", paste0("  - ", v$errors, collapse="\n"))
}
