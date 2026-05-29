# ============================================================
# PHASE 15.5: console_demo.R (v6 — 6 tests)
# ============================================================
# Test 1: Happy path — single cluster contractors
# Test 2: Chained — add tour extension on top
# Test 3: Multi-cluster — contractors to C3 AND C4
# Test 4: Info question — text only, no tool
# Test 5: Removal stress test — C3 loses 10 adjusters
# Test 6: Rejection path
# ============================================================

cat("\n===========================================================\n")
cat("PHASE 15 CONSOLE DEMO v6 — FULL PIPELINE\n")
cat("===========================================================\n\n")

# ---- SOURCE ----
# UPDATE THESE PATHS to match your local file locations
cat("Loading modules...\n")
source("simulation_engine.R")
source("validate_params.R")
source("build_context.R")
source("ai_recommend_execute.R")
source("convo_state_manager.R")

# ---- API KEY ----
if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  cat("ERROR: Set your API key first:\n")
  cat('  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")\n')
  stop("Missing API key")
}
cat("API key: set\n")

# ---- INIT ----
state <- init_convo_state(baseline_result)
payload <- build_context(baseline_result, state)
cat(sprintf("System prompt: %d chars\n\n", payload$char_count))

# ============================================================
# HELPER: run one turn of the pipeline
# ============================================================

run_turn <- function(question, state, auto_approve=TRUE) {
  cat(sprintf("\nSPONSOR: %s\n\n", question))

  state <- add_to_history(state, "user", question)
  payload <- build_context(baseline_result, state)

  cat("Calling Claude...\n")
  rec <- ai_recommend(question, payload)

  if (rec$type == "undo") {
    cat("TYPE: undo\n")
    state <- undo_last(state)
    state <- add_to_history(state, "assistant", "Last scenario undone.")
    return(list(state=state, rec=rec))
  }

  if (rec$type == "text") {
    cat("TYPE: text\n")
    cat("RESPONSE:\n", rec$message, "\n")
    state <- add_to_history(state, "assistant", rec$message)
    return(list(state=state, rec=rec))
  }

  # Tool call
  cat("TYPE: tool_call\n")
  cat("SCENARIO_JSON:", rec$scenario_json, "\n")
  cat("RATIONALE:", rec$rationale, "\n")
  cat("PROJECTED: $", format(rec$projected_savings, big.mark=","), "\n\n")

  # Parse + validate
  parsed <- parse_scenario_json(rec$scenario_json)
  if (!is.null(parsed$parse_error)) {
    cat("JSON PARSE ERROR:", parsed$parse_error, "\n")
    state <- add_to_history(state, "assistant", paste("Parse error:", parsed$parse_error))
    return(list(state=state, rec=rec))
  }

  v <- validate_params(parsed)
  if (!v$valid) {
    cat(format_validation_errors(v), "\n")
    state <- add_to_history(state, "assistant", format_validation_errors(v))
    return(list(state=state, rec=rec))
  }

  # Build scenario label BEFORE merging (this is what was NEW in this turn)
  scenario_label <- build_scenario_label(parsed)
  cat("SCENARIO:", scenario_label, "\n")

  # Check for compare mode
  if (!is.null(parsed$compare)) {
    cat("COMPARE MODE: running", length(parsed$compare), "strategies\n")
    results <- compare_strategies(parsed$compare, baseline_result)
    state <- add_to_history(state, "assistant", "Comparison complete.")
    return(list(state=state, rec=rec, compare_results=results))
  }

  # Merge with accumulated params
  run_params <- merge_params(state$accumulated_params, parsed)

  cat("MERGED PARAMS:\n")
  if (length(run_params$contractors) > 0)
    cat("  contractors:", paste(sapply(run_params$contractors,
      function(g) sprintf("%d PL%d@%s", g$n, g$skill, g$cluster)), collapse=" + "), "\n")
  if (length(run_params$tour_extensions) > 0)
    cat("  tour_ext:", paste(sapply(run_params$tour_extensions,
      function(g) sprintf("%d +%dd (PL%d+)", g$n, g$days, g$skill)), collapse=" + "), "\n")
  if (length(run_params$redeployments) > 0)
    cat("  redeploy:", paste(sapply(run_params$redeployments,
      function(g) sprintf("%d %s->%s", g$n, g$from, g$to)), collapse=" + "), "\n")
  if (length(run_params$removals) > 0)
    cat("  removals:", paste(sapply(run_params$removals,
      function(g) sprintf("%d@%s", g$n, g$cluster %||% "all")), collapse=" + "), "\n")
  if (length(run_params$new_claims) > 0)
    cat("  new_claims:", paste(sapply(run_params$new_claims,
      function(g) sprintf("+%d Sev-%d@%s", g$n, g$severity, g$cluster)), collapse=" + "), "\n")
  cat("\n")

  # Run scenario
  cat("Running simulation...\n")
  scenario <- run_scenario(run_params)
  compare_scenarios(baseline_result, scenario, question)

  # Interpret — pass scenario description so Claude connects action to outcome
  tool_result <- build_tool_result(state$current_state, scenario)
  interp <- ai_execute(tool_result, payload, scenario_label)
  cat("\nCLAUDE INTERPRETATION:\n", interp$interpretation, "\n")

  if (auto_approve) {
    state <- approve_scenario(state, parsed, scenario, tool_result,
                              interp$interpretation, label=scenario_label)
    state <- add_to_history(state, "assistant", interp$interpretation)
    cat("\nAPPROVED. Stack:\n")
    print(get_stack_summary(state))
  }

  list(state=state, rec=rec, scenario=scenario, tool_result=tool_result)
}

# ============================================================
# TEST 1: Single cluster contractors
# ============================================================

cat("===========================================================\n")
cat("TEST 1: Add 20 contractors to Tennessee\n")
cat("===========================================================\n")

t1 <- run_turn("What if we add 20 contractors to Tennessee?", state)
state <- t1$state

# ============================================================
# TEST 2: Chained — add tour extension ON TOP
# ============================================================

cat("\n\n===========================================================\n")
cat("TEST 2: Chained — extend tours on top of contractors\n")
cat("===========================================================\n")

t2 <- run_turn("On top of that, also extend the top 30 PL4+ tours by 7 days.", state)
state <- t2$state

# Verify merge
cat("\nCHAIN CHECK:\n")
cat("  contractors in accumulated:", length(state$accumulated_params$contractors), "\n")
cat("  tour_ext in accumulated:", length(state$accumulated_params$tour_extensions), "\n")
if (length(state$accumulated_params$contractors) == 0)
  cat("  WARNING: Contractors lost in chain!\n")

# ============================================================
# TEST 3: Multi-cluster contractors
# ============================================================

cat("\n\n===========================================================\n")
cat("TEST 3: Multi-cluster — contractors to C3 AND C4\n")
cat("===========================================================\n")

# Reset to baseline for clean test
state <- reset_state(state)
t3 <- run_turn("Add 20 contractors to Tennessee and 15 to Maryland, all PL5.", state)
state <- t3$state

# ============================================================
# TEST 4: Info question (no tool call)
# ============================================================

cat("\n\n===========================================================\n")
cat("TEST 4: Info question\n")
cat("===========================================================\n")

t4 <- run_turn("Which cluster is in the worst shape right now?", state)
state <- t4$state

if (t4$rec$type == "text") cat("CORRECT: text response, no tool call\n")
else cat("UNEXPECTED: got tool call for info question\n")

# ============================================================
# TEST 5: Removal stress test
# ============================================================

cat("\n\n===========================================================\n")
cat("TEST 5: Remove adjusters from cluster\n")
cat("===========================================================\n")

t5 <- run_turn("What if C3 Tennessee loses 10 adjusters?", state)
state <- t5$state

# ============================================================
# TEST 6: Rejection
# ============================================================

cat("\n\n===========================================================\n")
cat("TEST 6: Rejection path\n")
cat("===========================================================\n")

question6 <- "Add 50 contractors to C5 Mississippi."
cat(sprintf("\nSPONSOR: %s\n", question6))
state <- add_to_history(state, "user", question6)
payload <- build_context(baseline_result, state)

cat("Calling Claude...\n")
rec6 <- ai_recommend(question6, payload)

if (rec6$type == "tool_call") {
  cat("TYPE: tool_call\n")
  cat("SCENARIO_JSON:", rec6$scenario_json, "\n\n")

  parsed6 <- parse_scenario_json(rec6$scenario_json)
  label6 <- build_scenario_label(parsed6)
  cat("SCENARIO:", label6, "\n")
  cat("SPONSOR: No, that's too expensive. Rejected.\n")
  state <- reject_scenario(state, parsed6, "Too expensive for C5", label=label6)
  state <- add_to_history(state, "assistant", "Scenario rejected.")
  cat("REJECTED. Stack:\n")
  print(get_stack_summary(state))
} else {
  cat("TYPE:", rec6$type, "\n", rec6$message, "\n")
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n\n===========================================================\n")
cat("PIPELINE SUMMARY\n")
cat("===========================================================\n\n")

cat("Scenario stack:\n")
print(get_stack_summary(state))

cum <- get_cumulative_cost(state)
cat(sprintf("\nApproved: %d\n", cum$n_approved))
cat(sprintf("Penalty saved vs baseline: $%s\n", format(cum$penalty_saved, big.mark=",")))
cat(sprintf("Extra ops cost: $%s\n", format(cum$extra_ops_cost, big.mark=",")))
cat(sprintf("Net vs baseline: $%s\n", format(cum$total_net_vs_baseline, big.mark=",")))

cat(sprintf("History: %d messages\n", length(state$history)))
cat(sprintf("Rejections: %d\n", state$rejection_count))

cat("\nAccumulated params:\n")
if (length(state$accumulated_params$contractors) > 0)
  cat("  contractors:", paste(sapply(state$accumulated_params$contractors,
    function(g) sprintf("%d PL%d@%s", g$n, g$skill, g$cluster)), collapse=", "), "\n")
if (length(state$accumulated_params$tour_extensions) > 0)
  cat("  tour_ext:", paste(sapply(state$accumulated_params$tour_extensions,
    function(g) sprintf("%d+%dd", g$n, g$days)), collapse=", "), "\n")
if (length(state$accumulated_params$redeployments) > 0)
  cat("  redeploy:", paste(sapply(state$accumulated_params$redeployments,
    function(g) sprintf("%d %s->%s", g$n, g$from, g$to)), collapse=", "), "\n")

cat("\n===========================================================\n")
cat("ALL 6 TESTS COMPLETE\n")
cat("===========================================================\n")
