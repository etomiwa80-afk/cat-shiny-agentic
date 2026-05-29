# ============================================================
# PHASE 15.3: ai_recommend_execute.R (v7 — two-tool system)
# ============================================================
# ai_recommend()       — sponsor question -> tool call or text
# ai_execute()         — interpret scenario results
# ai_interpret_query() — interpret query_data results
#
# v7 changes:
#   - Second tool: query_data (read-only data lookup)
#   - filters passed as JSON string for ellmer compatibility
#   - ai_interpret_query() for no-tool text completion
# ============================================================

library(ellmer)
library(jsonlite)
CLAUDE_MODEL <- "claude-sonnet-4-20250514"

# ============================================================
# ai_recommend — sponsor question -> tool call or text
# ============================================================

ai_recommend <- function(user_msg, context_payload) {

  # Capture environments for each tool
  scenario_env <- new.env(parent = emptyenv())
  scenario_env$params <- NULL

  query_env <- new.env(parent = emptyenv())
  query_env$params <- NULL

  # Tool 1: run_cat_scenario (writes to scenario_env)
  scenario_fn <- function(scenario_json = "", rationale = "", projected_savings = 0) {
    scenario_env$params <- list(
      scenario_json     = scenario_json,
      rationale         = rationale,
      projected_savings = projected_savings
    )
    "INTERCEPTED"
  }

  # Tool 2: query_data (writes to query_env)
  # filters passed as a JSON string for ellmer compatibility
  query_fn <- function(query_type = "", filters_json = "", group_by = "") {
    filters <- tryCatch(
      if (nchar(trimws(filters_json)) > 0) fromJSON(filters_json, simplifyVector = TRUE) else list(),
      error = function(e) list()
    )
    query_env$params <- list(
      query_type = query_type,
      filters    = filters,
      group_by   = if (nchar(trimws(group_by)) > 0) group_by else NULL
    )
    "INTERCEPTED"
  }

  NO_MARKDOWN <- paste0(
    "CRITICAL FORMATTING RULE: Do NOT use markdown in any response. ",
    "No asterisks for bold (**text**), no asterisks for italic (*text*), ",
    "no hash headers (#), no bullet hyphens (-). ",
    "Use CAPS for emphasis. Use line breaks for structure. ",
    "Use plain ASCII tables (pipes and dashes) for breakdowns. ",
    "The UI renders text as-is — markdown symbols appear literally.\n\n"
  )

  chat <- chat_anthropic(
    system_prompt = paste0(NO_MARKDOWN, context_payload$system_prompt),
    model         = CLAUDE_MODEL
  )

  # Register Tool 1
  chat$register_tool(tool(
    scenario_fn,
    name        = "run_cat_scenario",
    description = context_payload$tools[[1]]$description,
    arguments   = list(
      scenario_json     = type_string(description = "JSON object with scenario params"),
      rationale         = type_string(description = "Cite clusters + dollar math"),
      projected_savings = type_number(description = "Net dollar savings (single number)")
    )
  ))

  # Register Tool 2
  chat$register_tool(tool(
    query_fn,
    name        = "query_data",
    description = context_payload$tools[[2]]$description,
    arguments   = list(
      query_type   = type_string(description = "One of: adjusters, claims, daily, cluster_summary"),
      filters_json = type_string(description = paste0(
        'Optional JSON string with filter fields: ',
        '{"cluster":"C4","skill":4,"severity":5,"day":5,',
        '"type":"local","status":"Working","pre_deployment":true}. ',
        'Omit fields you do not need. Pass empty string if no filters.'
      )),
      group_by     = type_string(description = "Optional: cluster, skill, severity, or type. Pass empty string if not grouping.")
    )
  ))

  response <- tryCatch(
    chat$chat(user_msg, echo = FALSE),
    error = function(e) paste("API_ERROR:", e$message)
  )

  # Check for API error before anything else
  if (is.character(response) && startsWith(response, "API_ERROR:")) {
    return(list(
      type    = "error",
      message = paste0(
        "The AI service returned an error: ",
        sub("API_ERROR: ", "", response),
        "\nPlease try again or rephrase your question."
      )
    ))
  }

  # Check: query_data tool was called
  if (!is.null(query_env$params)) {
    p <- query_env$params
    return(list(
      type       = "query",
      query_type = p$query_type,
      filters    = p$filters,
      group_by   = p$group_by
    ))
  }

  # Check: run_cat_scenario tool was called
  if (!is.null(scenario_env$params)) {
    return(list(
      type              = "tool_call",
      scenario_json     = scenario_env$params$scenario_json,
      rationale         = scenario_env$params$rationale,
      projected_savings = scenario_env$params$projected_savings,
      raw_response      = response
    ))
  }

  # Check for UNDO_LAST in text response
  if (grepl("UNDO_LAST", response, fixed = TRUE)) {
    return(list(type = "undo", message = response))
  }

  list(type = "text", message = response, params = NULL)
}

# ============================================================
# build_scenario_label — human-readable description of params
# ============================================================

build_scenario_label <- function(parsed_params) {
  parts <- character(0)
  if (length(parsed_params$contractors) > 0)
    parts <- c(parts, paste(sapply(parsed_params$contractors,
      function(g) sprintf("%d PL%d contractors -> %s", g$n, g$skill, g$cluster)), collapse = " + "))
  if (length(parsed_params$tour_extensions) > 0)
    parts <- c(parts, paste(sapply(parsed_params$tour_extensions,
      function(g) sprintf("Extend %d PL%d+ by %d days", g$n, g$skill, g$days)), collapse = " + "))
  if (length(parsed_params$redeployments) > 0)
    parts <- c(parts, paste(sapply(parsed_params$redeployments,
      function(g) sprintf("Move %d PL%d %s->%s", g$n, g$skill, g$from, g$to)), collapse = " + "))
  if (length(parsed_params$removals) > 0)
    parts <- c(parts, paste(sapply(parsed_params$removals,
      function(g) sprintf("Remove %d PL%d from %s", g$n, g$skill, g$cluster %||% "all")), collapse = " + "))
  if (length(parsed_params$new_claims) > 0)
    parts <- c(parts, paste(sapply(parsed_params$new_claims,
      function(g) sprintf("+%d Sev-%d claims in %s", g$n, g$severity, g$cluster)), collapse = " + "))
  if (length(parts) == 0) return("No changes")
  paste(parts, collapse = " | ")
}

# ============================================================
# ai_execute — interpret scenario results
# ============================================================

ai_execute <- function(tool_result, context_payload, scenario_description = "") {

  interpret_prompt <- sprintf(paste0(
    "You ran this scenario: %s\n\n",
    "PREVIOUS STATE:\n",
    "  Sev-5 missed: %d | Sev-4 missed: %d | Sev-3 missed: %d | Sev-2 missed: %d | Sev-1 missed: %d\n",
    "  Total cost: $%s\n\n",
    "NEW STATE:\n",
    "  Sev-5 missed: %d | Sev-4 missed: %d | Sev-3 missed: %d | Sev-2 missed: %d | Sev-1 missed: %d\n",
    "  Total cost: $%s\n\n",
    "DELTA:\n",
    "  Sev-5: %+d | Sev-4: %+d | Sev-3: %+d | Sev-2: %+d | Sev-1: %+d\n",
    "  Penalty saved: $%s | Extra ops cost: $%s | NET BENEFIT: $%s\n\n",
    "Respond with exactly 3 short paragraphs:\n",
    "1. What improved — cite the specific action, severity counts, and dollar savings\n",
    "2. What didn't move and why — reference capacity constraints, timing, or cascade behavior\n",
    "3. What to try next — one specific recommendation with cluster, count, and estimated impact"
  ),
    scenario_description,
    tool_result$prev$sev5_missed, tool_result$prev$sev4_missed,
    tool_result$prev$sev3_missed %||% 0, tool_result$prev$sev2_missed %||% 0, tool_result$prev$sev1_missed %||% 0,
    format(tool_result$prev$total_cost, big.mark = ","),
    tool_result$new$sev5_missed, tool_result$new$sev4_missed,
    tool_result$new$sev3_missed %||% 0, tool_result$new$sev2_missed %||% 0, tool_result$new$sev1_missed %||% 0,
    format(tool_result$new$total_cost, big.mark = ","),
    tool_result$delta$sev5, tool_result$delta$sev4,
    tool_result$delta$sev3 %||% 0, tool_result$delta$sev2 %||% 0, tool_result$delta$sev1 %||% 0,
    format(tool_result$delta$penalty_saved, big.mark = ","),
    format(tool_result$delta$extra_ops,     big.mark = ","),
    format(tool_result$delta$net_benefit,   big.mark = ",")
  )

  NO_MARKDOWN_EXEC <- paste0(
    "CRITICAL FORMATTING RULE: Do NOT use markdown. No asterisks, no bold, no headers, no bullet hyphens. ",
    "Use CAPS for emphasis. Plain text only. ASCII tables for breakdowns.\n\n"
  )

  chat <- chat_anthropic(
    system_prompt = paste0(NO_MARKDOWN_EXEC, context_payload$system_prompt),
    model         = CLAUDE_MODEL
  )

  interpretation <- tryCatch(
    chat$chat(interpret_prompt, echo = FALSE),
    error = function(e) {
      paste0("Interpretation unavailable (", e$message,
             "). Net benefit: $", format(tool_result$delta$net_benefit, big.mark = ","))
    }
  )

  list(success = TRUE, interpretation = interpretation)
}

# ============================================================
# ai_interpret_query — lightweight plain-text interpretation
#                      (no tools, no full 8KB system prompt)
# ============================================================

ai_interpret_query <- function(prompt, context_payload) {
  light_prompt <- paste0(
    "You are a Travelers Insurance CAT claims operations advisor. ",
    "Answer data lookup questions directly using specific numbers from the data provided. ",
    "Do NOT use markdown — no asterisks, no bold, no bullet hyphens, no headers. ",
    "FORMATTING RULE: When the answer involves a breakdown with 3 or more rows (severity counts, ",
    "skill counts, cluster counts, etc.), present it as a plain ASCII table using dashes and pipes. ",
    "Example format:\n",
    "Severity | Count | Notes\n",
    "---------|-------|------\n",
    "5        | 47    | On-site only\n",
    "Use consistent column spacing. For single-value answers, write plain prose — no table needed. ",
    "Reference cluster names when helpful: ",
    "C0=Carolinas, C1=Alabama/Georgia, C2=Northeast, C3=Tennessee/Kentucky, ",
    "C4=Maryland/DMV, C5=Mississippi/Louisiana, C6=New England. ",
    "Be concise and direct — answer the question, do not narrate the raw data."
  )

  chat <- chat_anthropic(
    system_prompt = light_prompt,
    model         = CLAUDE_MODEL
  )

  tryCatch(
    chat$chat(prompt, echo = FALSE),
    error = function(e) paste("Interpretation unavailable:", e$message)
  )
}
