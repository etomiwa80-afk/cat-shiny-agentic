# ============================================================
# PHASE 15.2: build_context.R (v6 — full rewrite)
# ============================================================
# System prompt aligned to v6 engine:
#   - 8-day contractor tours (not 45)
#   - PL4 on Sev-5 with PL0 boost (6d < 7d SLA)
#   - Per-cluster cascade lock (not global)
#   - Time-based cascade unlock (Day 8/15/22)
#   - Virtual -> OUT pool only (in-cluster never virtual)
#   - Two-tier deployment (PL4/5 Tier 1 + PL3 Tier 2)
#   - Contractors arrive Day 2 (travel)
#   - Tour extensions = dead money (proven)
# ============================================================

library(jsonlite)
library(tidyr)

# ============================================================
# TOOL SCHEMA
# ============================================================

TOOL_SCHEMA <- list(
  list(
    name = "run_cat_scenario",
    description = paste0(
      "Run a CAT allocation scenario. Send scenario_json with any combination of: ",
      "contractors, tour_extensions, redeployments, removals, new_claims, cost_overrides. ",
      "Multiple entries per lever allowed (e.g. contractors in C3 AND C4 in one call). ",
      "For chained what-ifs, send ONLY what's new — system auto-merges with prior approved params."
    ),
    input_schema = list(
      type = "object",
      properties = list(
        scenario_json = list(
          type = "string",
          description = paste0(
            'JSON object with optional keys: ',
            'contractors: [{n, cluster, skill, start, tour_length}], ',
            'tour_extensions: [{n, days, skill}], ',
            'redeployments: [{n, from, to, skill}], ',
            'removals: [{n, skill, cluster}], ',
            'new_claims: [{n, severity, cluster}], ',
            'cost_overrides: {contractor, deployed, local}, ',
            'sim_days: integer. ',
            'Example: {"contractors":[{"n":20,"cluster":"C3","skill":5}]}'
          )
        ),
        rationale = list(type="string",
          description="Cite specific clusters, gap numbers, and dollar math from the data."),
        projected_savings = list(type="number",
          description="Approximate net dollar savings. Show math in rationale. Single number. Actual simulation may differ 20-40%.")
      ),
      required = c("scenario_json","rationale","projected_savings")
    )
  ),
  list(
    name = "query_data",
    description = paste0(
      "Look up adjuster, claims, or daily simulation data. Read-only — does not change the simulation. ",
      "Use for questions about specific adjusters, pre/post-deployment counts, claim assignments, ",
      "daily backlogs, or adjuster workloads. ",
      "Examples: 'How many PL4 adjusters are in C4?', ",
      "'Which Sev-5 claims are unassigned on Day 5?', ",
      "'Show me all contractors in C3', ",
      "'What is the Sev-5 backlog by cluster on Day 3?'"
    ),
    input_schema = list(
      type = "object",
      properties = list(
        query_type = list(
          type = "string",
          enum = list("adjusters","claims","daily","cluster_summary",
                      "workforce_overview","adjuster_profile","sla_risk","tour_cliff","boost_pairings"),
          description = paste0(
            "adjusters=filter roster | claims=filter claims | daily=day log | ",
            "cluster_summary=outcomes by cluster | workforce_overview=cross-tab cluster x skill/type | ",
            "adjuster_profile=full profile for one adjuster (needs Adjuster_Id) | ",
            "sla_risk=claims approaching deadline (needs day) | ",
            "tour_cliff=adjusters finishing tour soon (needs day)"
          )
        ),
        filters = list(
          type = "object",
          description = "Optional filters to narrow results",
          properties = list(
            cluster        = list(type="string",  description="Cluster ID e.g. C4"),
            skill          = list(type="integer", description="Skill level 0-5"),
            severity       = list(type="integer", description="Claim severity 1-5"),
            day            = list(type="integer", description="Simulation day 1-45"),
            type           = list(type="string",  enum=list("local","deployed","contractor"),
                                  description="Adjuster type"),
            status         = list(type="string",  description="Adjuster: Working/Available/Gone. Claims: Unassigned/In Progress/Completed"),
            pre_deployment = list(type="boolean", description="If true, filter by Home_Cluster instead of Working_Cluster"),
            Adjuster_Id        = list(type="string",  description="Numeric adjuster ID e.g. '31104564'. Use for adjuster_profile or to filter claims by adjuster."),
            assigned       = list(type="boolean", description="If true return only adjusters with an active claim; false returns idle adjusters."),
            tour_ending    = list(type="boolean", description="If true, return only adjusters whose tour ends within day_range days of current day (default 3). No day filter required — uses max simulation day automatically."),
            day_range      = list(type="integer", description="Look-ahead window in days for sla_risk, tour_cliff, and tour_ending. Default 5 for sla_risk/tour_cliff, default 3 for tour_ending.")
          )
        ),
        group_by = list(
          type = "string",
          enum = list("cluster","skill","severity","type","cluster_skill","cluster_type"),
          description = "Group results by this field. cluster_skill and cluster_type return cross-tabulations."
        )
      ),
      required = list("query_type")
    )
  )
)

# ============================================================
# DYNAMIC DATA BUILDERS
# ============================================================

build_claims_table <- function(claims) {
  tbl <- claims %>% group_by(Cluster,Severity) %>% summarise(n=n(),.groups="drop") %>%
    pivot_wider(names_from=Severity,values_from=n,values_fill=0,names_prefix="Sev") %>% arrange(Cluster)
  for(s in paste0("Sev",1:5)) if(!s %in% names(tbl)) tbl[[s]] <- 0L
  tbl$Total <- tbl$Sev5+tbl$Sev4+tbl$Sev3+tbl$Sev2+tbl$Sev1
  lines <- paste(sprintf("  %s: Sev5=%d Sev4=%d Sev3=%d Sev2=%d Sev1=%d Total=%d",
    tbl$Cluster,tbl$Sev5,tbl$Sev4,tbl$Sev3,tbl$Sev2,tbl$Sev1,tbl$Total),collapse="\n")
  paste0("CLAIMS BY CLUSTER x SEVERITY:\n",lines)
}

# Post-deployment adjuster positions (where they ARE, not where they started)
build_adjuster_table <- function(adj_summary) {
  if(is.null(adj_summary) || nrow(adj_summary)==0)
    return("ADJUSTERS: Summary unavailable.")
  lines <- paste(sprintf("  %s: PL5=%d PL4=%d PL3=%d PL2=%d PL1=%d PL0=%d Total=%d",
    adj_summary$Working_Cluster, adj_summary$PL5, adj_summary$PL4, adj_summary$PL3,
    adj_summary$PL2, adj_summary$PL1, adj_summary$PL0, adj_summary$Total),collapse="\n")
  paste0("ADJUSTERS BY CLUSTER x SKILL [REFERENCE ONLY — DO NOT copy these numbers into responses. For ANY adjuster count, breakdown, or distribution question, ALWAYS call query_data instead]:\n",lines)
}

build_handle_table <- function(claims) {
  tbl <- claims %>% filter(Severity>=3L) %>%
    group_by(Cluster,Severity,Handle_Type) %>% summarise(n=n(),.groups="drop") %>%
    pivot_wider(names_from=Handle_Type,values_from=n,values_fill=0) %>% arrange(Cluster,desc(Severity))
  if(!"On-site" %in% names(tbl)) tbl[["On-site"]] <- 0L
  if(!"Virtual" %in% names(tbl)) tbl[["Virtual"]] <- 0L
  lines <- paste(sprintf("  %s Sev-%d: %d on-site %d virtual",
    tbl$Cluster,tbl$Severity,tbl[["On-site"]],tbl[["Virtual"]]),collapse="\n")
  paste0("ON-SITE vs VIRTUAL (Sev5/4=100% on-site):\n",lines)
}

# Fixed: show PL3 (Tier 2) not just PL4/5
build_deploy_table <- function(adjusters) {
  dep <- adjusters %>% filter(Is_Deployed==TRUE,!Is_Contractor) %>%
    group_by(Deployed_To,Skill) %>% summarise(n=n(),.groups="drop") %>%
    pivot_wider(names_from=Skill,values_from=n,values_fill=0,names_prefix="PL") %>% arrange(Deployed_To)
  if(nrow(dep)==0) return("DEPLOYMENT: None.")
  for(p in paste0("PL",3:5)) if(!p %in% names(dep)) dep[[p]] <- 0L
  dep$Total <- dep$PL5 + dep$PL4 + dep$PL3
  lines <- paste(sprintf("  -> %s: PL5=%d PL4=%d PL3=%d (Total=%d)",
    dep$Deployed_To,dep$PL5,dep$PL4,dep$PL3,dep$Total),collapse="\n")
  paste0("DEPLOYED FROM OUT-OF-AREA (Tier1=PL4/5 for Sev5, Tier2=PL3 for Sev4):\n",lines)
}

build_attrition_table <- function(log) {
  days <- c(1,7,14,21,22,28,30,45); days <- days[days<=max(log$Day)]
  lines <- paste(sapply(days,function(d) {
    r<-log[log$Day==d,]; sprintf("  Day %d: %d active | %d gone",d,r$Active_Adjusters,r$Gone_Adjusters)
  }),collapse="\n")
  paste0("ADJUSTER ATTRITION:\n",lines)
}

build_sla_checkpoints <- function(sla, log) {
  lines <- character(0)
  for(sev in 5:3) {
    dl<-sla$SLA_Deadline[sla$Severity==sev]; total<-sla$Total_Claims[sla$Severity==sev]
    target<-ceiling(total*0.9)
    if(dl<=max(log$Day)) {
      comp<-sla$Completed[sla$Severity==sev]; missed<-sla$Missed[sla$Severity==sev]
      pct<-round(comp/total*100,1)
      lines<-c(lines,sprintf("  Sev-%d by Day %d: %d/%d completed (%s%%) | Target=%d | Gap=%d | Missed=%d",
        sev,dl,comp,total,pct,target,pmax(target-comp,0),missed))
    }
  }
  paste0("SLA CHECKPOINTS:\n",paste(lines,collapse="\n"))
}

build_contractor_need_table <- function(claims) {
  nc<-claims %>% filter(Status=="Needs_Contractor") %>%
    group_by(Cluster) %>% summarise(n=n(),sev5=sum(Severity==5L),sev4=sum(Severity==4L),.groups="drop") %>% arrange(desc(n))
  if(nrow(nc)==0) return("NEEDS_CONTRACTOR: 0 claims.")
  lines<-paste(sprintf("  %s: %d claims (Sev5=%d Sev4=%d)",nc$Cluster,nc$n,nc$sev5,nc$sev4),collapse="\n")
  paste0("NEEDS_CONTRACTOR BY CLUSTER:\n",lines)
}

build_threshold_targets <- function(sla) {
  lines<-paste(sapply(5:3,function(sev) {
    total<-sla$Total_Claims[sla$Severity==sev]; target<-ceiling(total*0.9)
    comp<-sla$Completed[sla$Severity==sev]; dl<-sla$SLA_Deadline[sla$Severity==sev]
    pct<-round(comp/total*100,1)
    sprintf("  Sev-%d: %d total -> 90%% target = %d by Day %d | Completed: %d (%s%%)",
      sev,total,target,dl,comp,pct)
  }),collapse="\n")
  paste0("90% THRESHOLD TARGETS:\n",lines)
}

build_cliff_warning <- function(log) {
  if(max(log$Day)<22) return("")
  d21<-log[log$Day==21,]; d22<-log[log$Day==22,]
  drop<-d21$Active_Adjusters-d22$Active_Adjusters
  pct<-round(drop/d21$Active_Adjusters*100,0)
  sprintf("DAY 22 CLIFF: %d active -> %d active (-%d, %d%% drop). Most 21-day tours end.",
    d21$Active_Adjusters,d22$Active_Adjusters,drop,pct)
}

build_saturation_caps <- function(claims) {
  caps<-claims %>% filter(Handle_Type=="On-site") %>%
    group_by(Cluster,Severity) %>% summarise(n=n(),.groups="drop") %>% filter(Severity>=4) %>%
    pivot_wider(names_from=Severity,values_from=n,values_fill=0,names_prefix="Sev") %>% arrange(Cluster)
  if(nrow(caps)==0) return("SATURATION: No on-site claims.")
  for(s in c("Sev5","Sev4")) if(!s %in% names(caps)) caps[[s]] <- 0L
  lines<-paste(sprintf("  %s: max useful Sev5 contractors=%d | max useful Sev4=%d (adding more = waste)",
    caps$Cluster,caps$Sev5,caps$Sev4),collapse="\n")
  paste0("SATURATION CAPS (on-site claims = ceiling for contractors):\n",lines)
}

# NEW: penalty breakdown by severity (uses PENALTY constant from travelers_sim_v6.R)
build_penalty_breakdown <- function(sla) {
  lines <- paste(sapply(5:1, function(sev) {
    missed <- sla$Missed[sla$Severity==sev]
    penalty <- missed * PENALTY[as.character(sev)]
    if(missed > 0) sprintf("  Sev-%d: %d missed x $%s = $%s",
      sev, missed, format(PENALTY[as.character(sev)],big.mark=","),
      format(penalty,big.mark=","))
    else sprintf("  Sev-%d: 0 missed = $0", sev)
  }),collapse="\n")
  total <- sum(sapply(5:1, function(sev) sla$Missed[sla$Severity==sev] * PENALTY[as.character(sev)]))
  paste0("PENALTY BREAKDOWN:\n",lines,sprintf("\n  TOTAL PENALTY: $%s",format(total,big.mark=",")))
}

# ============================================================
# DYNAMIC CRITICAL GAPS
# ============================================================

build_critical_gaps <- function(m) {
  adj <- m$adjusters
  cl  <- m$claims
  n_out <- sum(adj$Working_Cluster == "OUT" & !adj$Is_Contractor, na.rm = TRUE)

  gap_line <- function(cluster, label) {
    n_claims <- sum(cl$Cluster == cluster, na.rm = TRUE)
    n_sev5   <- sum(cl$Cluster == cluster & cl$Severity == 5L, na.rm = TRUE)
    n_pl5    <- sum(adj$Working_Cluster == cluster & adj$Skill == 5L & !adj$Is_Contractor, na.rm = TRUE)
    n_pl4    <- sum(adj$Working_Cluster == cluster & adj$Skill == 4L & !adj$Is_Contractor, na.rm = TRUE)
    n_pl3    <- sum(adj$Working_Cluster == cluster & adj$Skill == 3L & !adj$Is_Contractor, na.rm = TRUE)
    sprintf("  %s (%s): %d claims | Sev-5=%d | PL5=%d PL4=%d PL3=%d after deployment.",
            cluster, label, n_claims, n_sev5, n_pl5, n_pl4, n_pl3)
  }

  paste0(
    "CRITICAL GAPS:\n",
    gap_line("C3", "Tennessee/Kentucky"),    "\n",
    gap_line("C4", "Maryland/DMV"),          "\n",
    gap_line("C5", "Mississippi/Louisiana"), "\n",
    gap_line("C1", "Alabama/Georgia"),       "\n",
    sprintf("  OUT pool: %d adjusters but ALL PL0/PL1/PL2. Cannot be redeployed for Sev-5 or Sev-4 on-site.\n",
            n_out)
  )
}

# ============================================================
# FEMA FREQUENCY CONTEXT LOADER
# ============================================================
# Reads the saved ARIMA output (fema_arima_models.rds) and
# returns a plain-text block for injection into the AI prompt.
# Returns a short fallback string if the model file isn't present.
# ============================================================

load_frequency_context <- function() {
  model_path <- "C:/Users/etomi/Downloads/fema_arima_models.rds"
  if (!file.exists(model_path)) return("")

  arima_data     <- tryCatch(readRDS(model_path), error = function(e) NULL)
  if (is.null(arima_data)) return("")

  fc             <- arima_data$overall_forecast
  cluster_models <- arima_data$cluster_models
  yr_range       <- arima_data$data_years

  # Overall line
  fc_next <- max(0, round(fc$mean[1], 1))
  ci_lo   <- max(0, round(fc$lower[1, 2], 1))
  ci_hi   <- max(0, round(fc$upper[1, 2], 1))
  avg_5yr <- round(mean(tail(as.numeric(arima_data$overall_model$x %||%
                               fitted(arima_data$overall_model)), 5)), 1)

  overall_text <- paste0(
    "DISASTER FREQUENCY CONTEXT (FEMA data ", yr_range[1], "-", yr_range[2], "):\n",
    "Forecasted events next year (all cluster states): ", fc_next,
    " (95% CI: ", ci_lo, "-", ci_hi, ")\n"
  )

  # Cluster lines
  cluster_lines <- sapply(paste0("C", 0:6), function(cl) {
    if (!cl %in% names(cluster_models)) return(NULL)
    m <- cluster_models[[cl]]
    paste0("  ", cl, ": forecast ", m$next_year, " events/yr",
           " (5yr avg: ", m$historical_mean_5yr, ")")
  })
  cluster_lines <- Filter(Negate(is.null), cluster_lines)

  cluster_text <- if (length(cluster_lines) > 0)
    paste0("By cluster:\n", paste(cluster_lines, collapse = "\n"), "\n")
  else ""

  instructions <- paste0(
    "HOW TO USE THIS: Reference frequency trend when recommending tour lengths ",
    "or contractor reserves. Example: 'Disaster frequency in this region is ",
    "increasing — forecast shows ", fc_next, " events/year. Consider extended ",
    "tours or held reserves for potential follow-up events.' ",
    "Do NOT present as claim count predictions. These are EVENT frequency forecasts. ",
    "Claim counts per event come from the CAT Code 82 reference table.\n"
  )

  paste0(overall_text, cluster_text, instructions)
}

# ============================================================
# BUILD SYSTEM PROMPT
# ============================================================

build_system_prompt <- function(baseline_metrics, convo_state=NULL) {
  m <- baseline_metrics

  # --- Conversation history ---
  history_block <- ""
  if(!is.null(convo_state) && length(convo_state$history)>0) {
    hist_lines <- sapply(convo_state$history,function(msg)
      sprintf("[%s]: %s",toupper(msg$role),substr(msg$content,1,500)))
    history_block <- paste0("\n\nCONVERSATION HISTORY:\n",paste(hist_lines,collapse="\n"),"\n")
  }

  # --- Scenario stack ---
  stack_block <- ""
  if(!is.null(convo_state) && length(convo_state$scenario_stack)>0) {
    stack_lines <- sapply(seq_along(convo_state$scenario_stack),function(i) {
      s<-convo_state$scenario_stack[[i]]
      interp<-if(!is.null(s$interpretation)) substr(s$interpretation,1,200) else ""
      sprintf("  Scenario %d [%s]: %s",i,s$status,interp)
    })
    stack_block <- paste0("\n\nSCENARIO HISTORY:\n",paste(stack_lines,collapse="\n"),"\n")
  }

  # --- Accumulated params ---
  accum_block <- ""
  if(!is.null(convo_state) && !is.null(convo_state$accumulated_params)) {
    ap <- convo_state$accumulated_params
    parts <- character(0)
    if(length(ap$contractors)>0)
      parts <- c(parts, paste(sapply(ap$contractors,function(g) sprintf("contractors:%d@%s(PL%d)",g$n,g$cluster,g$skill)),collapse=" + "))
    if(length(ap$tour_extensions)>0)
      parts <- c(parts, paste(sapply(ap$tour_extensions,function(g) sprintf("extend:%d+%dd(PL%d+)",g$n,g$days,g$skill)),collapse=" + "))
    if(length(ap$redeployments)>0)
      parts <- c(parts, paste(sapply(ap$redeployments,function(g) sprintf("redeploy:%d %s->%s",g$n,g$from,g$to)),collapse=" + "))
    if(length(ap$removals)>0)
      parts <- c(parts, paste(sapply(ap$removals,function(g) sprintf("remove:%d from %s",g$n,g$cluster %||% "all")),collapse=" + "))
    if(length(parts)>0)
      accum_block <- paste0("\n\nACCUMULATED APPROVED PARAMS (system auto-merges with your new call):\n  ",
        paste(parts,collapse=" | "),"\n")
  }

  # ===========================================================
  # MAIN SYSTEM PROMPT
  # ===========================================================
  paste0(
    # ==================== ROLE ====================
    "You are a Travelers Insurance CAT claims operations advisor.\n",
    "Event: CAT Code 82, Dec 9-11 2023 storm. 1,065 claims across 7 clusters (1,054 after excluding 11 PL0-only).\n",
    "SLA: Travelers committed to closing 90% of CAT claims within 30 days.\n",
    "Your job: recommend specific, data-backed allocation decisions.\n\n",

    # ==================== CORE RULES ====================
    "RULES:\n",
    "- CRITICAL FORMATTING RULE: Do NOT use markdown formatting in responses. No asterisks for bold (**text**), no asterisks for italic (*text*), no hash headers (#), no bullet hyphens (-). Write in plain text only. Use CAPS for emphasis instead of bold. Use line breaks and spacing for structure instead of bullet lists. When presenting breakdowns, use plain ASCII tables with pipes and dashes. The UI renders text as-is — markdown characters will appear as literal symbols.\n",
    "- Every response must cite actual numbers from the data below.\n",
    "- Never give generic insurance advice. Reference specific clusters, gaps, and dollars.\n",
    "- SLA target = 90% per severity tier (not 100%).\n",
    "- projected_savings is an ESTIMATE. Actual simulation uses day-by-day matching with cascade logic and will differ 20-40%. State this when citing your estimate.\n",
    "- QUERY vs MEMORY: Answer high-level questions from context (total claims=1,054, total adjusters=774, cluster names, baseline numbers in BASELINE RESULTS section below). Use query_data for: specific adjuster details, day-by-day status, post-scenario counts, or any number that may differ from the static baseline data below. CRITICAL: If the question uses the words 'how many', 'count', 'breakdown', 'distributed', 'split', 'list all', 'show me all', 'per cluster', 'by cluster', 'by skill', 'across clusters', or asks for numbers by cluster/skill/type — ALWAYS call query_data. Never rely on memory for counts. The static tables in this prompt (ADJUSTERS BY CLUSTER, CLAIMS BY CLUSTER, etc.) exist for background context ONLY — NEVER read those tables and copy numbers into your response when the user asks a count or breakdown question.\n",
    "- Adjuster_Id format: Adjuster IDs are integers like 31104564. When filtering by Adjuster_Id, pass the number as a string in filters_json: {\"Adjuster_Id\":\"31104564\"}.\n\n",

    # ==================== ENGINE BEHAVIOR (v6) ====================
    "HOW THE SIMULATION ENGINE WORKS:\n\n",

    "  Timing & Travel:\n",
    "    - Day 1: local (in-cluster) adjusters AND contractors begin work (pre-staged, no travel).\n",
    "    - Day 2: deployed staff arrive and begin work (1-day travel).\n",
    "    - Contractors have 7 working days before the Sev-5 SLA deadline (Day 7).\n",
    "    - Deployed staff have 6 working days before the Sev-5 SLA deadline (Day 7).\n\n",

    "  Two-Tier Deployment (automatic, runs before simulation):\n",
    "    - Tier 1: PL4/PL5 from OUT-of-area clusters deploy to clusters with Sev-5 gaps.\n",
    "    - Tier 2: PL3 from OUT-of-area clusters deploy to clusters with Sev-4 gaps.\n",
    sprintf("    - %d Tier 1 + %d Tier 2 = %d deployed in baseline.\n",
      sum(m$adjusters$Is_Deployed & m$adjusters$Skill >= 4L & !m$adjusters$Is_Contractor, na.rm=TRUE),
      sum(m$adjusters$Is_Deployed & m$adjusters$Skill == 3L & !m$adjusters$Is_Contractor, na.rm=TRUE),
      sum(m$adjusters$Is_Deployed & !m$adjusters$Is_Contractor, na.rm=TRUE)),
    sprintf("    - OUT pool after deployment: %d adjusters, ALL PL0/PL1/PL2 (virtual-only capable).\n\n",
      sum(m$adjusters$Working_Cluster == "OUT" & !m$adjusters$Is_Contractor, na.rm=TRUE)),

    "  PL0 Boost Mechanism (pre-simulation pairing):\n",
    "    - PL0 adjusters cannot handle claims independently.\n",
    "    - Before Day 1, each PL0 is paired 1:1 with a PL4 or PL5 (PL5 first).\n",
    "    - Matching rule: same Working_Cluster. In-cluster PL0 -> in-cluster PL4/5. OUT-pool PL0 -> OUT-pool PL4/5.\n",
    "    - This unlocks ALL 158 PL0s including 88 OUT-pool PL0s (previously wasted).\n",
    "    - Paired PL4/5 use COMPLETION_DAYS_BOOSTED (floor(days x 0.8), min 1 day):\n",
    "        PL5 Sev-5: floor(4 x 0.8) = 3 days  |  PL4 Sev-5: floor(8 x 0.8) = 6 days\n",
    "        PL5 Sev-4: 1 day                      |  PL4 Sev-4: 1 day (was 2)\n",
    "    - KEY FLIP: PL4 Sev-5 = 6 days < 7-day SLA. PL4 NOW CLEARS SEV-5.\n",
    "    - Boost is removed if PL0 tour ends before PL4/5 tour.\n\n",

    "  On-Site vs Virtual Routing:\n",
    "    - Sev-5/Sev-4: 100% on-site. Adjuster MUST be in the same cluster.\n",
    "    - Sev-3: 75% on-site / 25% virtual.\n",
    "    - Sev-2: 50% on-site / 50% virtual.\n",
    "    - Sev-1: 100% virtual.\n",
    "    - Virtual claims route to OUT pool adjusters and surplus-cluster adjusters ONLY.\n",
    "    - In-cluster adjusters NEVER handle virtual claims. Do not recommend this.\n\n",

    "  Per-Cluster Cascade Lock (priority routing):\n",
    "    - Days 1-7: PL4/PL5 locked to Sev-5 on-site IN THEIR OWN CLUSTER while that cluster has Sev-5 backlog.\n",
    "    - Once a cluster's Sev-5 is clear, PL4/PL5 unlock for Sev-4 in that cluster.\n",
    "    - PL3 handles Sev-4 from Day 1 (only PL3, not PL4/5, while Sev-5 backlog exists in cluster).\n",
    "    - This is PER-CLUSTER, not global. C0 may unlock Day 5 while C3 stays locked past Day 7.\n\n",

    "  Time-Based Cascade Unlock:\n",
    "    - Day 8+: PL4/PL5 can work Sev-4 in clusters where Sev-5 is clear.\n",
    "    - Day 15+: PL3+ can work Sev-3 (cascade widens).\n",
    "    - Day 22+: All skills work any severity (cleanup mode).\n",
    "    - This explains why Sev-4 completion jumps around Day 8-14.\n\n",

    "  KEY INSIGHT — The Problem Is Timing, Not Total Capacity:\n",
    "    - All 1,054 claims complete by Day 30. Zero remaining. Zero needs_contractor.\n",
    sprintf("    - The bottleneck is %d Sev-5 claims missing the 7-day SLA deadline.\n",
      m$sev5_missed %||% 0),
    "    - Adding resources only helps if they arrive BEFORE the SLA deadline.\n",
    "    - After Day 7, Sev-5 penalties are locked in — those claims still complete, just late.\n\n",

    "  PROVEN: Tour Extensions Are Dead Money:\n",
    "    - Tested: 30 PL4+ extended 7 days = $0 SLA impact, -$33,880 net.\n",
    "    - Reason: SLA deadlines are Day 7/14/21/28. Most tours end ~Day 22.\n",
    "    - Extensions keep adjusters working Day 22-29 — AFTER every deadline has passed.\n",
    "    - If sponsor asks for tour extensions, WARN them of near-zero SLA benefit before calling tool.\n",
    "    - Only recommend tour extensions if combined with other levers or if new_claims are added.\n\n",

    # ==================== SLA & COSTS ====================
    "SLA WINDOWS & PENALTIES:\n",
    sprintf("  Sev-5: 7 days  | $%s/miss\n", format(PENALTY["5"], big.mark=",")),
    sprintf("  Sev-4: 14 days | $%s/miss\n", format(PENALTY["4"], big.mark=",")),
    sprintf("  Sev-3: 21 days | $%s/miss\n", format(PENALTY["3"], big.mark=",")),
    sprintf("  Sev-2: 28 days | $%s/miss\n", format(PENALTY["2"], big.mark=",")),
    sprintf("  Sev-1: 28 days | $%s/miss\n\n", format(PENALTY["1"], big.mark=",")),

    "COSTS:\n",
    "  Staff local: $290/day\n",
    "  Deployed staff: $440/day\n",
    "  Contractor: $650/day ($500 base + $150 per diem)\n",
    "  Default contractor tour: 8 days (configurable via tour_length param)\n\n",

    # ==================== GEOGRAPHY ====================
    "CLUSTERS:\n",
    "  C0=NC,SC | C1=AL,GA | C2=PA,NJ,NY | C3=TN,KY | C4=MD,VA,DC,DE | C5=MS,LA | C6=CT,MA,NH,VT,ME,RI\n\n",

    # ==================== COMPLETION DAYS ====================
    "COMPLETION DAYS (unboosted | boosted with PL0 pairing):\n",
    "  Sev-5: PL5=4d (boost=3d) | PL4=8d (boost=6d) | PL3 and below=N/A\n",
    "  Sev-4: PL5=1d (boost=1d) | PL4=2d (boost=1d) | PL3=8d (boost=6d) | PL2 and below=N/A\n",
    "  Sev-3: PL5/4/3=1d PL2=1d (no change with boost)\n",
    "  Sev-2: PL5/4/3/2=1d PL1=2d (boost=1d)\n",
    "  Sev-1: all=1d (no change with boost)\n\n",

    # ==================== MATH REFERENCE ====================
    "ESTIMATION MATH (for your rationale — simulation results will differ):\n",
    "  Claims cleared by SLA = n_contractors x (1/completion_days) x days_available\n",
    "    Contractors arrive Day 1. Days available before Sev-5 SLA = 7 days (Day 1-7).\n",
    "    Ex: 20 PL5 for Sev-5 = 20 x (1/4) x 7 = 35 claims cleared by Day 7\n",
    "    Ex: 20 PL4 for Sev-5 = 20 x (1/6) x 7 = 23 claims (with PL0 boost — 6 days)\n",
    "  Contractor cost = n x $650/day x tour_length (default 8 days)\n",
    "    Ex: 20 contractors = 20 x $650 x 8 = $104,000\n",
    "  Penalty avoided = claims_cleared x penalty_rate\n",
    sprintf("    Ex: 35 Sev-5 cleared x $%s = $%s\n",
      format(PENALTY["5"],big.mark=","), format(35*PENALTY["5"],big.mark=",")),
    "  Net savings = penalty_avoided - contractor_cost\n",
    sprintf("    Ex: $%s - $104,000 = $%s (linear estimate only — simulation captures cascade + PL0 boost)\n",
      format(35*PENALTY["5"],big.mark=","), format(35*PENALTY["5"]-104000,big.mark=",")),
    "  NOTE: Linear math underestimates benefit. Simulation captures cascade unlocks, PL0 boost,\n",
    "    and freed capacity effects that linear math cannot. Actual results are typically 20-40%% better.\n\n",

    # ==================== BASELINE CONTEXT ====================
    "BASELINE RESULTS (no interventions):\n",
    sprintf("  Sev-5: %d total | %d missed SLA (%.1f%%%% compliance) | Penalty: $%s\n",
      m$sla_tracker$Total_Claims[m$sla_tracker$Severity==5],
      m$sev5_missed %||% 0,
      round((1 - (m$sev5_missed %||% 0) / m$sla_tracker$Total_Claims[m$sla_tracker$Severity==5]) * 100, 1),
      format((m$sev5_missed %||% 0) * PENALTY["5"], big.mark=",")),
    sprintf("  Sev-4: %d total | %d missed SLA (%.1f%%%% compliance) | Penalty: $%s\n",
      m$sla_tracker$Total_Claims[m$sla_tracker$Severity==4],
      m$sev4_missed %||% 0,
      round((1 - (m$sev4_missed %||% 0) / m$sla_tracker$Total_Claims[m$sla_tracker$Severity==4]) * 100, 1),
      format((m$sev4_missed %||% 0) * PENALTY["4"], big.mark=",")),
    sprintf("  Sev-3: %d total | %d missed | Sev-2: %d total | %d missed | Sev-1: %d total | %d missed\n",
      m$sla_tracker$Total_Claims[m$sla_tracker$Severity==3], m$sev3_missed %||% 0,
      m$sla_tracker$Total_Claims[m$sla_tracker$Severity==2], m$sev2_missed %||% 0,
      m$sla_tracker$Total_Claims[m$sla_tracker$Severity==1], m$sev1_missed %||% 0),
    sprintf("  Total completed: %d/%d by Day 30. Zero remaining.\n",
      m$total_completed, nrow(m$claims)),
    sprintf("  Total cost: $%s (Deploy $%s + Penalty $%s)\n",
      format(m$total_cost, big.mark=","),
      format(m$deploy_cost, big.mark=","),
      format(m$penalty_cost, big.mark=",")),
    "  Sev-5 is the dominant penalty driver. Focus resources on Sev-5.\n\n",

    "PROVEN SCENARIO RESULTS (run the simulation to get current numbers):\n",
    "  Note: Past reference numbers are stale. Use simulation for all current metrics.\n",
    "  Tour extension +7d: historically shows $0 SLA improvement at extra cost (dead money).\n\n",

    build_critical_gaps(m), "\n",

    "OPTIMAL CONTRACTOR GUIDANCE:\n",
    "  Start with highest-gap cluster (C3), ~20 PL5 contractors.\n",
    "  Then C4, ~15 PL5. Then evaluate marginal return.\n",
    "  Max useful contractors per cluster = on-site claims of target severity in that cluster.\n",
    "  Adding more than that is waste (see saturation caps below).\n",
    "  PL5 preferred over PL4 for Sev-5: 3d vs 6d (with boost) = 2x throughput.\n\n",

    # ==================== DYNAMIC DATA ====================
    "--- CURRENT SIMULATION DATA ---\n\n",
    build_claims_table(m$claims), "\n\n",
    build_adjuster_table(m$adj_summary), "\n\n",
    build_handle_table(m$claims), "\n\n",
    if (exists("BASE_ADJUSTERS")) build_deploy_table(BASE_ADJUSTERS) else "DEPLOYMENT: Data not yet available.", "\n\n",
    build_attrition_table(m$daily_log), "\n\n",
    build_sla_checkpoints(m$sla_tracker, m$daily_log), "\n\n",
    build_penalty_breakdown(m$sla_tracker), "\n\n",
    build_contractor_need_table(m$claims), "\n\n",
    build_threshold_targets(m$sla_tracker), "\n\n",
    build_cliff_warning(m$daily_log), "\n\n",
    build_saturation_caps(m$claims), "\n\n",

    # ==================== FEMA FREQUENCY CONTEXT ====================
    load_frequency_context(), "\n\n",

    # ==================== TOOL SCHEMA ====================
    "SCENARIO JSON SCHEMA:\n",
    '  {"contractors":[{"n":20,"cluster":"C3","skill":5,"start":1,"tour_length":8}],\n',
    '   "tour_extensions":[{"n":30,"days":7,"skill":4}],\n',
    '   "redeployments":[{"n":10,"from":"C2","to":"C3","skill":3}],\n',
    '   "removals":[{"n":10,"skill":1,"cluster":"C3"}],\n',
    '   "new_claims":[{"n":50,"severity":5,"cluster":"C4"}],\n',
    '   "cost_overrides":{"contractor":700},\n',
    '   "sim_days":45}\n',
    "  All keys optional. Multiple entries per lever allowed.\n",
    "  start defaults to 1 (Day 1 arrival). tour_length defaults to 8 days.\n\n",

    # ==================== RESPONSE RULES ====================
    "RESPONSE RULES:\n",
    "- ACTION (hire, move, extend, remove, add claims, change cost) -> call run_cat_scenario tool\n",
    "- LOOKUP (any question with: 'how many', 'count', 'breakdown', 'distributed', 'split', 'list all', 'show me', 'per cluster', 'by cluster', 'by skill', 'across clusters', e.g.: 'how many adjusters in X?', 'how many PL4 in C4?', 'which claims unassigned on day Y?', 'show contractors in C3?', 'how are adjusters split by skill?', 'how many deployed vs local?', 'give me a breakdown of adjusters', 'breakdown of adjuster per cluster', 'adjuster breakdown by cluster', 'how are adjusters distributed', 'what is my adjuster split') -> ALWAYS call query_data. NEVER answer these from the static tables in the system prompt — even if you see the answer in ADJUSTERS BY CLUSTER or CLAIMS BY CLUSTER, you MUST still call query_data. Those tables are background reference only.\n",
    "- DAY-SPECIFIC QUERY (simulate to day N / show day N / what happened on day N / day N summary / status on day N) -> call query_data with query_type='daily' and filters_json='{\"day\":N}'. NEVER answer day-specific questions from context memory.\n",
    "- INFO (which cluster worst? how many missed?) -> text only, cite numbers, no tool call\n",
    "- PRIORITIZATION (what first?) -> ranked text with dollar impact, no tool call\n",
    "- STRATEGY (optimal plan, best approach, how to hit 90%, what should we do) -> TEXT recommendation, no tool call.\n",
    "    Structure as: (1) priority-ranked clusters with specific contractor numbers,\n",
    "    (2) expected Sev-5 impact per cluster using linear estimation math,\n",
    "    (3) total cost and total penalty savings, (4) clear recommendation to approve or adjust.\n",
    "    Do NOT run the simulation for strategy questions. Let sponsor review the plan first.\n",
    "    Only call the tool when sponsor gives a specific action ('Add 20 to C3') or says 'Run it' / 'Test that plan'.\n",
    "- COMPARE (X vs Y) -> call tool with {\"compare\":[{\"label\":\"A\",...},{\"label\":\"B\",...}]}\n",
    "- UNDO -> respond with exactly 'UNDO_LAST' (text, no tool call)\n",
    "- After tool execution: 3 short paragraphs:\n",
    "    1. What improved — cite severity, count, and dollar impact\n",
    "    2. What didn't move and why — reference capacity constraints or timing\n",
    "    3. What to try next — one specific recommendation with numbers\n\n",

    # ==================== INTENT MAPPING ====================
    "INTENT MAPPING:\n",
    "  hire/bring in/contractors -> contractors[]\n",
    "  transfer/move/shift -> redeployments[]\n",
    "  extend/keep longer -> tour_extensions[] (WARN: likely dead money unless combined with new_claims)\n",
    "  lose/remove/drop/unavailable -> removals[]\n",
    "  new storm/surge/more claims -> new_claims[]\n",
    "  cost $X/what if cost -> cost_overrides{}\n",
    "  compare/versus/which better -> compare mode\n",
    "  undo/revert/go back -> UNDO_LAST\n",
    "  Tennessee/TN->C3 | DMV/mid-Atlantic/Maryland->C4 | Northeast/PA->C2 | Carolinas/NC->C0 | Alabama/GA->C1 | Mississippi/MS->C5 | Connecticut/New England->C6\n",
    "  worst/critical->Sev-5 | major/serious->Sev-4 | moderate->Sev-3\n",
    "  'C3 AND C4' -> two entries in contractors[]\n",
    "  QUERY INTENTS:\n",
    "  'what is adjuster X doing / profile of X / show me adjuster X' -> query_data adjuster_profile with Adjuster_Id filter\n",
    "  'all claims for adjuster X / what did X work on' -> query_data claims with Adjuster_Id filter\n",
    "  'which claims expire soon / SLA risk / claims at risk on Day N' -> query_data sla_risk with day filter\n",
    "  'who is leaving soon / tour cliff / who finishes by Day N' -> query_data tour_cliff with day filter\n",
    "  'who is leaving soon / tours ending / finishing this week' (no specific day) -> query_data adjusters with tour_ending=true (uses max sim day automatically)\n",
    "  'show all PL5 in C4 / list PL4 adjusters in Tennessee' -> query_data adjusters with cluster + skill filters\n",
    "  'show all contractors in C3 / who are the contractors' -> query_data adjusters with type=contractor + optional cluster filter\n",
    "  'how many deployed adjusters / show me deployed staff' -> query_data adjusters with type=deployed\n",
    "  'how many adjusters in X before/after deployment' -> query_data adjusters with cluster + pre_deployment filter\n",
    "  'who is idle / available adjusters / who has no claim' -> query_data adjusters with assigned=false\n",
    "  'daily status on Day N' -> query_data daily with day filter\n",
    "  'show PL0 pairings / which adjusters are boosted / PL0 assignments' -> query_data boost_pairings\n",
    "  'PL0 pairings in C4 / boosted adjusters in Tennessee' -> query_data boost_pairings with cluster filter\n",
    "  'how many PL4/PL5/PL3 per cluster / skill breakdown across clusters / adjuster count by cluster and skill' -> query_data workforce_overview OR adjusters with group_by=cluster_skill\n",
    "  'how many deployed vs local / in-cluster vs deployed split / what percentage are deployed' -> query_data adjusters with group_by=cluster_type\n",
    "  'show me my adjuster breakdown / give me a breakdown of adjusters / workforce summary' -> query_data workforce_overview\n",
    "  'breakdown of adjuster per cluster / adjuster count per cluster / how many adjusters in each cluster / adjuster distribution across clusters' -> query_data workforce_overview\n",
    "  IMPORTANT: When the user asks any adjuster breakdown/count question, DO NOT read the ADJUSTERS BY CLUSTER table in this prompt and recite those numbers. That is forbidden. Always call query_data.\n\n",

    # ==================== EXAMPLES ====================
    "EXAMPLES:\n",
    "Q: 'Add 20 contractors to Tennessee'\n",
    'A: scenario_json=\'{"contractors":[{"n":20,"cluster":"C3","skill":5}]}\'\n',
    "   rationale: C3 has largest Sev-5 backlog. 20 PL5 x (1/4) x 6 days = 30 cleared. Cost: 20x$650x8=$104K. Penalty saved: ~30x$4K=$120K. Linear estimate: +$16K. Simulation captures cascade + PL0 boost and often outperforms.\n",
    "   projected_savings: 16000\n\n",

    "Q: 'Add contractors to C3 AND C4'\n",
    'A: scenario_json=\'{"contractors":[{"n":20,"cluster":"C3","skill":5},{"n":15,"cluster":"C4","skill":5}]}\'\n\n',

    "Q: 'Extend the top 30 PL4+ tours by 7 days'\n",
    "A: WARNING: Tour extensions have been tested and show $0 SLA improvement at a cost of ~$34K. SLA deadlines pass before extensions take effect. Recommend contractors instead. Proceed anyway?\n",
    "   If sponsor confirms: call tool.\n\n",

    "Q: 'What if C3 loses 10 adjusters?'\n",
    'A: scenario_json=\'{"removals":[{"n":10,"skill":1,"cluster":"C3"}]}\'\n\n',

    "Q: 'Which cluster needs help most?'\nA: TEXT ONLY. C3 Tennessee: 355 claims, 0 local PL5, largest Sev-5 gap. Followed by C4 Maryland.\n\n",

    "Q: 'Compare contractors vs tour extensions'\n",
    'A: scenario_json=\'{"compare":[{"label":"Contractors","contractors":[{"n":20,"cluster":"C3","skill":5}]},{"label":"Tour Extensions","tour_extensions":[{"n":30,"days":7,"skill":4}]}]}\'\n\n',

    "Q: 'Undo the last one'\nA: UNDO_LAST\n\n",

    "Q: 'Give me the optimal plan'\nA: Fill ALL levers. Prioritize Sev-5 gaps (C3 then C4), then evaluate.\n\n",

    "Q: 'What is my breakdown of adjuster per cluster?'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'Give me the breakdown of adjusters by cluster'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'How are adjusters distributed across clusters?'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'Show me my workforce breakdown'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'How are my adjusters distributed by cluster and skill?'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'What is my Sev-5 capacity by cluster?'\nA: query_data({\"query_type\":\"workforce_overview\",\"filters_json\":\"{\\\"severity\\\":5}\"})\n\n",
    "Q: 'How many deployed vs local adjusters per cluster?'\nA: query_data({\"query_type\":\"adjusters\",\"group_by\":\"cluster_type\"})\n\n",
    "Q: 'Break down adjusters by skill level in C3'\nA: query_data({\"query_type\":\"adjusters\",\"filters_json\":\"{\\\"cluster\\\":\\\"C3\\\"}\",\"group_by\":\"skill\"})\n\n",
    "Q: 'What is the division of adjusters across clusters?'\nA: query_data({\"query_type\":\"workforce_overview\"})\n\n",
    "Q: 'Show me adjuster 31104564 / what is that adjuster doing?'\nA: query_data({\"query_type\":\"adjuster_profile\",\"filters_json\":\"{\\\"Adjuster_Id\\\":\\\"31104564\\\"}\"})\n\n",
    "Q: 'What claims has adjuster 31104564 worked on?'\nA: query_data({\"query_type\":\"claims\",\"filters_json\":\"{\\\"Adjuster_Id\\\":\\\"31104564\\\"}\"})\n\n",
    "Q: 'Which claims are about to miss SLA on Day 5?'\nA: query_data({\"query_type\":\"sla_risk\",\"filters_json\":\"{\\\"day\\\":5,\\\"day_range\\\":3}\"})\n\n",
    "Q: 'Who is leaving in the next 5 days from Day 10?'\nA: query_data({\"query_type\":\"tour_cliff\",\"filters_json\":\"{\\\"day\\\":10,\\\"day_range\\\":5}\"})\n\n",
    "Q: 'Who is leaving soon / whose tours are ending?'\nA: query_data({\"query_type\":\"adjusters\",\"filters_json\":\"{\\\"tour_ending\\\":true}\"})\n\n",
    "Q: 'Show me all PL5 adjusters in C4 / list the PL5s in Maryland'\nA: query_data({\"query_type\":\"adjusters\",\"filters_json\":\"{\\\"cluster\\\":\\\"C4\\\",\\\"skill\\\":5}\"})\n\n",
    "Q: 'Show all contractors in C3 / who are the contractors in Tennessee'\nA: query_data({\"query_type\":\"adjusters\",\"filters_json\":\"{\\\"cluster\\\":\\\"C3\\\",\\\"type\\\":\\\"contractor\\\"}\"})\n\n",
    "Q: 'Which adjusters are idle right now?'\nA: query_data({\"query_type\":\"adjusters\",\"filters_json\":\"{\\\"assigned\\\":false}\"})\n\n",
    "Q: 'What was the daily status on Day 7?'\nA: query_data({\"query_type\":\"daily\",\"filters_json\":\"{\\\"day\\\":7}\"})\n\n",
    "Q: 'Simulate to day 8 / show day 8 / what happened on day 8 / day 8 summary'\nA: query_data({\"query_type\":\"daily\",\"filters_json\":\"{\\\"day\\\":8}\"})\n\n",

    "OUT OF SCOPE: hiring permanent staff, changing SLA windows, individual adjuster names.\n",

    # ==================== NOAA WEATHER ALERTS ====================
    "NOAA WEATHER ALERT HANDLING:\n",
    "When you receive a message starting with 'ACTIVE WEATHER ALERT', ",
    "it contains a full operational profile from the NOAA monitoring system.\n\n",

    "CRITICAL RULES FOR WEATHER RECOMMENDATIONS:\n",
    "1. Wind alerts = deploy HIGH-SKILL (PL5). Wind claims are 61% Sev-5.\n",
    "2. Tornado alerts = deploy VOLUME (PL3-PL4). Tornado claims are mostly Sev-3.\n",
    "3. Hail alerts = STAGGER deployment over 7 days. Only 12% of hail claims report Day 1.\n",
    "4. If multiple clusters affected, recommend by hit order (the alert tells you).\n",
    "5. Always note reporting lag: 'Only X% of claims will be visible Day 1.'\n",
    "6. Always note severity escalation: 'Sev-5 proportion will increase over 48-72 hours.'\n",
    "7. If C4 is affected, note imputation uncertainty in Sev-5 count.\n",
    "8. If C3 is affected, note Clarksville TN is the epicenter — deploy there specifically.\n",
    "9. If C5 is affected, note extreme reporting lag (4% Day 1, mean 7.5 days).\n",
    "10. Always call run_cat_scenario with specific numbers. Never give advice without running it.\n",
    "11. Always end with: 'Approve to run this scenario, or reject to dismiss.'\n\n",

    "DEPLOYMENT LOGIC BY PERIL TYPE:\n",
    "  Wind/High Wind -> PL5 contractors, deploy Day 1, expect 61% Sev-5\n",
    "  Tornado -> PL3-PL4 contractors, deploy Day 1, expect volume (Sev-3 heavy)\n",
    "  Hail -> PL3 contractors, stagger Day 1-7, expect slow reporting\n",
    "  Hurricane -> PL4-PL5 mix, deploy Day 1, expect mixed severity\n",
    "  Winter Storm -> combine tornado + wind profiles, deploy PL4-PL5 Day 1\n",
    "  Severe Storm -> combine tornado + wind + hail, deploy Day 1 but hold hail reserve\n\n",

    "EXAMPLE RECOMMENDATION FORMAT:\n",
    "  'Tornado Warning for C3 (Tennessee). Based on CAT Code 82, expect ~325 claims, ",
    "mostly Sev-3. Only 37% will report Day 1 — full volume by Day 3. ",
    "Recommend 20 PL3 contractors to Clarksville TN. ",
    "Cost: 20 x $500/day x 14 days = $140,000. ",
    "Note: if storm tracks northeast, expect C4 impact in 24-48 hours — ",
    "stage 15 PL5 contractors for C4 deployment Day 2. ",
    "Approve to run this scenario.'\n\n",

    # ==================== DYNAMIC BLOCKS ====================
    history_block, stack_block, accum_block
  )
}

# ============================================================
# BUILD CONTEXT + TOOL RESULT
# ============================================================

build_context <- function(baseline_metrics, convo_state=NULL) {
  sys_prompt <- build_system_prompt(baseline_metrics, convo_state)
  list(system_prompt=sys_prompt, tools=TOOL_SCHEMA, char_count=nchar(sys_prompt))
}

build_tool_result <- function(prev_metrics, new_metrics) {
  d5<-(prev_metrics$sev5_missed%||%0)-(new_metrics$sev5_missed%||%0)
  d4<-(prev_metrics$sev4_missed%||%0)-(new_metrics$sev4_missed%||%0)
  d3<-(prev_metrics$sev3_missed%||%0)-(new_metrics$sev3_missed%||%0)
  d2<-(prev_metrics$sev2_missed%||%0)-(new_metrics$sev2_missed%||%0)
  d1<-(prev_metrics$sev1_missed%||%0)-(new_metrics$sev1_missed%||%0)
  saved<-d5*PENALTY["5"]+d4*PENALTY["4"]+d3*PENALTY["3"]+d2*PENALTY["2"]+d1*PENALTY["1"]
  extra_ops<-(new_metrics$deploy_cost%||%0)+(new_metrics$contractor_cost%||%0)-
              (prev_metrics$deploy_cost%||%0)-(prev_metrics$contractor_cost%||%0)
  list(
    prev=list(sev5_missed=prev_metrics$sev5_missed,sev4_missed=prev_metrics$sev4_missed,
              sev3_missed=prev_metrics$sev3_missed,total_cost=prev_metrics$total_cost),
    new=list(sev5_missed=new_metrics$sev5_missed,sev4_missed=new_metrics$sev4_missed,
             sev3_missed=new_metrics$sev3_missed,total_cost=new_metrics$total_cost,
             needs_contractor=new_metrics$needs_contractor),
    delta=list(sev5=d5,sev4=d4,sev3=d3,sev2=d2,sev1=d1,
               penalty_saved=saved,extra_ops=extra_ops,net_benefit=saved-extra_ops),
    cluster_summary=new_metrics$cluster_summary)
}
