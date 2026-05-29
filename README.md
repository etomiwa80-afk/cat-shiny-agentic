# CAT Claims Optimization — Agentic AI Simulation

Agentic AI simulation system for catastrophic insurance claims management.
Built as a DS 7900 capstone project at Kennesaw State University.

## What it does

Integrates Claude API tool use, real-time NOAA weather monitoring, and a
45-day rolling horizon optimization engine to recommend and execute adjuster
deployment decisions during CAT events. An LLM agent evaluates simulation
state, proposes interventions, and waits for sponsor approval before
committing changes — a human-in-the-loop design built for operational trust.

## Demo

[![CAT Claims Optimization — Agentic AI Simulation Demo](https://img.youtube.com/vi/nH5jGABT50k/0.jpg)](https://www.youtube.com/watch?v=nH5jGABT50k)

## Architecture

| File | Role |
|---|---|
| `simulation_engine.R` | 45-day priority-scored simulation loop |
| `contractor_logic.R` | Contractor deployment logic |
| `validate_params.R` | Two-pass parameter validation + sponsor approval gate |
| `build_context.R` | Assembles simulation state for Claude API |
| `ai_recommend_execute.R` | ellmer tool-use wrapper |
| `convo_state_manager.R` | 5-turn history, scenario stack, approve/reject/reset |
| `noaa_monitor.R` | NOAA weather poller, maps alerts to clusters |
| `risk_monitor.R` | Post-scenario SLA breach and queue depth scanner |
| `app.R` | Shiny UI — 4 tabs: Overview, Cluster, Assignment, Stress Tests |
| `config.R` | Constants and configuration |
| `utils.R` | Shared utility functions |
| `console_demo.R` | Console test harness |

## Setup

1. Clone the repo
2. Create a `.Renviron` file in the project root: 3. Install dependencies: `renv::restore()` or install manually
4. Run: `shiny::runApp("app.R")`

## Tools

R, Shiny, ellmer, Claude API (Anthropic), NOAA API, dplyr, readxl, jsonlite

## Note on data

No data files are included in this repo. The simulation runs on your own
roster and claims input files. See Setup above.
