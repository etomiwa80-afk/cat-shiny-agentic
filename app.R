# app.R — CATOPT Shiny Dashboard V2
# CAT Claims Optimization Dashboard · DS 7900 Capstone · KSU
#
# Run 1 + Run 2: pre-computed, loaded from RDS.
# Run 3: on-demand via simulate_contractors() — <1 second, reactive to sliders.
# Enhancements 1–8, 10–12 applied per CATOPT_SHINY_ENHANCEMENTS.md

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(scales)
  library(leaflet)
  library(sf)
  library(plotly)
})

# ── Source CATOPT functions ────────────────────────────────────────────────────
source("catopt/config.R",           local = FALSE)
source("catopt/utils.R",            local = FALSE)
source("catopt/contractor_logic.R", local = FALSE)

# ── Load pre-computed results ──────────────────────────────────────────────────
results <- readRDS("catopt_results.rds")
run1    <- results$run1
run2    <- results$run2
cfg     <- default_config

# ── Static data ───────────────────────────────────────────────────────────────
cluster_states <- list(
  "0" = c("NC", "SC"),
  "1" = c("AL", "GA"),
  "2" = c("PA", "NJ", "NY"),
  "3" = c("TN", "KY"),
  "4" = c("MD", "VA", "DC", "DE"),
  "5" = c("MS", "LA"),
  "6" = c("CT", "MA", "NH", "VT", "ME", "RI")
)

cluster_labels <- c(
  "0" = "C0 — NC / SC",
  "1" = "C1 — AL / GA",
  "2" = "C2 — NJ / NY / PA",
  "3" = "C3 — KY / TN",
  "4" = "C4 — MD / VA / DC / DE",
  "5" = "C5 — MS / LA",
  "6" = "C6 — CT / MA / NE"
)

sev5_per_cluster <- c("0"=30L, "1"=25L, "2"=55L, "3"=47L, "4"=103L, "5"=2L, "6"=16L)

state_to_cluster <- unlist(lapply(names(cluster_states), function(cl)
  setNames(rep(cl, length(cluster_states[[cl]])), cluster_states[[cl]])
))
cat_states <- names(state_to_cluster)

# ── Build cluster polygons via st_union ───────────────────────────────────────
cluster_sf <- tryCatch({
  states_raw <- tigris::states(cb = TRUE, class = "sf", progress_bar = FALSE) |>
    filter(STUSPS %in% cat_states) |>
    select(STUSPS, geometry) |>
    mutate(cluster = state_to_cluster[STUSPS])

  polys <- states_raw |>
    group_by(cluster) |>
    summarise(geometry = sf::st_union(geometry), .groups = "drop")

  if (!inherits(polys, "sf")) polys <- sf::st_sf(polys)

  # Transform to WGS84 so leaflet doesn't warn about datum mismatch
  polys <- sf::st_transform(polys, crs = 4326)

  polys |>
    mutate(
      cluster       = as.character(cluster),
      cluster_label = cluster_labels[cluster],
      n_sev5        = as.integer(sev5_per_cluster[cluster]),
      map_label     = paste0("C", cluster, " — ", n_sev5, " Sev-5")
    )
}, error = function(e) NULL)

# ── Pre-compute claim arrival data (Enhancement 3, static) ────────────────────
arrivals_df <- tryCatch({
  run1$assignments %>%
    group_by(day_arrived) %>%
    summarise(
      all_claims  = n(),
      sev5_claims = sum(severity == 5L),
      .groups = "drop"
    ) %>%
    filter(!is.na(day_arrived)) %>%
    arrange(day_arrived)
}, error = function(e) data.frame(day_arrived=integer(), all_claims=integer(), sev5_claims=integer()))

# ── Max simulation day (for drill-down slider) ────────────────────────────────
max_sim_day <- max(run1$assignments$day_arrived, na.rm = TRUE)

# ── Pre-compute cost-benefit curve (Enhancement 4) ────────────────────────────
cb_curve <- tryCatch({
  total_sev5_all <- sum(sev5_per_cluster)
  alloc_frac     <- sev5_per_cluster / total_sev5_all
  totals         <- c(0L, 10L, 20L, 30L, 45L, 60L, 80L, 100L)

  do.call(rbind, lapply(totals, function(n) {
    counts <- setNames(as.integer(round(alloc_frac * n)), names(sev5_per_cluster))
    r      <- simulate_contractors(run2, counts, cfg)
    data.frame(
      n_contractors = n,
      op_cost       = r$summary$cost$incremental_operating,
      sla_exposure  = r$summary$cost$sla_failure_exposure,
      stringsAsFactors = FALSE
    )
  }))
}, error = function(e) NULL)

# ── Helper functions ───────────────────────────────────────────────────────────
sla_css <- function(pct) {
  if (is.na(pct)) return("sla-red")
  if (pct >= 90) "sla-green" else if (pct >= 70) "sla-amber" else "sla-red"
}

sla_tag_html <- function(pct) {
  cls <- if (pct >= 90) "tag-green" else if (pct >= 70) "tag-amber" else "tag-red"
  lbl <- if (pct >= 90) "Covered" else if (pct >= 70) "Moderate" else "Critical"
  sprintf('<span class="tag %s">%s</span>', cls, lbl)
}

fmt_cost <- function(x) {
  if (is.na(x) || x == 0) return("$0")
  if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
  sprintf("$%sK", formatC(round(x / 1000), format = "d", big.mark = ","))
}

fmt_pct <- function(x) paste0(round(x, 1), "%")

fmt_sev <- function(met, missed) {
  total <- met + missed
  pct   <- if (total > 0) round(met / total * 100, 1) else 100
  sprintf('<span class="%s"><b>%d / %d</b> (%s%%)</span>',
          sla_css(pct), met, missed, pct)
}

map_fill <- function(pct) {
  if (is.na(pct)) return("#f5f5f5")
  if (pct >= 90) "#E1F5EE" else if (pct >= 70) "#FAEEDA" else "#FCEBEB"
}
map_stroke <- function(pct) {
  if (is.na(pct)) return("#cccccc")
  if (pct >= 90) "#1D9E75" else if (pct >= 70) "#EF9F27" else "#E24B4A"
}

cluster_sev5_pct <- function(run_result) {
  run_result$assignments %>%
    filter(severity == 5L) %>%
    mutate(cluster = as.character(cluster)) %>%
    group_by(cluster) %>%
    summarise(
      pct = round(sum(sla_met == TRUE, na.rm = TRUE) / n() * 100, 1),
      .groups = "drop"
    )
}

cluster_sla_pct <- function(run_result, cl) {
  cl  <- as.character(cl[1])
  asgn <- run_result$assignments
  sub  <- asgn[as.character(asgn$cluster) == cl, ]
  if (nrow(sub) == 0) return(list())
  lapply(5:1, function(s) {
    ss <- sub[sub$severity == s, ]
    if (nrow(ss) == 0) return(NULL)
    list(
      sev  = s,
      met  = sum(ss$sla_met == TRUE,  na.rm = TRUE),
      miss = sum(ss$sla_met == FALSE, na.rm = TRUE),
      pct  = round(sum(ss$sla_met == TRUE, na.rm = TRUE) / nrow(ss) * 100, 1)
    )
  }) %>% Filter(Negate(is.null), .)
}

# Delta column helpers (Enhancement 2)
delta_sev_html <- function(d_pct) {
  if (is.na(d_pct) || !is.finite(d_pct) || abs(d_pct) < 0.05)
    return('<span style="color:#aaa;">—</span>')
  if (d_pct > 0)
    sprintf('<span style="color:#0F6E56;font-weight:700;">+%.1f%%</span>', d_pct)
  else
    sprintf('<span style="color:#A32D2D;font-weight:700;">%.1f%%</span>', d_pct)
}

delta_cost_html <- function(d_cost, invert_good = FALSE) {
  if (is.na(d_cost) || !is.finite(d_cost) || abs(d_cost) < 100)
    return('<span style="color:#aaa;">—</span>')
  abs_val  <- abs(d_cost)
  sign_chr <- if (d_cost >= 0) "+" else "-"
  fmt_val  <- if (abs_val >= 1e6) sprintf("$%.1fM", abs_val / 1e6)
              else sprintf("$%sK", formatC(round(abs_val / 1000), format = "d", big.mark = ","))
  full_str <- paste0(sign_chr, fmt_val)

  is_good <- if (invert_good) d_cost < 0 else d_cost > 0
  col     <- if (is_good) "#0F6E56" else "#EF9F27"
  arrow   <- if (d_cost > 0) "" else " ↓"
  sprintf('<span style="color:%s;font-weight:700;">%s%s</span>', col, full_str, arrow)
}


# ── CSS ────────────────────────────────────────────────────────────────────────
app_css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: #f7f8fc; }
.navbar { background: #002060 !important; border: none; box-shadow: 0 2px 8px rgba(0,0,0,0.18); }
.navbar-brand, .navbar-nav > li > a { color: #fff !important; }
.navbar-nav > li > a:hover { color: #FFC52E !important; }
.navbar-brand { display:flex; align-items:center; font-weight:700; letter-spacing:0.5px; }

/* Dynamic headline */
.dynamic-headline {
  background: #fff; border-left: 4px solid #002060;
  border-radius: 8px; padding: 14px 18px;
  font-size: 14px; color: #222; line-height: 1.6;
  box-shadow: 0 1px 4px rgba(0,0,0,0.06);
  margin-bottom: 18px;
}

.section-header {
  background: #002060; color: #fff;
  padding: 8px 14px; border-radius: 6px;
  font-size: 14px; font-weight: 500;
  margin-bottom: 10px;
}
.gold-header {
  background: #FFC52E; color: #002060;
  padding: 8px 14px; border-radius: 6px;
  font-size: 14px; font-weight: 600;
  margin-bottom: 10px;
}

/* KPI cards */
.kpi-card {
  background: #fff; border: 1px solid #e8eaed;
  border-radius: 12px; text-align: center;
  padding: 18px 10px; margin-bottom: 16px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.06);
  transition: box-shadow 0.2s ease;
}
.kpi-card:hover { box-shadow: 0 3px 12px rgba(0,32,96,0.12); }
.kpi-value { font-size: 26px; font-weight: 700; margin: 0; line-height: 1.2; }
.kpi-label { font-size: 11px; color: #888; margin-top: 5px; }
.kpi-sub   { font-size: 12px; color: #aaa; margin-top: 3px; }

@keyframes kpi-flash {
  0%   { background: #fff; }
  30%  { background: rgba(255,197,46,0.2); }
  100% { background: #fff; }
}
.kpi-card.flash { animation: kpi-flash 0.5s ease forwards; }

@keyframes cost-pulse {
  0%   { background: #f4f6fa; }
  35%  { background: rgba(255,197,46,0.32); }
  100% { background: #f4f6fa; }
}
.slider-total.pulse { animation: cost-pulse 0.45s ease forwards; }

.sla-green { color: #0F6E56; }
.sla-amber { color: #854F0B; }
.sla-red   { color: #A32D2D; }

.tag {
  display: inline-block; padding: 2px 10px;
  border-radius: 12px; font-size: 11px; font-weight: 600;
}
.tag-red   { background: #FCEBEB; color: #791F1F; }
.tag-amber { background: #FAEEDA; color: #633806; }
.tag-green { background: #E1F5EE; color: #085041; }

/* Gap bars with CSS transition */
.gap-bar-wrap { background: #f0f0f0; border-radius: 4px; height: 8px; margin: 3px 0 8px 0; }
.gap-bar-fill { height: 8px; border-radius: 4px; transition: width 0.35s cubic-bezier(0.4,0,0.2,1); }

/* Comparison table */
.comparison-table { width: 100%; border-collapse: collapse; font-size: 13px; }
.comparison-table th {
  background: #002060; color: #fff;
  padding: 8px 12px; text-align: left;
}
.comparison-table th.run3-col  { background: #FFC52E; color: #002060; }
.comparison-table th.delta-col { background: #1D9E75; color: #fff; font-size: 11px; min-width: 80px; }
.comparison-table td { padding: 7px 12px; border-bottom: 1px solid #f0f0f0; }
.comparison-table tr:nth-child(even) td { background: #fafafa; }
.comparison-table tr.section-divider td {
  background: #f4f6fa; font-weight: 600;
  font-size: 12px; color: #555; padding: 4px 12px;
}

.cluster-card {
  background: #fff; border-radius: 10px;
  border: 1px solid #e8eaed; padding: 14px;
  margin-bottom: 10px; cursor: pointer;
}
.cluster-card:hover { box-shadow: 0 2px 8px rgba(0,32,96,0.1); }

.detail-panel {
  background: #fff; border-radius: 10px;
  border: 1px solid #e8eaed; padding: 16px;
  min-height: 400px;
}

.callout-amber {
  background: #FAEEDA; border-left: 4px solid #EF9F27;
  border-radius: 6px; padding: 10px 14px;
  font-size: 13px; margin-top: 12px;
}
.callout-green {
  background: #E1F5EE; border-left: 4px solid #1D9E75;
  border-radius: 6px; padding: 10px 14px;
  font-size: 13px; margin-top: 12px;
}
.callout-navy {
  background: #EEF2FA; border-left: 4px solid #002060;
  border-radius: 6px; padding: 10px 14px;
  font-size: 13px; margin-top: 12px; margin-bottom: 12px;
}

/* Claim detail card */
.claim-detail-card {
  background: #fff; border-radius: 10px;
  border: 1px solid #dde1e8; padding: 18px;
  margin-top: 16px;
  box-shadow: 0 2px 8px rgba(0,32,96,0.08);
}
.claim-detail-card h5 { margin-bottom: 12px; color: #002060; }
.detail-section {
  background: #f9fafb; border-radius: 8px;
  padding: 12px 14px; margin-bottom: 10px;
  font-size: 13px;
}
.detail-row {
  display: flex; justify-content: space-between;
  padding: 3px 0; border-bottom: 1px solid #eee;
}
.detail-row:last-child { border-bottom: none; }
.detail-label { color: #888; }
.detail-value { font-weight: 600; color: #222; }
.adj-box {
  background: #E6F1FB; border-radius: 8px;
  padding: 12px 14px; margin-bottom: 10px;
  font-size: 13px;
}
.why-box {
  background: #FAEEDA; border-left: 4px solid #EF9F27;
  border-radius: 6px; padding: 12px 14px;
  margin-bottom: 10px; font-size: 13px;
}

.footer-note {
  font-size: 11px; color: #999;
  border-top: 1px solid #eee;
  padding-top: 10px; margin-top: 16px;
}

.slider-total {
  font-size: 13px; color: #444;
  padding: 8px 12px; background: #f4f6fa;
  border-radius: 6px; margin-bottom: 8px;
  font-weight: 500;
}

/* Slider value badges */
.slider-val-badge {
  display: inline-block; font-size: 13px; font-weight: 700;
  margin-left: 6px; min-width: 20px;
  transition: color 0.25s ease;
}
.badge-zero { color: #bbb; }
.badge-low  { color: #EF9F27; }
.badge-high { color: #1D9E75; }

/* Daily Drill-Down stat cards */
.drill-stat {
  background: #fff; border-radius: 8px;
  border: 1px solid #e8eaed; padding: 12px 8px;
  text-align: center; margin-bottom: 10px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05);
}
.drill-stat-val { font-size: 24px; font-weight: 700; line-height: 1.1; }
.drill-stat-lbl { font-size: 11px; color: #888; margin-top: 4px; }
.drill-stat-sub { font-size: 11px; margin-top: 3px; }

/* Download button */
.btn-download {
  background: #002060; color: #fff;
  border: none; border-radius: 6px;
  padding: 6px 14px; font-size: 13px;
  cursor: pointer; margin-top: 6px;
}
.btn-download:hover { background: #003080; color: #FFC52E; }
"

# ── JavaScript ────────────────────────────────────────────────────────────────
app_js <- "
$(document).ready(function() {

  // Slider badge color feedback
  function applyBadge(name, val) {
    val = parseInt(val) || 0;
    var suffix = name.replace('sl_c', '');
    var $b = $('#sl_c' + suffix + '_badge');
    if ($b.length === 0) return;
    $b.text(val);
    $b.removeClass('badge-zero badge-low badge-high');
    if (val === 0)       $b.addClass('badge-zero');
    else if (val >= 15)  $b.addClass('badge-high');
    else                 $b.addClass('badge-low');
  }

  $(document).on('shiny:inputchanged', function(ev) {
    if (ev.name && /^sl_c[0-6]$/.test(ev.name)) {
      applyBadge(ev.name, ev.value);

      // Pulse slider-total
      var $tot = $('.slider-total');
      $tot.removeClass('pulse');
      void ($tot[0] && $tot[0].offsetWidth);
      $tot.addClass('pulse');
      setTimeout(function(){ $tot.removeClass('pulse'); }, 500);

      // Flash KPI cards
      $('.kpi-card').removeClass('flash');
      void document.body.offsetWidth;
      $('.kpi-card').addClass('flash');
      setTimeout(function(){ $('.kpi-card').removeClass('flash'); }, 550);
    }
  });

  // Initialize badges to 0 on load
  ['sl_c0','sl_c1','sl_c2','sl_c3','sl_c4','sl_c5','sl_c6'].forEach(function(id){
    applyBadge(id, 0);
  });
});
"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = tags$span(
    style = "display:flex; align-items:center;",
    tags$b("CATOPT", style = "color:#FFC52E;"),
    tags$span(" — CAT Claims Allocation Optimizer",
              style = "color:#cdd3db; font-size:0.88em; margin-left:6px;")
  ),
  windowTitle = "CATOPT",
  id = "main_nav",
  header = tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),

  # ── Tab 1: Overview ──────────────────────────────────────────────────────────
  tabPanel(
    "Overview",
    fluidPage(
      br(),

      # Dynamic headline (Enhancement 1)
      uiOutput("dynamic_headline"),

      # KPI Cards
      fluidRow(
        column(3, uiOutput("kpi_overall")),
        column(3, uiOutput("kpi_sev5")),
        column(3, uiOutput("kpi_contractors")),
        column(3, uiOutput("kpi_cost"))
      ),

      # Comparison table + Gap bars
      fluidRow(
        column(7,
          div(class = "section-header", icon("table"), " Three-Pass Comparison"),
          uiOutput("comparison_table")
        ),
        column(5,
          div(class = "section-header", icon("chart-bar"), " Sev-5 Gap by Cluster"),
          uiOutput("gap_bars")
        )
      ),

      br(),

      # Sliders
      fluidRow(
        column(12,
          div(class = "gold-header",
              icon("sliders-h"),
              " Contractor Deployment — PL5 · Sev-5 Only · Pre-positioned Day 1"
          ),
          fluidRow(
            lapply(0:6, function(cl) {
              cl_str <- as.character(cl)
              n5     <- sev5_per_cluster[cl_str]
              column(
                width = if (cl < 4) 3 else if (cl < 6) 4 else 4,
                sliderInput(
                  inputId = paste0("sl_c", cl_str),
                  label   = HTML(sprintf(
                    "<span style='font-size:12px;font-weight:600;'>%s</span>
                     <span style='font-size:11px;color:#666;'> · %d Sev-5</span>
                     <span id='sl_c%s_badge' class='slider-val-badge badge-zero'>0</span>",
                    cluster_labels[cl_str], n5, cl_str
                  )),
                  min   = 0,
                  max   = as.integer(ceiling(n5 * 0.65)),
                  value = 0,
                  step  = 1,
                  ticks = FALSE
                )
              )
            })
          ),
          fluidRow(
            column(4, uiOutput("slider_total_line")),
            column(2,
              actionButton("btn_reset", "Reset All",
                           class = "btn btn-outline-secondary btn-sm",
                           style = "margin-top:6px;")
            ),
            column(2,
              actionButton("btn_suggest", "Suggested Preset",
                           class = "btn btn-outline-warning btn-sm",
                           style = "margin-top:6px;")
            ),
            column(2,
              downloadButton("download_plan", "Download Plan",
                             class = "btn btn-sm btn-download",
                             style = "margin-top:6px;")
            )
          )
        )
      ),

      br(),

      # Claim Reporting Chart (Enhancement 3)
      fluidRow(
        column(12,
          div(class = "section-header", icon("calendar"), " Claim Reporting Timeline — When Claims Arrive"),
          plotlyOutput("plot_arrivals", height = "280px"),
          div(class = "callout-amber",
              style = "margin-top:8px;",
              icon("exclamation-triangle"), " ",
              "47% of claims report within 24 hours. 73% by Day 3. But ",
              tags$b("16% arrive after Day 7"),
              " — those Sev-5 claims have already exceeded their SLA window before any adjuster can reach them."
          )
        )
      ),

      br(),

      # Cumulative Chart (Enhancement 11 — plotly)
      fluidRow(
        column(12,
          div(class = "section-header", icon("line-chart"), " Cumulative Claims Resolved by Day"),
          plotlyOutput("plot_daily", height = "320px")
        )
      ),

      br(),

      # Cost-Benefit Chart (Enhancement 4)
      fluidRow(
        column(12,
          div(class = "section-header", icon("balance-scale"), " Cost vs. SLA Exposure Tradeoff"),
          plotlyOutput("plot_costbenefit", height = "300px"),
          uiOutput("costbenefit_callout")
        )
      ),

      # Daily Drill-Down
      fluidRow(
        column(12,
          div(class = "section-header", icon("search"), " Daily Drill-Down — What Happened on Day X?"),
          fluidRow(
            column(4,
              sliderInput("drill_day", "Select Simulation Day",
                min = 1, max = max_sim_day, value = 3, step = 1, ticks = FALSE)
            ),
            column(3,
              selectInput("drill_run", "Run",
                choices  = c("Run 1 (Raw)" = "run1",
                             "Run 2 (PL0 Boost)" = "run2",
                             "Run 3 (Contractors)" = "run3"),
                selected = "run2")
            ),
            column(5,
              div(style = "padding-top:26px; font-size:12px; color:#888;",
                  "Select a day to see every claim that arrived, who handled it,",
                  " and the end-of-day workforce snapshot.")
            )
          ),
          uiOutput("drill_summary"),
          br(),
          DTOutput("drill_tbl")
        )
      ),

      br(),

      # Footer note
      div(class = "footer-note",
          "All contractors are PL5 (only skill that closes Sev-5 in 7 days). Pre-positioned Day 1. ",
          "Cost = $500/day × days worked. ",
          "Est. SLA failure exposure: $5K/Sev-5 · $2.5K/Sev-4 · $1K/Sev-3 · $500/Sev-2 · $100/Sev-1."
      ),

      br()
    )
  ),

  # ── Tab 2: Cluster Detail ────────────────────────────────────────────────────
  tabPanel(
    "Cluster Detail",
    fluidPage(
      br(),
      fluidRow(
        column(7,
          div(class = "section-header", icon("map"), " Sev-5 Compliance by Cluster"),
          tags$p(
            "Cluster regions colored by Sev-5 SLA compliance. Click a cluster for details.",
            style = "font-size:12px; color:#888; margin-bottom:8px;"
          ),
          leafletOutput("map_clusters", height = "480px")
        ),
        column(5,
          div(class = "section-header", icon("info-circle"), " Cluster Detail"),
          uiOutput("cluster_detail_panel")
        )
      ),
      br()
    )
  ),

  # ── Tab 3: Assignment Detail ─────────────────────────────────────────────────
  tabPanel(
    "Assignment Detail",
    fluidPage(
      br(),
      fluidRow(
        column(2, selectInput("flt_run",     "Run",
          choices  = c("Run 2 (PL0 Boost)"  = "run2",
                       "Run 1 (Raw)"         = "run1",
                       "Run 3 (Contractors)" = "run3"),
          selected = "run2"
        )),
        column(2, selectInput("flt_cluster", "Cluster",
          choices  = c("All", as.character(0:6)), selected = "All")),
        column(2, selectInput("flt_sev",     "Severity",
          choices  = c("All", "5", "4", "3", "2", "1"), selected = "All")),
        column(2, selectInput("flt_sla",     "SLA Status",
          choices  = c("All", "Met", "Missed"), selected = "All")),
        column(2, selectInput("flt_type",    "Assignment Type",
          choices  = c("All", "local", "deployed", "contractor"), selected = "All"))
      ),
      div(style="font-size:11px;color:#888;margin:4px 0 6px;",
          textOutput("tbl_row_count", inline=TRUE)),
      DTOutput("tbl_assignments"),
      uiOutput("claim_detail_card"),
      br()
    )
  ),

  footer = tags$div(
    style = "background:#002060; color:#8fa3bf; text-align:center;
             padding:10px; font-size:11px; margin-top:20px; letter-spacing:0.3px;",
    "CAT Code 82 · December 2023 · CATOPT v2 · DS 7900 Capstone · Kennesaw State University"
  )
)


# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Core reactive: Run 3 ───────────────────────────────────────────────────
  run3 <- reactive({
    counts <- c(
      "0" = input$sl_c0, "1" = input$sl_c1,
      "2" = input$sl_c2, "3" = input$sl_c3,
      "4" = input$sl_c4, "5" = input$sl_c5,
      "6" = input$sl_c6
    )
    simulate_contractors(run2, counts, cfg)
  })

  total_contractors <- reactive({
    sum(input$sl_c0, input$sl_c1, input$sl_c2, input$sl_c3,
        input$sl_c4, input$sl_c5, input$sl_c6)
  })

  selected_cluster  <- reactiveVal("4")
  selected_claim_id <- reactiveVal(NULL)

  # Slider controls
  observeEvent(input$btn_reset, {
    for (cl in 0:6) updateSliderInput(session, paste0("sl_c", cl), value = 0)
  })

  observeEvent(input$btn_suggest, {
    preset <- c("0"=5, "1"=3, "2"=8, "3"=10, "4"=15, "5"=1, "6"=3)
    for (cl in 0:6)
      updateSliderInput(session, paste0("sl_c", cl), value = preset[as.character(cl)])
  })

  # ── Dynamic headline (Enhancement 1) ──────────────────────────────────────
  output$dynamic_headline <- renderUI({
    r3  <- run3()
    n   <- total_contractors()
    s2m <- run2$summary$by_severity$sev5$missed
    s3  <- r3$summary$by_severity$sev5

    if (n == 0) {
      exp_str <- fmt_cost(run2$summary$cost$sla_failure_exposure)
      txt <- sprintf(
        "Without contractors, <b>%d</b> Sev-5 claims miss their 7-day SLA — estimated <b>%s</b> in failure exposure.",
        s2m, exp_str
      )
    } else {
      saved     <- run2$summary$cost$sla_failure_exposure - r3$summary$cost$sla_failure_exposure
      cc        <- r3$summary$cost$contractor_cost
      ratio     <- if (cc > 0) round(saved / cc, 1) else NA
      ratio_str <- if (!is.na(ratio) && is.finite(ratio))
                     sprintf(" (<b>%.1fx</b> return.)", ratio) else ""
      txt <- sprintf(
        "<b>%d</b> PL5 contractors reduce Sev-5 misses from <b>%d</b> → <b>%d</b> — avoiding <b>%s</b> in failure exposure for <b>%s</b> in contractor cost.%s",
        n, s2m, s3$missed, fmt_cost(saved), fmt_cost(cc), ratio_str
      )
    }
    div(class = "dynamic-headline", HTML(txt))
  })

  # ── KPI Cards ──────────────────────────────────────────────────────────────
  kpi_card <- function(value_html, label, sub = NULL, border_color = "#002060") {
    div(
      class = "kpi-card",
      style = paste0("border-top: 4px solid ", border_color, ";"),
      div(class = "kpi-value", HTML(value_html)),
      div(class = "kpi-label", label),
      if (!is.null(sub)) div(class = "kpi-sub", sub)
    )
  }

  output$kpi_overall <- renderUI({
    pct <- run3()$summary$overall_compliance
    col <- if (pct >= 90) "#1D9E75" else if (pct >= 70) "#EF9F27" else "#E24B4A"
    kpi_card(
      sprintf('<span style="color:%s">%s%%</span>', col, round(pct, 1)),
      "Overall SLA Compliance", "Run 3 — with contractors", col
    )
  })

  output$kpi_sev5 <- renderUI({
    s   <- run3()$summary$by_severity$sev5
    col <- if (s$pct >= 90) "#1D9E75" else if (s$pct >= 70) "#EF9F27" else "#E24B4A"
    kpi_card(
      sprintf('<span style="color:%s">%s%%</span>', col, round(s$pct, 1)),
      "Sev-5 SLA Compliance",
      sprintf("7-day window · %d met / %d missed", s$met, s$missed),
      col
    )
  })

  output$kpi_contractors <- renderUI({
    n <- total_contractors()
    kpi_card(
      sprintf('<span style="color:#002060">%d</span>', n),
      "PL5 Contractors", "Pre-positioned Day 1", "#002060"
    )
  })

  output$kpi_cost <- renderUI({
    val <- run3()$summary$cost$incremental_operating
    kpi_card(
      sprintf('<span style="color:#EF9F27">%s</span>', fmt_cost(val)),
      "Incremental Operating Cost", "Per diem + contractor days", "#EF9F27"
    )
  })

  # ── Comparison table (Enhancement 2 — delta column) ───────────────────────
  output$comparison_table <- renderUI({
    r3 <- run3()
    n_contr <- if (!is.null(r3$contractor_log)) nrow(r3$contractor_log) else 0L

    sev_pct <- function(s) {
      m <- s$met; ms <- s$missed; tot <- m + ms
      if (tot > 0) round(m / tot * 100, 1) else 100
    }

    sev_row <- function(sev_n, window) {
      s1 <- run1$summary$by_severity[[paste0("sev", sev_n)]]
      s2 <- run2$summary$by_severity[[paste0("sev", sev_n)]]
      s3 <- r3$summary$by_severity[[paste0("sev", sev_n)]]
      dp    <- sev_pct(s3) - sev_pct(s1)
      dp23  <- sev_pct(s3) - sev_pct(s2)
      tip3  <- sprintf("vs Run 2: %+.1f%% (%+d claims met)", dp23, s3$met - s2$met)
      tags$tr(
        tags$td(HTML(sprintf(
          "Sev-%d <span style='color:#aaa;font-size:11px;'>(%d-day SLA)</span>",
          sev_n, window))),
        tags$td(HTML(fmt_sev(s1$met, s1$missed))),
        tags$td(HTML(fmt_sev(s2$met, s2$missed))),
        tags$td(title = tip3, HTML(fmt_sev(s3$met, s3$missed))),
        tags$td(HTML(delta_sev_html(dp)))
      )
    }

    val_row <- function(label, v1, v2, v3, delta_html_str, tip3 = "") {
      tags$tr(
        tags$td(label),
        tags$td(tags$b(v1)),
        tags$td(tags$b(v2)),
        tags$td(title = tip3, tags$b(v3)),
        tags$td(HTML(delta_html_str))
      )
    }

    div_row <- function(label) {
      tags$tr(class = "section-divider",
        tags$td(colspan = 5, label)
      )
    }

    r1oc  <- fmt_cost(run1$summary$cost$incremental_operating)
    r2oc  <- fmt_cost(run2$summary$cost$incremental_operating)
    r3oc  <- fmt_cost(r3$summary$cost$incremental_operating)
    r1exp <- fmt_cost(run1$summary$cost$sla_failure_exposure)
    r2exp <- fmt_cost(run2$summary$cost$sla_failure_exposure)
    r3exp <- fmt_cost(r3$summary$cost$sla_failure_exposure)

    d_oc  <- r3$summary$cost$incremental_operating  - run1$summary$cost$incremental_operating
    d_exp <- r3$summary$cost$sla_failure_exposure   - run1$summary$cost$sla_failure_exposure

    d_oc23  <- r3$summary$cost$incremental_operating - run2$summary$cost$incremental_operating
    d_exp23 <- r3$summary$cost$sla_failure_exposure  - run2$summary$cost$sla_failure_exposure

    r1pct <- sprintf('<span class="%s">%s%%</span>',
                     sla_css(run1$summary$overall_compliance),
                     run1$summary$overall_compliance)
    r2pct <- sprintf('<span class="%s">%s%%</span>',
                     sla_css(run2$summary$overall_compliance),
                     run2$summary$overall_compliance)
    r3pct <- sprintf('<span class="%s">%s%%</span>',
                     sla_css(r3$summary$overall_compliance),
                     r3$summary$overall_compliance)
    d_compliance <- r3$summary$overall_compliance - run1$summary$overall_compliance
    d_comp23     <- r3$summary$overall_compliance - run2$summary$overall_compliance

    tags$table(
      class = "comparison-table",
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          tags$th("Run 1 — Raw"),
          tags$th("Run 2 — PL0 Boost"),
          tags$th(class = "run3-col", paste0("Run 3 — Contractors (", total_contractors(), ")")),
          tags$th(class = "delta-col", "Δ R1 → R3")
        )
      ),
      tags$tbody(
        sev_row(5,  7),
        sev_row(4, 14),
        sev_row(3, 21),
        sev_row(2, 30),
        sev_row(1, 45),
        div_row("Summary"),
        tags$tr(
          tags$td(tags$b("Overall Compliance")),
          tags$td(HTML(r1pct)),
          tags$td(HTML(r2pct)),
          tags$td(title = sprintf("vs Run 2: %+.1f%%", d_comp23), HTML(r3pct)),
          tags$td(HTML(delta_sev_html(d_compliance)))
        ),
        tags$tr(
          tags$td("Contractors Deployed"),
          tags$td("0"), tags$td("0"),
          tags$td(tags$b(as.character(n_contr))),
          tags$td(HTML(sprintf('<span style="color:#888;">—</span>')))
        ),
        div_row("Costs"),
        val_row("Incremental Op. Cost",   r1oc, r2oc, r3oc,
                delta_cost_html(d_oc, invert_good = FALSE),
                sprintf("vs Run 2: %s", fmt_cost(d_oc23))),
        val_row("Est. SLA Failure Exposure", r1exp, r2exp, r3exp,
                delta_cost_html(d_exp, invert_good = TRUE),
                sprintf("vs Run 2: %s", fmt_cost(d_exp23)))
      )
    )
  })

  # ── Gap bars ───────────────────────────────────────────────────────────────
  output$gap_bars <- renderUI({
    r3 <- run3()
    sev5_df <- cluster_sev5_pct(r3)

    rows <- lapply(0:6, function(cl) {
      cl_str <- as.character(cl)
      row    <- sev5_df[sev5_df$cluster == cl_str, ]
      pct    <- if (nrow(row) == 0) 0 else row$pct[1]
      n5     <- sev5_per_cluster[cl_str]
      bar_col <- if (pct >= 90) "#1D9E75" else if (pct >= 70) "#EF9F27" else "#E24B4A"
      list(cl = cl_str, pct = pct, n5 = n5, bar_col = bar_col)
    })

    rows_sorted <- rows[order(sapply(rows, `[[`, "pct"))]

    div(
      lapply(rows_sorted, function(r) {
        div(
          style = "margin-bottom: 10px;",
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            tags$span(
              style = "font-size:13px; font-weight:600; color:#002060;",
              cluster_labels[r$cl]
            ),
            div(
              HTML(sla_tag_html(r$pct)),
              tags$span(style = "font-size:11px; color:#888; margin-left:6px;",
                        paste0(r$n5, " Sev-5"))
            )
          ),
          div(
            class = "gap-bar-wrap",
            div(
              class = "gap-bar-fill",
              style = sprintf("width:%s%%; background:%s;", min(r$pct, 100), r$bar_col)
            )
          ),
          tags$span(
            style = paste0("font-size:12px; color:", r$bar_col, "; font-weight:600;"),
            fmt_pct(r$pct)
          )
        )
      })
    )
  })

  # ── Slider total line ──────────────────────────────────────────────────────
  output$slider_total_line <- renderUI({
    n   <- total_contractors()
    est <- n * 4 * cfg$cost_contractor_incremental
    div(
      class = "slider-total",
      sprintf("Total: %d PL5 contractor%s · Est. cost: %s",
              n, if (n == 1) "" else "s", fmt_cost(est))
    )
  })

  # ── Claim Arrivals Chart (Enhancement 3 — static plotly) ──────────────────
  output$plot_arrivals <- renderPlotly({
    if (nrow(arrivals_df) == 0) {
      return(plot_ly() %>% layout(title = "No day_arrived data available"))
    }

    max_day <- max(arrivals_df$day_arrived, 22L)

    p <- plot_ly(arrivals_df, x = ~day_arrived) %>%
      add_bars(y = ~all_claims, name = "All Claims",
               marker = list(color = "#002060", opacity = 0.85),
               hovertemplate = "Day %{x}<br>All Claims: %{y}<extra></extra>") %>%
      add_bars(y = ~sev5_claims, name = "Sev-5 Claims",
               marker = list(color = "#E24B4A", opacity = 0.9),
               hovertemplate = "Day %{x}<br>Sev-5: %{y}<extra></extra>") %>%
      layout(
        barmode = "overlay",
        xaxis = list(
          title = "Simulation Day",
          range = c(0.5, max_day + 0.5),
          tickmode = "linear", tick0 = 1, dtick = 1,
          gridcolor = "#f2f2f2"
        ),
        yaxis = list(title = "Claims Arriving", gridcolor = "#f2f2f2"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin = list(t = 30, r = 20),
        shapes = list(list(
          type = "line", x0 = 7, x1 = 7, y0 = 0, y1 = 1,
          yref = "paper",
          line = list(color = "#E24B4A", dash = "dash", width = 2)
        )),
        annotations = list(list(
          x = 7.15, y = 0.95, xref = "x", yref = "paper",
          text = "Sev-5 SLA deadline\n(Day 1 arrivals)",
          showarrow = FALSE, font = list(size = 10, color = "#E24B4A"),
          xanchor = "left"
        ))
      ) %>%
      config(displayModeBar = FALSE)

    p
  })

  # ── Cumulative Chart (Enhancement 11 — plotly with direct labels) ──────────
  output$plot_daily <- renderPlotly({
    r3_log <- run3()$daily_log

    days_all <- sort(unique(c(run1$daily_log$day, run2$daily_log$day, r3_log$day)))
    max_cum  <- max(run1$daily_log$claims_done_cum,
                    run2$daily_log$claims_done_cum,
                    r3_log$claims_done_cum, na.rm = TRUE)

    r1_end <- tail(run1$daily_log$claims_done_cum, 1)
    r2_end <- tail(run2$daily_log$claims_done_cum, 1)
    r3_end <- tail(r3_log$claims_done_cum, 1)
    r1_pct <- round(run1$summary$overall_compliance, 1)
    r2_pct <- round(run2$summary$overall_compliance, 1)
    r3_pct <- round(run3()$summary$overall_compliance, 1)

    p <- plot_ly() %>%
      # SLA vlines
      add_trace(x = c(7, 7),   y = c(0, max_cum * 1.05), type = "scatter", mode = "lines",
                line = list(color = "#E24B4A", dash = "dash", width = 1.5),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(14, 14), y = c(0, max_cum * 1.05), type = "scatter", mode = "lines",
                line = list(color = "#EF9F27", dash = "dash", width = 1.2),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(21, 21), y = c(0, max_cum * 1.05), type = "scatter", mode = "lines",
                line = list(color = "#aaa", dash = "dash", width = 1),
                showlegend = FALSE, hoverinfo = "none") %>%
      # Run lines
      add_trace(data = run1$daily_log, x = ~day, y = ~claims_done_cum,
                type = "scatter", mode = "lines", name = "Run 1: Raw",
                line = list(color = "#002060", width = 2),
                hovertemplate = "Day %{x}<br>Run 1: %{y:,}<extra></extra>") %>%
      add_trace(data = run2$daily_log, x = ~day, y = ~claims_done_cum,
                type = "scatter", mode = "lines", name = "Run 2: PL0 Boost",
                line = list(color = "#FFC52E", width = 2),
                hovertemplate = "Day %{x}<br>Run 2: %{y:,}<extra></extra>") %>%
      add_trace(data = r3_log, x = ~day, y = ~claims_done_cum,
                type = "scatter", mode = "lines", name = "Run 3: Contractors",
                line = list(color = "#1D9E75", width = 2.5),
                hovertemplate = "Day %{x}<br>Run 3: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(
          title = "Simulation Day",
          tickvals = c(1, 7, 14, 21, 30, 45),
          gridcolor = "#f2f2f2"
        ),
        yaxis = list(title = "Cumulative Claims Resolved", gridcolor = "#f2f2f2",
                     tickformat = ",d"),
        legend = list(orientation = "h", x = 0, y = -0.18),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin = list(t = 40, r = 100),
        annotations = list(
          list(x = 7.2,  y = max_cum * 0.06, text = "Sev-5 SLA",
               showarrow = FALSE, font = list(size = 9, color = "#E24B4A"), xanchor = "left"),
          list(x = 14.2, y = max_cum * 0.06, text = "Sev-4 SLA",
               showarrow = FALSE, font = list(size = 9, color = "#EF9F27"), xanchor = "left"),
          list(x = 21.2, y = max_cum * 0.06, text = "Sev-3 SLA",
               showarrow = FALSE, font = list(size = 9, color = "#aaa"), xanchor = "left"),
          # Direct end-of-line labels
          list(x = tail(run1$daily_log$day, 1) + 0.5, y = r1_end,
               text = sprintf("Run 1: %s%%", r1_pct),
               showarrow = FALSE, font = list(size = 10, color = "#002060"),
               xanchor = "left"),
          list(x = tail(run2$daily_log$day, 1) + 0.5, y = r2_end,
               text = sprintf("Run 2: %s%%", r2_pct),
               showarrow = FALSE, font = list(size = 10, color = "#B8900A"),
               xanchor = "left"),
          list(x = tail(r3_log$day, 1) + 0.5, y = r3_end,
               text = sprintf("Run 3: %s%%", r3_pct),
               showarrow = FALSE, font = list(size = 10, color = "#1D9E75"),
               xanchor = "left")
        )
      ) %>%
      config(displayModeBar = FALSE)

    p
  })

  # ── Cost-Benefit Chart (Enhancement 4) ────────────────────────────────────
  output$plot_costbenefit <- renderPlotly({
    if (is.null(cb_curve)) {
      return(plot_ly() %>% layout(title = "Cost-benefit data unavailable"))
    }

    r3     <- run3()
    dot_n  <- total_contractors()
    dot_oc <- r3$summary$cost$incremental_operating
    dot_ex <- r3$summary$cost$sla_failure_exposure

    net <- cb_curve$sla_exposure[1] - cb_curve$sla_exposure -
           (cb_curve$op_cost - cb_curve$op_cost[1])

    p <- plot_ly(cb_curve) %>%
      add_trace(x = ~n_contractors, y = ~op_cost, name = "Incremental Cost",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#002060", width = 2.5),
                marker = list(color = "#002060", size = 5),
                hovertemplate = "%{x} contractors<br>Op Cost: $%{y:,.0f}<extra></extra>") %>%
      add_trace(x = ~n_contractors, y = ~sla_exposure, name = "SLA Failure Exposure",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#E24B4A", width = 2.5),
                marker = list(color = "#E24B4A", size = 5),
                hovertemplate = "%{x} contractors<br>Exposure: $%{y:,.0f}<extra></extra>") %>%
      add_trace(x = cb_curve$n_contractors, y = net, name = "Net Savings",
                type = "scatter", mode = "lines",
                line = list(color = "#1D9E75", width = 2, dash = "dash"),
                hovertemplate = "%{x} contractors<br>Net Savings: $%{y:,.0f}<extra></extra>") %>%
      # Reactive dot — current scenario
      add_trace(x = c(dot_n), y = c(dot_oc), name = "Current (Cost)",
                type = "scatter", mode = "markers",
                marker = list(color = "#002060", size = 14, symbol = "circle",
                              line = list(color = "white", width = 2)),
                hovertemplate = paste0("Current scenario<br>",
                                       dot_n, " contractors<br>",
                                       "Op Cost: $", formatC(dot_oc, format="d", big.mark=","),
                                       "<extra></extra>"),
                showlegend = FALSE) %>%
      add_trace(x = c(dot_n), y = c(dot_ex), name = "Current (Exposure)",
                type = "scatter", mode = "markers",
                marker = list(color = "#E24B4A", size = 14, symbol = "circle",
                              line = list(color = "white", width = 2)),
                hovertemplate = paste0("Current scenario<br>",
                                       dot_n, " contractors<br>",
                                       "Exposure: $", formatC(dot_ex, format="d", big.mark=","),
                                       "<extra></extra>"),
                showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Total PL5 Contractors", gridcolor = "#f2f2f2"),
        yaxis = list(title = "Dollars ($)", gridcolor = "#f2f2f2", tickformat = "$,.0f"),
        legend = list(orientation = "h", x = 0, y = -0.2),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin = list(t = 20, r = 20),
        shapes = list(list(
          type = "line", x0 = dot_n, x1 = dot_n, y0 = 0, y1 = 1,
          yref = "paper",
          line = list(color = "#888", dash = "dot", width = 1)
        ))
      ) %>%
      config(displayModeBar = FALSE)

    p
  })

  output$costbenefit_callout <- renderUI({
    r3 <- run3()
    n  <- total_contractors()
    if (n == 0) return(NULL)
    contr_cost  <- r3$summary$cost$contractor_cost
    exp_avoided <- run2$summary$cost$sla_failure_exposure - r3$summary$cost$sla_failure_exposure
    ratio       <- if (contr_cost > 0) round(exp_avoided / contr_cost, 1) else NA
    per_contr   <- if (n > 0) round(exp_avoided / n / 1000, 0) else NA
    div(
      class = "callout-green",
      style = "margin-top:8px;",
      icon("check-circle"), " ",
      if (!is.na(ratio) && is.finite(ratio))
        HTML(sprintf(
          "Each contractor costs ~$2K but avoids ~<b>$%dK</b> in SLA failure exposure on average. The investment returns <b>%.1fx</b> — up to the structural ceiling where late-reporting claims can't be saved.",
          max(0, per_contr), ratio))
      else
        "Add contractors to see the cost-benefit analysis."
    )
  })

  # ── MAP (Enhancement 5 — cluster polygons via st_union) ───────────────────
  output$map_clusters <- renderLeaflet({
    if (is.null(cluster_sf)) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(-82, 37, zoom = 5) %>%
          addControl("<b>Map unavailable.</b><br/>Install tigris package.", position = "topright")
      )
    }

    leaflet(cluster_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-82, 37, zoom = 5) %>%
      addPolygons(
        layerId      = ~cluster,
        fillColor    = "#e8eaf0",
        fillOpacity  = 0.65,
        color        = "#666",
        weight       = 2,
        label        = ~map_label,
        labelOptions = labelOptions(
          style     = list("font-weight" = "600", "font-size" = "13px",
                           "background" = "rgba(255,255,255,0.92)",
                           "padding" = "4px 8px", "border-radius" = "6px"),
          permanent = FALSE, sticky = TRUE
        ),
        highlightOptions = highlightOptions(
          weight = 3.5, color = "#002060",
          fillOpacity = 0.82, bringToFront = TRUE
        )
      )
  })

  # Recolor cluster map when run3 changes
  observeEvent(run3(), {
    req(!is.null(cluster_sf))

    sev5_df  <- cluster_sev5_pct(run3())
    cl_ids   <- as.character(cluster_sf$cluster)

    fills   <- sapply(cl_ids, function(cl) {
      row <- sev5_df[sev5_df$cluster == cl, ]
      map_fill(if (nrow(row) == 0) NA_real_ else row$pct[1])
    })
    strokes <- sapply(cl_ids, function(cl) {
      row <- sev5_df[sev5_df$cluster == cl, ]
      map_stroke(if (nrow(row) == 0) NA_real_ else row$pct[1])
    })

    labels_html <- sapply(cl_ids, function(cl) {
      row <- sev5_df[sev5_df$cluster == cl, ]
      pct <- if (nrow(row) == 0) NA_real_ else row$pct[1]
      n5  <- as.integer(sev5_per_cluster[cl])
      sprintf("C%s — %d Sev-5 · %.1f%%", cl, n5, if (is.na(pct)) 0 else pct)
    })

    leafletProxy("map_clusters", data = cluster_sf) %>%
      clearShapes() %>%
      addPolygons(
        layerId      = ~cluster,
        fillColor    = fills,
        fillOpacity  = 0.70,
        color        = strokes,
        weight       = 2.5,
        label        = labels_html,
        labelOptions = labelOptions(
          style     = list("font-weight" = "600", "font-size" = "13px",
                           "background" = "rgba(255,255,255,0.92)",
                           "padding" = "4px 8px", "border-radius" = "6px"),
          permanent = FALSE, sticky = TRUE
        ),
        highlightOptions = highlightOptions(
          weight = 4, color = "#002060",
          fillOpacity = 0.88, bringToFront = TRUE
        )
      )
  })

  # Cluster polygon click → update selected_cluster
  observeEvent(input$map_clusters_shape_click, {
    cid <- input$map_clusters_shape_click$id
    if (!is.null(cid)) selected_cluster(as.character(cid))
  })

  # ── Cluster detail panel ───────────────────────────────────────────────────
  output$cluster_detail_panel <- renderUI({
    cl  <- as.character(selected_cluster()[1])
    if (length(cl) == 0 || is.na(cl) || !cl %in% names(cluster_states)) cl <- "4"
    r3  <- run3()

    sev5_df      <- cluster_sev5_pct(r3)
    row5         <- sev5_df[sev5_df$cluster == cl, ]
    sev5_pct_val <- if (nrow(row5) == 0) 0 else row5$pct[1]

    tag_html   <- sla_tag_html(sev5_pct_val)
    border_col <- map_stroke(sev5_pct_val)

    sev_bars_r3 <- cluster_sla_pct(r3, cl)

    n_contr <- if (!is.null(r3$contractor_log) && nrow(r3$contractor_log) > 0)
      sum(as.character(r3$contractor_log$cluster) == cl, na.rm = TRUE) else 0L

    cs2      <- run2$cluster_summary[as.character(run2$cluster_summary$cluster) == cl, ]
    n_claims <- if (nrow(cs2) > 0) cs2$n_claims[1] else "—"
    n_sev5_t <- if (nrow(cs2) > 0) cs2$n_sev5[1]   else "—"

    n_deployed <- run2$assignments %>%
      filter(as.character(.data$cluster) == cl,
             .data$assignment_type == "deployed", !is.na(.data$assigned_adj)) %>%
      pull(assigned_adj) %>% n_distinct()

    current_counts <- c(
      "0"=input$sl_c0,"1"=input$sl_c1,"2"=input$sl_c2,"3"=input$sl_c3,
      "4"=input$sl_c4,"5"=input$sl_c5,"6"=input$sl_c6
    )
    test_counts     <- current_counts
    test_counts[cl] <- test_counts[cl] + 5L
    test_r3         <- simulate_contractors(run2, test_counts, cfg)
    test_sev5_df    <- cluster_sev5_pct(test_r3)
    test_row        <- test_sev5_df[test_sev5_df$cluster == cl, ]
    test_pct        <- if (nrow(test_row) == 0) sev5_pct_val else test_row$pct[1]
    gain            <- round(test_pct - sev5_pct_val, 1)

    div(
      class = "detail-panel",
      style = paste0("border-left: 5px solid ", border_col, ";"),

      div(
        style = "margin-bottom:14px;",
        tags$h5(style = "margin-bottom:4px;", cluster_labels[cl], " ", HTML(tag_html)),
        tags$p(
          style = "font-size:12px; color:#888; margin:0;",
          paste0(paste(cluster_states[[cl]], collapse = " · "),
                 " · ", sev5_per_cluster[cl], " Sev-5 claims")
        )
      ),

      tags$table(
        style = "width:100%; font-size:13px; margin-bottom:12px;",
        tags$tr(
          tags$td(style="color:#888;padding:3px 0;","Total claims"),
          tags$td(style="font-weight:600;text-align:right;", n_claims)
        ),
        tags$tr(
          tags$td(style="color:#888;padding:3px 0;","Sev-5 claims"),
          tags$td(style="font-weight:600;text-align:right;", n_sev5_t)
        ),
        tags$tr(
          tags$td(style="color:#888;padding:3px 0;","Deployed adjusters"),
          tags$td(style="font-weight:600;text-align:right;", n_deployed)
        ),
        tags$tr(
          tags$td(style="color:#888;padding:3px 0;","PL5 contractors (Run 3)"),
          tags$td(style="font-weight:600;text-align:right;", n_contr)
        )
      ),

      tags$p(style="font-size:12px;font-weight:600;color:#444;margin-bottom:6px;",
             "SLA Compliance by Severity (Run 3)"),

      lapply(sev_bars_r3, function(s) {
        bar_col <- if (s$pct>=90) "#1D9E75" else if (s$pct>=70) "#EF9F27" else "#E24B4A"
        div(
          style = "margin-bottom:8px;",
          div(
            style = "display:flex; justify-content:space-between;",
            tags$span(style="font-size:12px;", paste0("Sev-",s$sev)),
            tags$span(style=paste0("font-size:12px;font-weight:600;color:",bar_col,";"), fmt_pct(s$pct))
          ),
          div(class="gap-bar-wrap",
              div(class="gap-bar-fill",
                  style=sprintf("width:%s%%;background:%s;",min(s$pct,100),bar_col)))
        )
      }),

      if (gain > 0.1) {
        div(class="callout-amber", icon("lightbulb"), " ",
            sprintf("Adding 5 more PL5 contractors to this cluster would push Sev-5 from %s%% → %s%%. Adjust the slider.",
                    round(sev5_pct_val,1), round(test_pct,1)))
      } else {
        div(class="callout-green", style="background:#E1F5EE;border-color:#1D9E75;",
            icon("check-circle"), " ",
            "This cluster is well covered at current contractor levels.")
      }
    )
  })

  # ── Assignment table (Enhancement 8 — conditional row formatting) ──────────
  output$tbl_row_count <- renderText({
    df <- switch(input$flt_run,
      "run1" = run1$assignments,
      "run2" = run2$assignments,
      "run3" = run3()$assignments
    )
    paste0("Rows loaded: ", nrow(df))
  })

  output$tbl_assignments <- renderDT({
    df <- switch(input$flt_run,
      "run1" = run1$assignments,
      "run2" = run2$assignments,
      "run3" = run3()$assignments
    )

    # NA-safe base-R filters
    if (!is.null(input$flt_cluster) && input$flt_cluster != "All")
      df <- df[!is.na(df$cluster) & as.character(df$cluster) == input$flt_cluster, ]
    if (!is.null(input$flt_sev) && input$flt_sev != "All")
      df <- df[!is.na(df$severity) & df$severity == as.integer(input$flt_sev), ]
    if (!is.null(input$flt_sla) && input$flt_sla == "Met")
      df <- df[!is.na(df$sla_met) & df$sla_met == TRUE, ]
    if (!is.null(input$flt_sla) && input$flt_sla == "Missed")
      df <- df[!is.na(df$sla_met) & df$sla_met == FALSE, ]
    if (!is.null(input$flt_type) && input$flt_type != "All")
      df <- df[!is.na(df$assignment_type) & df$assignment_type == input$flt_type, ]

    # Build display frame with pure base R — force all columns to safe scalar types
    sla_col <- ifelse(
      !is.na(df$sla_met) & df$sla_met == TRUE,  "Met",
      ifelse(!is.na(df$sla_met) & df$sla_met == FALSE, "Missed", "N/A")
    )

    df_disp <- data.frame(
      Claim     = as.character(df$claim_id),
      Sev       = as.integer(df$severity),
      Cluster   = as.character(df$cluster),
      Handle    = as.character(df$handle_type),
      AdjID     = as.character(df$assigned_adj),
      Skill     = as.integer(df$adj_skill),
      Type      = as.character(df$assignment_type),
      Assigned  = as.integer(df$day_assigned),
      Completed = as.integer(df$day_completed),
      Deadline  = as.integer(df$sla_deadline),
      SLA       = sla_col,
      stringsAsFactors = FALSE
    )
    # Replace any Inf/-Inf/NaN that break JSON serialization
    df_disp[sapply(df_disp, is.numeric)] <- lapply(
      df_disp[sapply(df_disp, is.numeric)],
      function(x) { x[!is.finite(x)] <- NA_integer_; x }
    )

    datatable(
      df_disp,
      rownames  = FALSE,
      selection = "single",
      options   = list(
        pageLength = 25,
        scrollX    = TRUE,
        dom        = "ltipr",
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        createdRow = JS("
          function(row, data, index) {
            if (parseInt(data[1]) === 5 && data[10] === 'Missed') {
              $(row).css('background-color', '#FCEBEB');
            }
            if (data[6] === 'contractor') {
              $(row).css('border-left', '4px solid #378ADD');
            }
          }
        ")
      )
    ) %>%
      formatStyle("SLA",
        color      = styleEqual(c("Met", "Missed"), c("#0F6E56", "#A32D2D")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Sev",
        fontWeight = styleEqual(c(1,2,3,4,5), c("normal","normal","normal","normal","bold")),
        color      = styleEqual(c(1,2,3,4,5), c("#333","#333","#333","#333","#A32D2D"))
      ) %>%
      formatStyle("Type",
        color = styleEqual(
          c("local", "deployed", "contractor"),
          c("#555",  "#EF9F27",  "#378ADD")
        )
      )
  }, server = TRUE)

  # Track selected claim row (Enhancement 6)
  observeEvent(input$tbl_assignments_rows_selected, {
    sel <- input$tbl_assignments_rows_selected
    if (length(sel) == 0) { selected_claim_id(NULL); return() }

    df <- switch(input$flt_run,
      "run1" = run1$assignments,
      "run2" = run2$assignments,
      "run3" = run3()$assignments
    )
    if (input$flt_cluster != "All")
      df <- df[as.character(df$cluster) == input$flt_cluster, ]
    if (input$flt_sev != "All")
      df <- df[df$severity == as.integer(input$flt_sev), ]
    if (input$flt_sla == "Met")
      df <- df[df$sla_met == TRUE, ]
    if (input$flt_sla == "Missed")
      df <- df[df$sla_met == FALSE, ]
    if (input$flt_type != "All")
      df <- df[!is.na(df$assignment_type) & df$assignment_type == input$flt_type, ]

    if (nrow(df) >= sel) selected_claim_id(df$claim_id[sel])
  })

  # ── Claim detail card (Enhancement 6) ─────────────────────────────────────
  output$claim_detail_card <- renderUI({
    cid <- selected_claim_id()
    if (is.null(cid)) return(NULL)

    r3     <- run3()
    r3_row <- r3$assignments[r3$assignments$claim_id == cid, ]
    r2_row <- run2$assignments[run2$assignments$claim_id == cid, ]
    if (nrow(r3_row) == 0 && nrow(r2_row) == 0) return(NULL)

    ref_row  <- if (nrow(r3_row) > 0) r3_row[1, ] else r2_row[1, ]
    sev_val  <- ref_row$severity
    sla_met  <- isTRUE(ref_row$sla_met)

    # Severity tag color
    sev_col <- if (sev_val == 5) "#E24B4A" else if (sev_val == 4) "#EF9F27" else "#1D9E75"
    sev_tag <- sprintf('<span style="background:%s;color:#fff;padding:2px 8px;
                        border-radius:10px;font-size:11px;font-weight:700;">Sev-%d</span>',
                       sev_col, sev_val)
    sla_tag <- if (sla_met)
      '<span style="background:#E1F5EE;color:#085041;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:700;">✓ SLA Met</span>'
    else
      '<span style="background:#FCEBEB;color:#791F1F;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:700;">✗ SLA Missed</span>'

    # Field helper — show value or dash
    fld <- function(df, col) {
      if (!col %in% colnames(df) || is.na(df[[col]][1])) return("—")
      as.character(df[[col]][1])
    }

    # Build "Why this assignment" narrative
    r2_met  <- nrow(r2_row) > 0 && isTRUE(r2_row$sla_met[1])
    r3_met  <- nrow(r3_row) > 0 && isTRUE(r3_row$sla_met[1])

    why_html <- if (r2_met && r3_met) {
      "This claim was handled by staff in both Run 2 and Run 3. No contractor intervention was needed."
    } else if (!r2_met && r3_met && nrow(r3_row) > 0 && r3_row$assignment_type[1] == "contractor") {
      sprintf(
        "In Run 2 (no contractors), this Sev-%d claim %s. Run 3 assigned a PL5 contractor (ID %s) starting Day %s and completing Day %s — within the Day %s deadline. Contractor cost: ~%s. Without this contractor: ~$%sK SLA exposure avoided.",
        sev_val,
        if (nrow(r2_row) > 0 && !is.na(r2_row$day_completed[1]))
          sprintf("completed Day %s but missed deadline Day %s", r2_row$day_completed[1], r2_row$sla_deadline[1])
        else "was not assigned in time",
        r3_row$assigned_adj[1],
        r3_row$day_assigned[1],
        r3_row$day_completed[1],
        r3_row$sla_deadline[1],
        fmt_cost((as.integer(r3_row$day_completed[1]) - as.integer(r3_row$day_assigned[1]) + 1L) *
                   cfg$cost_contractor_incremental),
        round(cfg$sla_failure_cost[as.character(sev_val)] / 1000, 0)
      )
    } else if (!r3_met) {
      sprintf(
        "This claim arrives Day %s with SLA deadline Day %s. Only %s days available — even PL5 (4 days needed) cannot complete in time. This is a structural miss caused by late reporting.",
        fld(ref_row, "day_arrived"),
        ref_row$sla_deadline[1],
        max(0, as.integer(ref_row$sla_deadline[1]) - as.integer(fld(ref_row, "day_arrived")))
      )
    } else {
      "Assignment details for this claim."
    }

    # Other eligible adjusters (same cluster, sufficient skill, nearby timing)
    cl_str  <- as.character(ref_row$cluster)
    min_sk  <- cfg$min_skill[as.character(sev_val)]
    arr_day <- if ("day_arrived" %in% colnames(ref_row)) ref_row$day_arrived[1] else ref_row$day_assigned[1]

    alt_adjs <- tryCatch({
      run2$assignments %>%
        filter(as.character(cluster) == cl_str,
               !is.na(adj_skill), adj_skill >= min_sk,
               claim_id != cid,
               !is.na(day_assigned),
               abs(as.integer(day_assigned) - as.integer(arr_day)) <= 5) %>%
        arrange(day_assigned) %>%
        head(4) %>%
        select(adj_id = assigned_adj, skill = adj_skill, type = assignment_type,
               day_assigned, day_completed)
    }, error = function(e) NULL)

    div(
      class = "claim-detail-card",

      # Header
      div(style = "margin-bottom:14px;",
          tags$h5(
            style = "margin-bottom:6px;",
            paste0("Claim ", cid, "  "), HTML(sev_tag), "  ", HTML(sla_tag)
          )
      ),

      fluidRow(
        # Left: Claim details
        column(6,
          div(class = "detail-section",
              tags$b(style="color:#002060;font-size:12px;", "Claim Details"),
              tags$hr(style="margin:4px 0;"),
              div(class="detail-row",
                  span(class="detail-label","Cluster"),
                  span(class="detail-value",
                       paste0("C", cl_str, " — ", paste(cluster_states[[cl_str]], collapse=" / ")))),
              div(class="detail-row",
                  span(class="detail-label","Handle type"),
                  span(class="detail-value", fld(ref_row, "handle_type"))),
              div(class="detail-row",
                  span(class="detail-label","City / State"),
                  span(class="detail-value", paste(fld(ref_row,"city"), fld(ref_row,"state"), sep=" "))),
              div(class="detail-row",
                  span(class="detail-label","Peril group"),
                  span(class="detail-value", fld(ref_row,"peril_group"))),
              div(class="detail-row",
                  span(class="detail-label","Distance to centroid"),
                  span(class="detail-value",
                       if ("distance_km" %in% colnames(ref_row) && !is.na(ref_row$distance_km))
                         paste0(round(ref_row$distance_km, 0), " km") else "—")),
              div(class="detail-row",
                  span(class="detail-label","Severity source"),
                  span(class="detail-value", fld(ref_row,"severity_source")))
          )
        ),
        # Right: Timeline
        column(6,
          div(class = "detail-section",
              tags$b(style="color:#002060;font-size:12px;", "Timeline"),
              tags$hr(style="margin:4px 0;"),
              div(class="detail-row",
                  span(class="detail-label","Day arrived (NOL)"),
                  span(class="detail-value",
                       if ("day_arrived" %in% colnames(ref_row))
                         paste0("Day ", ref_row$day_arrived[1]) else "—")),
              div(class="detail-row",
                  span(class="detail-label","SLA deadline"),
                  span(class="detail-value", paste0("Day ", ref_row$sla_deadline[1]))),
              div(class="detail-row",
                  span(class="detail-label","Day assigned"),
                  span(class="detail-value",
                       if (!is.na(ref_row$day_assigned[1]))
                         paste0("Day ", ref_row$day_assigned[1]) else "—")),
              div(class="detail-row",
                  span(class="detail-label","Day completed"),
                  span(class="detail-value",
                       if (!is.na(ref_row$day_completed[1]))
                         paste0("Day ", ref_row$day_completed[1]) else "—")),
              div(class="detail-row",
                  span(class="detail-label","Days to resolve"),
                  span(class="detail-value",
                       if (!is.na(ref_row$day_completed[1]) && !is.na(ref_row$day_assigned[1]))
                         as.integer(ref_row$day_completed[1]) - as.integer(ref_row$day_assigned[1]) + 1L
                       else "—"))
          )
        )
      ),

      # Assigned adjuster box
      if (!is.na(ref_row$assigned_adj[1])) {
        n_days <- if (!is.na(ref_row$day_completed[1]) && !is.na(ref_row$day_assigned[1]))
          as.integer(ref_row$day_completed[1]) - as.integer(ref_row$day_assigned[1]) + 1L else NA
        cost   <- if (!is.na(n_days))
          n_days * switch(ref_row$assignment_type[1],
                          local=cfg$cost_local_incremental,
                          deployed=cfg$cost_deployed_incremental,
                          contractor=cfg$cost_contractor_incremental,
                          0) else NA
        div(class = "adj-box",
            tags$b(style="font-size:12px;color:#1A4F7A;", "Assigned Adjuster"),
            tags$br(),
            sprintf("ID %s · PL%s · %s assignment",
                    ref_row$assigned_adj[1], ref_row$adj_skill[1], ref_row$assignment_type[1]),
            tags$br(),
            if (!is.na(n_days))
              sprintf("%d days worked · Est. incremental cost: %s", n_days, fmt_cost(cost))
        )
      },

      # Why this assignment
      div(class = "why-box",
          tags$b(style="font-size:12px;", "Why This Assignment"), tags$br(),
          why_html
      ),

      # Other eligible adjusters
      if (!is.null(alt_adjs) && nrow(alt_adjs) > 0) {
        div(
          style = "margin-top:8px;",
          tags$p(style="font-size:12px;font-weight:600;color:#444;margin-bottom:6px;",
                 "Other Eligible Adjusters (same cluster, within ±5 days)"),
          tags$table(
            style = "width:100%;font-size:12px;border-collapse:collapse;",
            tags$tr(
              lapply(c("Adj ID","Skill","Type","Assigned","Completed"),
                     function(h) tags$th(style="background:#f4f6fa;padding:4px 8px;", h))
            ),
            lapply(seq_len(nrow(alt_adjs)), function(i) {
              r <- alt_adjs[i, ]
              tags$tr(
                tags$td(style="padding:4px 8px;border-bottom:1px solid #eee;", r$adj_id),
                tags$td(style="padding:4px 8px;border-bottom:1px solid #eee;", paste0("PL", r$skill)),
                tags$td(style="padding:4px 8px;border-bottom:1px solid #eee;", r$type),
                tags$td(style="padding:4px 8px;border-bottom:1px solid #eee;",
                        if (!is.na(r$day_assigned)) paste0("Day ", r$day_assigned) else "—"),
                tags$td(style="padding:4px 8px;border-bottom:1px solid #eee;",
                        if (!is.na(r$day_completed))
                          paste0("Day ", r$day_completed, " — Busy")
                        else "—")
              )
            })
          )
        )
      }
    )
  })

  # ── Export button (Enhancement 10) ────────────────────────────────────────
  output$download_plan <- downloadHandler(
    filename = function() {
      paste0("CATOPT_Deployment_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      r3 <- run3()
      sev5_df <- cluster_sev5_pct(r3)

      rows <- lapply(0:6, function(cl) {
        cl_str  <- as.character(cl)
        contr_n <- c(input$sl_c0, input$sl_c1, input$sl_c2, input$sl_c3,
                     input$sl_c4, input$sl_c5, input$sl_c6)[cl + 1]
        row     <- sev5_df[sev5_df$cluster == cl_str, ]
        pct     <- if (nrow(row) == 0) 0 else row$pct[1]
        c_cost  <- contr_n * 4L * cfg$cost_contractor_incremental
        cl_asgn <- r3$assignments %>% filter(as.character(cluster) == cl_str, severity == 5L)
        exp     <- sum(cl_asgn$sla_met == FALSE, na.rm=TRUE) * cfg$sla_failure_cost[["5"]]
        data.frame(
          Cluster      = paste0("C", cl),
          States       = paste(cluster_states[[cl_str]], collapse=", "),
          Sev5_Claims  = sev5_per_cluster[cl_str],
          PL5_Contrs   = contr_n,
          Sev5_SLA_Pct = paste0(round(pct, 1), "%"),
          Contr_Cost   = fmt_cost(c_cost),
          SLA_Exposure = fmt_cost(exp),
          stringsAsFactors = FALSE
        )
      })
      plan_df <- do.call(rbind, rows)

      total_row <- data.frame(
        Cluster = "TOTAL", States = "",
        Sev5_Claims  = sum(sev5_per_cluster),
        PL5_Contrs   = total_contractors(),
        Sev5_SLA_Pct = paste0(round(r3$summary$by_severity$sev5$pct, 1), "%"),
        Contr_Cost   = fmt_cost(r3$summary$cost$contractor_cost),
        SLA_Exposure = fmt_cost(r3$summary$cost$sla_failure_exposure),
        stringsAsFactors = FALSE
      )
      plan_df <- rbind(plan_df, total_row)

      con <- file(file, "w")
      on.exit(close(con))
      writeLines(c(
        "CATOPT Deployment Plan",
        paste0("Generated: ", format(Sys.Date(), "%B %d, %Y")),
        "CAT Event: Code 82 - December 2023",
        sprintf("Run 2 Baseline: %.1f%% overall, %.1f%% Sev-5",
                run2$summary$overall_compliance,
                run2$summary$by_severity$sev5$pct),
        sprintf("Run 3 (This Plan): %.1f%% overall, %.1f%% Sev-5",
                r3$summary$overall_compliance,
                r3$summary$by_severity$sev5$pct),
        sprintf("Total PL5 Contractors: %d", total_contractors()),
        sprintf("Incremental Cost: %s", fmt_cost(r3$summary$cost$incremental_operating)),
        sprintf("Est. SLA Exposure Avoided: %s",
                fmt_cost(run2$summary$cost$sla_failure_exposure -
                           r3$summary$cost$sla_failure_exposure)),
        "---", ""
      ), con)
      write.table(plan_df, con, sep=",", row.names=FALSE, quote=TRUE)
    }
  )

  # ── Daily Drill-Down (Summary cards) ────────────────────────────────────────
  output$drill_summary <- renderUI({
    D <- as.integer(input$drill_day)
    asgn <- switch(input$drill_run,
      "run1" = run1$assignments,
      "run2" = run2$assignments,
      "run3" = run3()$assignments
    )

    # --- Arrivals today ---
    arrived    <- asgn[!is.na(asgn$day_arrived) & asgn$day_arrived == D, ]
    n_arrived  <- nrow(arrived)
    n_sev5_arr <- sum(arrived$severity == 5L, na.rm = TRUE)
    n_onsite   <- sum(!is.na(arrived$handle_type) & arrived$handle_type == "on-site")
    n_virtual  <- sum(!is.na(arrived$handle_type) & arrived$handle_type == "virtual")

    # Severity breakdown of arrivals
    sev_parts <- vapply(5:1, function(s) {
      n <- sum(arrived$severity == s, na.rm = TRUE)
      if (n > 0) paste0("Sev-", s, ": ", n) else ""
    }, character(1))
    sev_str <- paste(sev_parts[nchar(sev_parts) > 0], collapse = "  |  ")

    # --- Assigned today (from any arrival day) ---
    assigned   <- asgn[!is.na(asgn$day_assigned) & asgn$day_assigned == D, ]
    n_assigned <- nrow(assigned)
    n_local    <- sum(!is.na(assigned$assignment_type) & assigned$assignment_type == "local")
    n_deployed <- sum(!is.na(assigned$assignment_type) & assigned$assignment_type == "deployed")
    n_contr    <- sum(!is.na(assigned$assignment_type) & assigned$assignment_type == "contractor")

    # --- Completed today ---
    n_done <- sum(!is.na(asgn$day_completed) & asgn$day_completed == D)

    # --- In progress end-of-day D ---
    in_prog  <- asgn[!is.na(asgn$day_assigned) & asgn$day_assigned <= D &
                     (is.na(asgn$day_completed) | asgn$day_completed > D), ]
    n_inprog <- nrow(in_prog)

    # Skill breakdown of in-progress workforce
    skill_counts <- table(paste0("PL", in_prog$adj_skill[!is.na(in_prog$adj_skill)]))
    skill_str <- if (length(skill_counts) > 0)
      paste(paste0(names(skill_counts), "=", as.integer(skill_counts)), collapse = "  |  ")
    else "No active assignments"

    # --- Waiting end-of-day D ---
    n_waiting <- sum(
      !is.na(asgn$day_arrived) & asgn$day_arrived <= D &
      (is.na(asgn$day_assigned) | asgn$day_assigned > D)
    )

    # --- Cumulative SLA misses through day D ---
    n_sla_miss <- sum(
      !is.na(asgn$sla_met) & asgn$sla_met == FALSE &
      !is.na(asgn$sla_deadline) & asgn$sla_deadline <= D
    )

    tagList(
      # Six stat cards
      fluidRow(
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val", style = "color:#002060;", n_arrived),
          div(class = "drill-stat-lbl", "Arrived Today"),
          div(class = "drill-stat-sub",
              style = if (n_sev5_arr > 0) "color:#A32D2D;" else "color:#aaa;",
              if (n_sev5_arr > 0) paste0(n_sev5_arr, " Sev-5") else "no Sev-5")
        )),
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val", style = "color:#1D9E75;", n_assigned),
          div(class = "drill-stat-lbl", "Assigned Today"),
          div(class = "drill-stat-sub", style = "color:#888;",
              paste0(n_local, "L / ", n_deployed, "D",
                     if (n_contr > 0) paste0(" / ", n_contr, "C") else ""))
        )),
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val", style = "color:#1D9E75;", n_done),
          div(class = "drill-stat-lbl", "Completed Today"),
          div(class = "drill-stat-sub", style = "color:#aaa;", "closed out")
        )),
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val", style = "color:#EF9F27;", n_inprog),
          div(class = "drill-stat-lbl", "In Progress"),
          div(class = "drill-stat-sub", style = "color:#aaa;", "end of day")
        )),
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val",
              style = if (n_waiting > 0) "color:#E24B4A;" else "color:#888;",
              n_waiting),
          div(class = "drill-stat-lbl", "Waiting (Backlog)"),
          div(class = "drill-stat-sub", style = "color:#aaa;", "unassigned")
        )),
        column(2, div(class = "drill-stat",
          div(class = "drill-stat-val",
              style = if (n_sla_miss > 0) "color:#A32D2D;" else "color:#888;",
              n_sla_miss),
          div(class = "drill-stat-lbl", "SLA Misses (Cumul.)"),
          div(class = "drill-stat-sub", style = "color:#aaa;", "through today")
        ))
      ),
      # Detail callouts
      fluidRow(
        column(6,
          div(class = "callout-navy", style = "margin-top:6px; font-size:12px;",
            tags$b("Arrivals breakdown — "),
            if (nchar(sev_str) > 0) sev_str else "none",
            tags$br(),
            paste0(n_onsite, " on-site  |  ", n_virtual, " virtual")
          )
        ),
        column(6,
          div(class = "callout-navy", style = "margin-top:6px; font-size:12px;",
            tags$b("Active workforce (end-of-day) — "),
            skill_str
          )
        )
      )
    )
  })

  # ── Daily Drill-Down (Claims table) ─────────────────────────────────────────
  output$drill_tbl <- renderDT({
    D    <- as.integer(input$drill_day)
    asgn <- switch(input$drill_run,
      "run1" = run1$assignments,
      "run2" = run2$assignments,
      "run3" = run3()$assignments
    )

    # All claims that arrived on day D
    df <- asgn[!is.na(asgn$day_arrived) & asgn$day_arrived == D, ]
    df <- df[order(df$severity), ]   # Sev-5 first

    # Day-D snapshot status for each claim (not the final status column)
    day_status <- ifelse(
      !is.na(df$day_completed) & df$day_completed <= D, "Done",
      ifelse(!is.na(df$day_assigned) & df$day_assigned <= D, "In Progress", "Waiting")
    )

    df_disp <- data.frame(
      Claim     = as.character(df$claim_id),
      Sev       = as.integer(df$severity),
      Cluster   = as.character(df$cluster),
      Handle    = as.character(df$handle_type),
      Deadline  = as.integer(df$sla_deadline),
      Status    = day_status,
      Assigned  = as.integer(df$day_assigned),
      AdjSkill  = as.integer(df$adj_skill),
      AdjType   = as.character(df$assignment_type),
      Completed = as.integer(df$day_completed),
      SLAFinal  = ifelse(!is.na(df$sla_met) & df$sla_met == TRUE,  "Met",
                  ifelse(!is.na(df$sla_met) & df$sla_met == FALSE, "Missed", "TBD")),
      stringsAsFactors = FALSE
    )
    # Scrub Inf/-Inf from numeric columns
    df_disp[sapply(df_disp, is.numeric)] <- lapply(
      df_disp[sapply(df_disp, is.numeric)],
      function(x) { x[!is.finite(x)] <- NA; x }
    )

    datatable(
      df_disp,
      rownames  = FALSE,
      selection = "none",
      caption   = htmltools::tags$caption(
        style = "caption-side:top; font-size:13px; font-weight:600; color:#002060; padding:6px 0;",
        paste0("Claims that arrived on Day ", D,
               " — ", nrow(df_disp), " claims",
               "  ·  Table shows end-of-Day-", D, " state")
      ),
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "tip",
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        createdRow = JS("
          function(row, data, idx) {
            if (data[10] === 'Missed') { $(row).css('background-color','#FCEBEB'); }
            else if (data[5] === 'Waiting') { $(row).css('background-color','#FFF8F0'); }
            else if (data[5] === 'Done')    { $(row).css('background-color','#F2FCF8'); }
          }
        ")
      )
    ) %>%
      formatStyle("Sev",
        color      = styleEqual(c(1,2,3,4,5), c("#555","#555","#555","#EF9F27","#A32D2D")),
        fontWeight = styleEqual(c(1,2,3,4,5), c("normal","normal","normal","normal","bold"))
      ) %>%
      formatStyle("Status",
        backgroundColor = styleEqual(
          c("Done",     "In Progress", "Waiting"),
          c("#E1F5EE",  "#EEF2FA",     "#FFF8F0")
        ),
        color = styleEqual(
          c("Done",     "In Progress", "Waiting"),
          c("#0F6E56",  "#002060",     "#854F0B")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle("SLAFinal",
        color      = styleEqual(c("Met","Missed","TBD"), c("#0F6E56","#A32D2D","#888")),
        fontWeight = styleEqual(c("Met","Missed","TBD"), c("bold","bold","normal"))
      ) %>%
      formatStyle("AdjType",
        color = styleEqual(
          c("local","deployed","contractor"),
          c("#555",  "#EF9F27", "#378ADD")
        )
      )
  }, server = TRUE)

}

shinyApp(ui, server)
