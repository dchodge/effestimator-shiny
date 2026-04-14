library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinyjs)

# ── Theme ──────────────────────────────────────────────────────────────────────
ft_theme <- bs_theme(
  version   = 5,
  bg        = "#FFFFFF",
  fg        = "#33302E",
  primary   = "#0D7680",
  secondary = "#9E9B93",
  danger    = "#990F3D",
  warning   = "#FF8833",
  "sidebar-bg"           = "#F2EDE8",
  "sidebar-border-color" = "#E9E4D9",
  "card-border-color"    = "#E9E4D9",
  "navbar-bg"            = "#33302E",
  "navbar-brand-color"   = "#FF8833",
  "input-border-color"       = "#D0C9C0",
  "input-focus-border-color" = "#0D7680"
)

# ── Dataset choices ────────────────────────────────────────────────────────────
DATASETS <- c(
  "Nirsevimab (MELODY / MEDLEY)"            = "nirsevimab",
  "Maternal vaccine — Pfizer MATISSE"       = "maternal",
  "Older adult — GSK Arexvy" = "oa_gsk",
  "Older adult — Pfizer Abrysvo"            = "oa_pfizer"
)

# ── Per-dataset paper references ─────────────────────────────────────────────
DATASET_REFS <- list(
  nirsevimab = list(
    cite = "Hammitt et al. (2022) \u2013 Lancet",
    url  = "https://www.sciencedirect.com/science/article/pii/S2352464222003212"
  ),
  maternal = list(
    cite = "Kampmann et al. (2023) \u2013 NEJM",
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2216480"
  ),
  oa_gsk = list(
    cite = "Papi et al. (2023) \u2013 NEJM (2-season follow-up)",
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2209604"
  ),
  oa_pfizer = list(
    cite = "Walsh et al. (2023) \u2013 NEJM",
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2213836"
  )
)

# ── Compartment diagram HTML helper ──────────────────────────────────────────
compartment_diagram <- function(n, col, rate_label) {
  cols    <- rep(col, n)
  s_col   <- "#D5CFC8"
  box_w   <- 54; box_h <- 36; gap <- 36
  total_w <- n * box_w + (n - 1) * gap + gap + box_w + 20
  total_h <- box_h + 28

  svg_parts <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="100%%" height="%d">',
            total_w, total_h, total_h),
    '<defs><marker id="arr" markerWidth="7" markerHeight="7" refX="6" refY="3.5" orient="auto">',
    '<path d="M0,0 L7,3.5 L0,7 Z" fill="#9E9B93"/>',
    '</marker></defs>'
  )

  x <- 6
  for (i in seq_len(n)) {
    svg_parts <- c(svg_parts,
      sprintf('<rect x="%d" y="6" width="%d" height="%d" rx="4" fill="%s"/>',
              x, box_w, box_h, cols[i]),
      sprintf('<text x="%d" y="29" text-anchor="middle" fill="white" font-size="13" font-weight="700">V\u2080%d</text>',
              x + box_w / 2, i)
    )
    if (i < n) {
      x2 <- x + box_w
      svg_parts <- c(svg_parts,
        sprintf('<line x1="%d" y1="24" x2="%d" y2="24" stroke="#9E9B93" stroke-width="1.5" marker-end="url(#arr)"/>',
                x2, x2 + gap - 6),
        sprintf('<text x="%d" y="18" text-anchor="middle" fill="#9E9B93" font-size="9">%s</text>',
                x2 + gap / 2, rate_label)
      )
      x <- x + box_w + gap
    }
  }
  # final arrow to S
  x2 <- x + box_w
  svg_parts <- c(svg_parts,
    sprintf('<line x1="%d" y1="24" x2="%d" y2="24" stroke="#9E9B93" stroke-width="1.5" marker-end="url(#arr)"/>',
            x2, x2 + gap - 6),
    sprintf('<text x="%d" y="18" text-anchor="middle" fill="#9E9B93" font-size="9">%s</text>',
            x2 + gap / 2, rate_label),
    sprintf('<rect x="%d" y="6" width="%d" height="%d" rx="4" fill="%s" stroke="#9E9B93" stroke-width="1.2" stroke-dasharray="3 2"/>',
            x2 + gap, box_w, box_h, s_col),
    sprintf('<text x="%d" y="29" text-anchor="middle" fill="#6E6259" font-size="13" font-weight="700">S</text>',
            x2 + gap + box_w / 2),
    '</svg>'
  )
  HTML(paste(svg_parts, collapse = "\n"))
}

# ── Helpers ────────────────────────────────────────────────────────────────────
empty_plot <- function(msg = "Click \u2018Fit Models\u2019 to run the analysis") {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = msg,
      size = 5, colour = "#9E9B93", hjust = 0.5, vjust = 0.5
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#FAFAF8", colour = NA))
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Efficacy Estimator",
  theme    = ft_theme,
  fillable = TRUE,
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),
  # Navbar links (absolutely positioned into navbar via CSS)
  tags$div(
    class = "brand-links-bar",
    tags$a("R package", href = "https://github.com/dchodge/effestimator",
           target = "_blank", rel = "noopener"),
    " \u00b7 ",
    tags$a("Hodgson et al. 2024, Lancet Europe",
           href = "https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00248-X/fulltext",
           target = "_blank", rel = "noopener")
  ),

  # ── Intro strip (below navbar, above sidebar layout) ──────────────────────
  tags$div(
    class = "app-intro-bar",
    tags$span(class = "app-intro-title", "Waning Efficacy Estimator"),
    tags$span(class = "app-intro-sep", "\u2014"),
    "Translate randomised trial results into waning protection curves ",
    "ready for use in compartment epidemic models. ",
    "Fit exponential or Erlang-k waning models via Bayesian HMC (Stan) ",
    "to interval-format event data.",
    tags$span(
      class = "app-intro-ref",
      " Methods: ",
      tags$a(
        "Hodgson et al. 2024, Lancet Regional Health \u2014 Europe",
        href = "https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00248-X/fulltext",
        target = "_blank", rel = "noopener"
      ),
      "."
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 300,
    open  = "open",

    # ── Step 1: Data ──────────────────────────────────────────────────────────
    tags$p(tags$span("1"), " Data", class = "step-heading"),

    radioButtons(
      "data_source", NULL,
      choices  = c("Built-in RSV dataset" = "builtin",
                   "Upload CSV files"     = "upload"),
      selected = "builtin"
    ),

    conditionalPanel(
      "input.data_source == 'builtin'",
      selectInput("builtin_dataset", NULL, choices = DATASETS, selected = "maternal"),
      uiOutput("paper_ref")
    ),

    conditionalPanel(
      "input.data_source == 'upload'",
      tags$p("Vaccine / treatment arm", class = "upload-label"),
      fileInput("vac_csv", NULL, accept = ".csv",
                placeholder = "vaccine.csv", buttonLabel = icon("folder-open")),
      tags$p("Placebo / control arm", class = "upload-label"),
      fileInput("pla_csv", NULL, accept = ".csv",
                placeholder = "placebo.csv", buttonLabel = icon("folder-open")),
      tags$div(
        class = "csv-hint",
        "Required columns: ",
        tags$code("t"), ", ", tags$code("person_time"), ", ", tags$code("n")
      )
    ),

    tags$hr(class = "sidebar-rule"),

    # ── Step 2: Models ────────────────────────────────────────────────────────
    tags$p(tags$span("2"), " Waning models", class = "step-heading"),

    checkboxGroupInput(
      "models", NULL,
      choiceNames  = list(
        span("Exponential",  class = "model-pill exp"),
        span("Erlang-2",     class = "model-pill er2"),
        span("Erlang-3",     class = "model-pill er3")
      ),
      choiceValues = c("exp", "er2", "er3"),
      selected     = c("exp", "er2", "er3")
    ),

    tags$div(
      class = "model-diagram-hint",
      tags$span(icon("circle-info"), " See "),
      tags$a("About", href = "#",
             onclick = "Shiny.setInputValue('goto_about', Math.random())"),
      " tab for compartment structure"
    ),

    tags$hr(class = "sidebar-rule"),

    # ── Step 3: MCMC ──────────────────────────────────────────────────────────
    accordion(
      id   = "mcmc_acc",
      open = NULL,
      accordion_panel(
        title = tags$span(
          tags$span("3"), " MCMC settings",
          class = "step-heading"
        ),
        value = "mcmc_panel",
        checkboxInput("bounded", HTML("Bound wane&#8239;rate <em>(wane_b)</em>"),
                      value = FALSE),
        conditionalPanel(
          "input.bounded",
          numericInput("upper_bound_b", "Upper bound for wane_b",
                       value = 0.05, min = 0.001, max = 1, step = 0.01)
        ),
        numericInput("chains",  "Chains (forced to 1 on server)",  value = 1,    min = 1, max = 8),
        numericInput("iter_w",  "Warmup / chain",  value = 500,  min = 100, step = 100),
        numericInput("iter_s",  "Samples / chain", value = 500,  min = 100, step = 100),
        numericInput("seed",    "Random seed",     value = 123,  min = 1)
      )
    ),

    tags$hr(class = "sidebar-rule"),

    # ── Fit button + status ───────────────────────────────────────────────────
    actionButton(
      "fit_btn", HTML('<i class="fa fa-play me-2"></i>Fit Models'),
      class = "btn-primary w-100 fit-btn"
    ),

    uiOutput("fit_status")
  ),

  # ── Main tabset ─────────────────────────────────────────────────────────────
  navset_tab(
    id = "main_tabs",

    # Tab 1: Data preview
    nav_panel(
      value = "tab_data",
      title = span(icon("table"), " Data"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          class = "h-100",
          card_header("Vaccine / treatment arm"),
          card_body(class = "p-0", DTOutput("tbl_vac", height = "420px"))
        ),
        card(
          class = "h-100",
          card_header("Placebo / control arm"),
          card_body(class = "p-0", DTOutput("tbl_pla", height = "420px"))
        )
      )
    ),

    # Tab 2: Waning efficacy
    nav_panel(
      value = "tab_waning",
      title = span(icon("chart-line"), " Waning Efficacy"),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Waning vaccine efficacy",
          tags$div(
            class = "d-flex gap-2 ms-auto",
            downloadButton("dl_curves_csv", "Curves CSV", icon = icon("file-csv"),
                           class = "btn-sm btn-outline-secondary dl-btn"),
            downloadButton("dl_waning", label = NULL, icon = icon("download"),
                           class = "btn-sm btn-outline-secondary dl-btn")
          )
        ),
        plotOutput("plot_waning", height = "480px")
      )
    ),

    # Tab 3: Incidence fit
    nav_panel(
      value = "tab_incidence",
      title = span(icon("virus"), " Incidence Fit"),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Fitted incidence vs. observed data",
          downloadButton("dl_incidence", label = NULL, icon = icon("download"),
                         class = "btn-sm btn-outline-secondary dl-btn ms-auto")
        ),
        plotOutput("plot_incidence", height = "480px")
      )
    ),

    # Tab 4: Diagnostics
    nav_panel(
      value = "tab_diag",
      title = span(icon("stethoscope"), " Diagnostics"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "MCMC convergence diagnostics",
          downloadButton("dl_posteriors_csv", "Posteriors CSV", icon = icon("file-csv"),
                         class = "btn-sm btn-outline-secondary dl-btn ms-auto")
        ),
        card_body(DTOutput("tbl_diag"))
      )
    ),

    # Tab 5: About / Methods
    nav_panel(
      value = "tab_about",
      title = span(icon("circle-info"), " About"),

      layout_columns(
        col_widths = c(12),

        card(
          card_header("Waning model compartment structures"),
          card_body(
            tags$p(
              class = "about-intro",
              "Each waning model defines how protection leaves the vaccinated state over time. ",
              "Structurally, Erlang-k waning is equivalent to passing through k sequential ",
              "ODE compartments before becoming susceptible \u2014 yielding a more delayed and ",
              "bell-shaped waning profile as k increases. The efficacy at time t is the ",
              "fraction still in any protected compartment:"
            ),
            tags$div(
              class = "model-how",
              HTML("<code>n<sub>vac</sub>[t] ~ Poisson( person_time[t] \u00b7 \u03bb(t) \u00b7 (1 &minus; eff(t)) )</code>"),
              HTML("<code>n<sub>pla</sub>[t] ~ Poisson( person_time[t] \u00b7 \u03bb(t) )</code>"),
              HTML("<code>\u03bb(t) ~ exponentiated Gaussian process (placebo arm)</code>")
            ),
            tags$hr(),

            # \u2500 Exponential
            tags$div(
              class = "model-block",
              tags$div(
                class = "model-block-header",
                tags$span(class = "model-pill exp me-2", "Exponential"),
                tags$span(class = "model-eq", HTML("eff(t) = a \u00b7 exp(\u2212\u03b2t)"))
              ),
              compartment_diagram(1, "#0D7680", "\u03b2"),
              tags$p(class = "model-desc",
                "One protected compartment. Immunity decays at a constant instantaneous rate \u03b2 ",
                "from day 0 (memoryless, constant hazard). The simplest assumption: protection ",
                "falls off smoothly and immediately from the start.")
            ),

            # \u2500 Erlang-2
            tags$div(
              class = "model-block",
              tags$div(
                class = "model-block-header",
                tags$span(class = "model-pill er2 me-2", "Erlang-2"),
                tags$span(class = "model-eq", HTML("eff(t) = a \u00b7 (1 \u2212 \u0393(t | 2, \u03b2))"))
              ),
              compartment_diagram(2, "#990F3D", "2\u03b2"),
              tags$p(class = "model-desc",
                "Two sequential protected compartments (V\u2081 \u2192 V\u2082 \u2192 S). ",
                "An individual must pass through both stages before losing protection. ",
                "This creates a hump-shaped waning curve: protection is better maintained ",
                "early on, then drops more sharply than exponential.")
            ),

            # \u2500 Erlang-3
            tags$div(
              class = "model-block",
              tags$div(
                class = "model-block-header",
                tags$span(class = "model-pill er3 me-2", "Erlang-3"),
                tags$span(class = "model-eq", HTML("eff(t) = a \u00b7 (1 \u2212 \u0393(t | 3, \u03b2))"))
              ),
              compartment_diagram(3, "#593380", "3\u03b2"),
              tags$p(class = "model-desc",
                "Three sequential protected compartments. Even more delayed waning: ",
                "protection is near-maximal for longer, then falls rapidly. Best suited ",
                "to products with a pronounced lag before immunity decline.")
            )
          )
        ),

        card(
          card_header("Model parameters: initial efficacy and waning rate"),
          card_body(
            tags$p(class = "about-intro",
              "Each waning model has two key structural parameters estimated by the Bayesian sampler. ",
              "Understanding these helps interpret the waning curves produced by the app."
            ),

            tags$h6("Parameter a \u2014 initial efficacy (responder fraction)", class = "about-section-h"),
            tags$p(class = "about-intro",
              tags$strong("a"), " is the fraction of vaccinees who are immunologically protected ",
              "immediately after vaccination / administration (at t\u00a0=\u00a00). Equivalently, ",
              tags$strong("1\u2009\u2212\u2009a"), " is the non-responder fraction \u2014 individuals who receive ",
              "the product but are not protected against the endpoint. ",
              "It appears directly as the y-intercept of the fitted waning curve."
            ),
            tags$div(
              class = "model-how",
              HTML("<code>eff(0) = a &nbsp;&nbsp; (y-intercept of the fitted waning curve)</code>")
            ),

            tags$h6("Parameter \u03b2 \u2014 waning rate", class = "about-section-h mt-3"),
            tags$p(class = "about-intro",
              tags$strong("\u03b2"), " (wane_b in the Stan model) is the per-day rate at which protection ",
              "drains through each immunological compartment. For the ",
              tags$strong("exponential model"), ", \u03b2 is the instantaneous hazard of waning, ",
              "and the protection half-life is:"
            ),
            tags$div(
              class = "model-how",
              HTML("<code>t&#189; = ln(2) / \u03b2 &nbsp;&nbsp; (days until efficacy falls to a/2, exponential only)</code>")
            ),
            tags$p(class = "about-intro mt-2",
              "For ", tags$strong("Erlang-k"), " models the same \u03b2 governs the transition between ",
              "k sequential compartments (compartment drain rate = k\u03b2). ",
              "Because an individual must pass through all k stages before losing protection, ",
              "the effective time to half-protection is ", tags$em("longer"), " than ln(2)/\u03b2, ",
              "giving the characteristic plateau-then-fall shape seen in the compartment diagrams above."
            ),

            tags$h6("Approximate values for the built-in datasets", class = "about-section-h mt-3"),
            tags$p(class = "about-intro",
              "Trial-reported primary-endpoint VEs as a guide to a, together with approximate ",
              "exponential half-lives estimated from the published waning data. ",
              "Fitting the Stan models gives the full posterior distribution of both parameters."
            ),
            tags$table(
              class = "ref-table mb-2",
              tags$thead(tags$tr(
                tags$th("Product"),
                tags$th("Trial VE (\u2248\u2009a)"),
                tags$th("Approx. exp. half-life"),
                tags$th("Source")
              )),
              tags$tbody(
                tags$tr(
                  tags$td("Nirsevimab (MELODY\u2009/\u2009MEDLEY)"),
                  tags$td("~75%"),
                  tags$td("~150 days"),
                  tags$td(tags$a("Hammitt et al. 2022",
                    href = "https://www.sciencedirect.com/science/article/pii/S2352464222003212",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("Maternal vaccine \u2014 Pfizer MATISSE"),
                  tags$td("~57%"),
                  tags$td("~90\u2013150 days"),
                  tags$td(tags$a("Kampmann et al. 2023",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2216480",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA GSK Arexvy"),
                  tags$td("~83%"),
                  tags$td(">12 months"),
                  tags$td(tags$a("Papi et al. 2023",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2209604",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA Pfizer Abrysvo"),
                  tags$td("~67%"),
                  tags$td("~9\u201312 months"),
                  tags$td(tags$a("Walsh et al. 2023",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2213836",
                    target = "_blank", class = "ref-link"))
                )
              )
            ),
            tags$p(class = "about-intro",
              tags$em(
                "Half-life estimates are approximate and based on exponential fits to published ",
                "waning data; Erlang-k models yield longer effective half-lives for the same \u03b2. ",
                "Click \u2018Fit Models\u2019 to obtain full posterior distributions."
              )
            )
          )
        ),

        card(          card_header("Fitting pipeline — how it works"),
          card_body(
            tags$p(class = "about-intro",
              "Three-stage estimation illustrated using the Pfizer MATISSE maternal RSV ",
              "vaccine trial (Kampmann et al. 2023). Step 3 waning curves use illustrative ",
              "parameters to demonstrate model shapes; actual posterior curves are produced ",
              "by the Stan sampler after clicking \u2018Fit Models\u2019."
            ),
            tags$div(
              class = "row g-2 mt-1",
              tags$div(
                class = "col-md-4",
                tags$div(
                  class = "pipe-step-card",
                  tags$div(class = "pipe-step-header",
                    tags$span(class = "pipe-step-num", "\u2460"),
                    " Observed interval data"
                  ),
                  plotOutput("pipe_p1", height = "270px")
                )
              ),
              tags$div(
                class = "col-md-4",
                tags$div(
                  class = "pipe-step-card",
                  tags$div(class = "pipe-step-header",
                    tags$span(class = "pipe-step-num", "\u2461"),
                    " Background hazard \u03bb(t)"
                  ),
                  plotOutput("pipe_p2", height = "270px")
                )
              ),
              tags$div(
                class = "col-md-4",
                tags$div(
                  class = "pipe-step-card",
                  tags$div(class = "pipe-step-header",
                    tags$span(class = "pipe-step-num", "\u2462"),
                    " Fitted waning efficacy"
                  ),
                  plotOutput("pipe_p3", height = "270px")
                )
              )
            )
          )
        ),

        card(          card_header("Data format & preparation"),
          card_body(

            # ── Required format ───────────────────────────────────────────────
            tags$h6("Required CSV format", class = "about-section-h"),
            tags$p(class = "about-intro",
              "Each arm (vaccine and placebo/control) must be supplied as a separate CSV ",
              "with one row per time interval. Four columns are required:"
            ),
            tags$table(
              class = "ref-table mb-2",
              tags$thead(tags$tr(
                tags$th("Column"), tags$th("Type"), tags$th("Description")
              )),
              tags$tbody(
                tags$tr(
                  tags$td(tags$code("tp")),
                  tags$td("integer"),
                  tags$td("Time-point index (1, 2, 3, \u2026) used by the GP covariance kernel.")
                ),
                tags$tr(
                  tags$td(tags$code("t")),
                  tags$td("numeric"),
                  tags$td("Midpoint of the interval, in days post-vaccination / post-birth.")
                ),
                tags$tr(
                  tags$td(tags$code("person_time")),
                  tags$td("numeric"),
                  tags$td("Total person-days of follow-up accumulated within this interval.")
                ),
                tags$tr(
                  tags$td(tags$code("n")),
                  tags$td("integer"),
                  tags$td("Number of endpoint events (e.g. RSV-LRTI cases) observed in this interval.")
                )
              )
            ),

            # ── Person-time ───────────────────────────────────────────────────
            tags$h6("Calculating person-time from at-risk counts", class = "about-section-h mt-3"),
            tags$p(class = "about-intro",
              "Trial papers typically report the number at risk at a series of timepoints ",
              tags$em("t"),
              "\u2080, ",
              tags$em("t"),
              "\u2081, \u2026 alongside a Kaplan\u2013Meier curve. ",
              "Person-time for interval [",
              tags$em("t"),
              "\u1d62, ",
              tags$em("t"),
              "\u1d62\u208a\u2081] is approximated by the ",
              tags$strong("trapezoid rule"),
              ":"
            ),
            tags$div(
              class = "model-how",
              HTML("<code>person_time<sub>i</sub> = (N<sub>i</sub> + N<sub>i+1</sub>) / 2 &times; (t<sub>i+1</sub> &minus; t<sub>i</sub>)</code>")
            ),
            tags$p(class = "about-intro mt-2",
              "where N\u1d62 is the number at risk at ",
              tags$em("t"),
              "\u1d62. This estimates the area under the at-risk curve over the interval. ",
              "For the most accurate estimate, use the at-risk counts recorded at each reported ",
              "timepoint rather than interpolating them."
            ),

            # ── How preloaded data were prepared ──────────────────────────────
            tags$h6("How the built-in datasets were prepared", class = "about-section-h mt-3"),
            tags$p(class = "about-intro",
              "Two approaches were used, depending on the level of data available in the ",
              "published paper:"
            ),
            tags$div(
              class = "about-method-block",
              tags$strong("Most datasets (Maternal, OA-GSK, OA-Pfizer)"),
              tags$p(class = "about-intro mt-1",
                "At-risk counts (N\u1d62) and cumulative event counts were read from the ",
                "published tables or digitised from the KM figure. ",
                "Person-time per interval was then computed with the trapezoid rule above. ",
                "Incident events per interval equal the difference between successive ",
                "cumulative counts."
              )
            ),
            tags$div(
              class = "about-method-block",
              tags$strong("Nirsevimab (MELODY\u2009/\u2009MEDLEY \u2014 event-level reconstruction)"),
              tags$p(class = "about-intro mt-1",
                "Individual-level event times were digitised from the published KM curves using ",
                "the Guyot algorithm, giving a sequence of events (type I\u2009=\u2009infection, ",
                "C\u2009=\u2009censoring) with exact day. A standard KM estimator was then run on these ",
                "events to recover the at-risk count at each event day, from which person-time ",
                "was accumulated. The resulting daily series was aggregated to 1-day intervals ",
                "before fitting. This approach is used when no at-risk table is published but a ",
                "KM figure is available."
              )
            ),
            tags$p(class = "about-intro mt-2",
              "Example R code reproducing the trapezoid calculation for a uniformly spaced ",
              "monthly follow-up:"
            ),
            tags$div(
              class = "model-how",
              HTML(paste0(
                "<code>dt &lt;- 30  # days per interval<br>",
                "ts &lt;- seq(15, 165, dt)  # interval midpoints<br>",
                "person_time &lt;- (at_risk[-l] + at_risk[-1]) * dt / 2<br>",
                "n &lt;- diff(cumulative_events)</code>"
              ))
            )
          )
        ),

        card(
          card_header("References"),
          card_body(
            tags$p(
              class = "about-intro",
              "The statistical framework is described in:"
            ),
            tags$div(
              class = "ref-block",
              tags$strong("Hodgson D, Wilkins N, van Leeuwen E et al. (2024)"),
              " Protecting infants against RSV disease: an impact and cost-effectiveness ",
              "comparison of long-acting monoclonal antibodies and maternal vaccination. ",
              tags$em("The Lancet Regional Health \u2014 Europe"),
              ", 38. ",
              tags$a("doi:10.1016/S2666-7762(23)00248-X",
                     href = "https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00248-X/fulltext",
                     target = "_blank", rel = "noopener", class = "ref-link")
            ),
            tags$p(class = "about-intro mt-3", "Built-in trial data sources:"),
            tags$table(
              class = "ref-table",
              tags$thead(tags$tr(
                tags$th("Dataset"), tags$th("Citation")
              )),
              tags$tbody(
                tags$tr(
                  tags$td("Nirsevimab (MELODY / MEDLEY)"),
                  tags$td(tags$a("Hammitt et al. 2022, Lancet",
                    href = "https://www.sciencedirect.com/science/article/pii/S2352464222003212",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("Maternal vaccine \u2014 Pfizer MATISSE"),
                  tags$td(tags$a("Kampmann et al. 2023, NEJM",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2216480",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA GSK Arexvy"),
                  tags$td(tags$a("Papi et al. 2023, NEJM",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2209604",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA Pfizer Abrysvo"),
                  tags$td(tags$a("Walsh et al. 2023, NEJM",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2213836",
                    target = "_blank", class = "ref-link"))
                )
              )
            )
          )
        )
      )
    )
  )
)


# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  fit_result <- reactiveVal(NULL)

  # ── Navigate to About tab from sidebar link ──────────────────────────────────
  observeEvent(input$goto_about, {
    nav_select("main_tabs", "tab_about")
  })

  # ── Dataset paper reference ───────────────────────────────────────────────────
  output$paper_ref <- renderUI({
    ref <- DATASET_REFS[[input$builtin_dataset]]
    if (is.null(ref)) return(NULL)
    tags$div(
      class = "paper-ref",
      icon("file-lines"),
      " ",
      tags$a(ref$cite, href = ref$url, target = "_blank",
             rel = "noopener", class = "paper-ref-link")
    )
  })

  # ── Load data ────────────────────────────────────────────────────────────────
  loaded_data <- reactive({
    if (input$data_source == "builtin") {
      tryCatch(
        rsv_example(input$builtin_dataset),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
    } else {
      req(input$vac_csv, input$pla_csv)
      tryCatch(
        load_trial_csv(
          input$vac_csv$datapath,
          input$pla_csv$datapath
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
    }
  })

  # ── Data tables ──────────────────────────────────────────────────────────────
  make_arm_table <- function(arm) {
    renderDT({
      dat <- loaded_data()
      req(dat)
      df         <- as.data.frame(dat[[arm]])
      df$rate    <- round(df$n / df$person_time * 1000, 3)
      names(df)  <- c("tp (index)", "t (days)", "person-days", "events",
                      "rate / 1 000 PD")
      datatable(
        df,
        options  = list(
          pageLength = 15, dom = "tp",
          scrollX = TRUE, autoWidth = FALSE,
          columnDefs = list(list(className = "dt-right", targets = 1:4))
        ),
        rownames = FALSE,
        class    = "compact stripe hover"
      ) |>
        formatRound(c("t (days)", "person-days"), digits = 1) |>
        formatRound("rate / 1 000 PD",             digits = 3)
    })
  }

  output$tbl_vac <- make_arm_table("vaccine")
  output$tbl_pla <- make_arm_table("placebo")

  # ── Fit button ───────────────────────────────────────────────────────────────
  observeEvent(input$fit_btn, {

    dat <- loaded_data()
    if (is.null(dat)) {
      showNotification("No data loaded.", type = "warning")
      return()
    }

    models_sel <- input$models
    if (length(models_sel) == 0) {
      showNotification("Select at least one waning model.", type = "warning")
      return()
    }

    shinyjs::disable("fit_btn")
    fit_result(NULL)

    result <- withProgress(
      message = "Fitting Stan models\u2026",
      detail  = paste("Models:", paste(toupper(models_sel), collapse = ", ")),
      value   = 0.05,
      {
        setProgress(0.10, detail = "Compiling Stan model\u2026")

        ub <- rep(input$upper_bound_b, length(models_sel))
        n_cores <- min(
          as.integer(input$chains),
          max(1L, parallel::detectCores(logical = FALSE))
        )

        # withCallingHandlers logs the FULL call stack to R console
        # (visible in shinyapps.io Application Logs) before tryCatch unwinds it.
        tryCatch(
          withCallingHandlers(
            fit_efficacy(
              data            = dat,
              models          = models_sel,
              bounded         = isTRUE(input$bounded),
              upper_bound_b   = ub,
              chains          = as.integer(input$chains),
              parallel_chains = n_cores,
              iter_warmup     = as.integer(input$iter_w),
              iter_sampling   = as.integer(input$iter_s),
              seed            = as.integer(input$seed),
              refresh         = 0
            ),
            error = function(cond) {
              # Log full traceback to shinyapps.io Application Logs
              message("=== FIT ERROR ===")
              message("Message : ", conditionMessage(cond))
              message("Call    : ", paste(deparse(conditionCall(cond)), collapse = " "))
              message("Sink #  : ", sink.number())
              calls <- sys.calls()
              message("Stack depth: ", length(calls))
              for (i in seq_along(calls)) {
                message(sprintf("  [%02d] %s", i, deparse(calls[[i]])[1]))
              }
              message("=== END ===")
            }
          ),
          error = function(e) {
            showNotification(
              paste("Fitting failed:", conditionMessage(e)),
              type = "error", duration = 20
            )
            NULL
          }
        )
      }
    )

    fit_result(result)
    shinyjs::enable("fit_btn")

    if (!is.null(result)) {
      nav_select("main_tabs", "tab_waning")
      showNotification(
        paste("Fitted:", paste(toupper(result$models), collapse = ", ")),
        type = "message", duration = 4
      )
    }
  })

  # ── Sidebar status badge ─────────────────────────────────────────────────────
  output$fit_status <- renderUI({
    res <- fit_result()
    if (is.null(res)) {
      tags$p(class = "status-badge status-none",
             icon("circle"), " No fit yet")
    } else {
      tags$p(class = "status-badge status-done",
             icon("circle-check"),
             paste(" Fitted:", paste(toupper(res$models), collapse = " \u00b7 ")))
    }
  })

  # ── Reactive plots ───────────────────────────────────────────────────────────
  waning_plot <- reactive({
    res <- fit_result()
    if (is.null(res)) return(empty_plot())
    plot_waning(res)
  })

  incidence_plot <- reactive({
    res <- fit_result()
    if (is.null(res)) return(empty_plot())
    plot_incidence(res)
  })

  output$plot_waning <- renderPlot({
    print(waning_plot())
  }, res = 150)

  output$plot_incidence <- renderPlot({
    print(incidence_plot())
  }, res = 150)

  # ── Downloads ────────────────────────────────────────────────────────────────
  output$dl_waning <- downloadHandler(
    filename = function() "waning_efficacy.png",
    content  = function(file) {
      ggplot2::ggsave(file, plot = waning_plot(),
                      width = 10, height = 6, dpi = 300, bg = "white")
    }
  )

  output$dl_incidence <- downloadHandler(
    filename = function() "incidence_fit.png",
    content  = function(file) {
      ggplot2::ggsave(file, plot = incidence_plot(),
                      width = 12, height = 5, dpi = 300, bg = "white")
    }
  )

  output$dl_curves_csv <- downloadHandler(
    filename = function() "waning_curves.csv",
    content  = function(file) {
      res <- fit_result()
      req(res)
      rows <- lapply(res$models, function(m) {
        w <- rstan::extract(res$fits[[m]], pars = "waning")$waning  # [draws x 730]
        t_seq <- 0:729
        data.frame(
          model  = toupper(m),
          t_days = t_seq,
          median = apply(w, 2, stats::median),
          lb_95  = apply(w, 2, stats::quantile, 0.025),
          ub_95  = apply(w, 2, stats::quantile, 0.975),
          lb_50  = apply(w, 2, stats::quantile, 0.25),
          ub_50  = apply(w, 2, stats::quantile, 0.75)
        )
      })
      utils::write.csv(do.call(rbind, rows), file, row.names = FALSE)
    }
  )

  output$dl_posteriors_csv <- downloadHandler(
    filename = function() "posterior_samples.csv",
    content  = function(file) {
      res <- fit_result()
      req(res)
      rows <- lapply(res$models, function(m) {
        p <- rstan::extract(res$fits[[m]], pars = c("wane_a", "wane_b", "alpha", "rho"))
        data.frame(
          model  = toupper(m),
          draw   = seq_len(length(p$wane_a)),
          wane_a = p$wane_a,
          wane_b = p$wane_b,
          alpha  = p$alpha,
          rho    = p$rho
        )
      })
      utils::write.csv(do.call(rbind, rows), file, row.names = FALSE)
    }
  )

  # ── Diagnostics table ────────────────────────────────────────────────────────
  output$tbl_diag <- renderDT({
    res <- fit_result()
    if (is.null(res)) {
      return(datatable(
        data.frame(Message = "Run a fit to see diagnostics."),
        options  = list(dom = "t"),
        rownames = FALSE
      ))
    }

    rows <- lapply(res$models, function(m) {
      fit  <- res$fits[[m]]

      sp   <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE),
                       error = function(e) NULL)
      summ <- tryCatch(as.data.frame(rstan::summary(fit)$summary),
                       error = function(e) NULL)

      n_div  <- if (!is.null(sp))
        sum(sapply(sp, function(x) sum(x[, "divergent__"]))) else NA_integer_
      n_tree <- if (!is.null(sp))
        sum(sapply(sp, function(x) sum(x[, "treedepth__"] >= 10))) else NA_integer_

      data.frame(
        Model            = toupper(m),
        Divergences      = n_div,
        `Max tree depth` = n_tree,
        `Max Rhat`       = if (!is.null(summ)) round(max(summ$Rhat,  na.rm = TRUE), 4) else NA_real_,
        `Min ESS`        = if (!is.null(summ)) round(min(summ$n_eff, na.rm = TRUE), 0) else NA_real_,
        check.names      = FALSE,
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, rows)

    datatable(
      df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", pageLength = 10),
      class    = "compact stripe"
    ) |>
      formatStyle(
        "Divergences",
        backgroundColor = styleInterval(c(0, 5),
                                        c("#E8F5E9", "#FFF8E1", "#FFEBEE"))
      ) |>
      formatStyle(
        "Max Rhat",
        backgroundColor = styleInterval(c(1.01, 1.05),
                                        c("#E8F5E9", "#FFF8E1", "#FFEBEE"))
      ) |>
      formatStyle(
        "Min ESS",
        backgroundColor = styleInterval(c(100, 400),
                                        c("#FFEBEE", "#FFF8E1", "#E8F5E9"))
      )
  })
  # ── Pipeline schematic plots (About tab) ────────────────────────────────────────────
  .pipe_data <- local({
    dat <- rsv_example("maternal")
    vac <- dat$vaccine
    pla <- dat$placebo
    vac$rate <- 1000 * vac$n / vac$person_time
    pla$rate <- 1000 * pla$n / pla$person_time
    lrr    <- log(vac$rate / pla$rate)
    se_lrr <- sqrt(1 / pmax(vac$n, 0.5) + 1 / pmax(pla$n, 0.5))
    list(
      vac   = vac, pla = pla,
      ve    = 1 - exp(lrr),
      ve_lo = 1 - exp(lrr + 1.96 * se_lrr),
      ve_hi = 1 - exp(lrr - 1.96 * se_lrr)
    )
  })

  output$pipe_p1 <- renderPlot({
    pd <- .pipe_data
    df <- rbind(
      data.frame(t = pd$vac$t, rate = pd$vac$rate, arm = "Vaccine"),
      data.frame(t = pd$pla$t, rate = pd$pla$rate, arm = "Placebo")
    )
    ggplot(df, aes(x = t, y = rate, colour = arm)) +
      geom_point(size = 3.0) +
      geom_line(linewidth = 1.0, alpha = 0.75) +
      scale_colour_manual(
        values = c("Vaccine" = "#0D7680", "Placebo" = "#990F3D"), name = NULL
      ) +
      labs(
        title    = "Observed interval event rates",
        subtitle = "MATISSE trial \u2014 Kampmann et al. 2023",
        x        = "Days post-vaccination",
        y        = "Events / 1,000 person-days"
      ) +
      theme_ft()
  }, bg = "#FFFFFF", res = 96)

  output$pipe_p2 <- renderPlot({
    pd    <- .pipe_data
    pla   <- pd$pla
    t_seq <- seq(pla$t[1] - 10, tail(pla$t, 1) + 10, by = 1)
    sp    <- smooth.spline(pla$t, pla$rate, df = 3)
    fit   <- pmax(predict(sp, t_seq)$y, 0)
    df_pts <- data.frame(t = pla$t, rate = pla$rate)
    df_fit <- data.frame(t = t_seq, lo = fit * 0.65, hi = fit * 1.35, mid = fit)
    ggplot() +
      geom_ribbon(data = df_fit,
        aes(x = t, ymin = lo, ymax = hi), fill = "#990F3D", alpha = 0.18) +
      geom_line(data = df_fit,
        aes(x = t, y = mid), colour = "#990F3D", linewidth = 1.2) +
      geom_point(data = df_pts,
        aes(x = t, y = rate), colour = "#990F3D", size = 3.5) +
      labs(
        title    = "Background hazard \u03bb(t)",
        subtitle = "Spline on placebo-arm rates (Bayesian GP in full model)",
        x        = "Days post-vaccination",
        y        = "\u03bb(t) \u2014 events / 1,000 person-days"
      ) +
      theme_ft()
  }, bg = "#FFFFFF", res = 96)

  output$pipe_p3 <- renderPlot({
    pd    <- .pipe_data
    t_seq <- seq(0, tail(pd$vac$t, 1) + 15, by = 1)
    a <- 0.57; beta <- 0.006
    wane <- data.frame(
      t     = rep(t_seq, 3),
      eff   = c(
        a * exp(-beta * t_seq),
        a * pgamma(t_seq, 2, 2 * beta, lower.tail = FALSE),
        a * pgamma(t_seq, 3, 3 * beta, lower.tail = FALSE)
      ),
      model = rep(c("Exponential", "Erlang-2", "Erlang-3"), each = length(t_seq))
    )
    obs <- data.frame(
      t     = pd$vac$t,
      ve    = pd$ve,
      ve_lo = pmax(pd$ve_lo, 0),
      ve_hi = pmin(pd$ve_hi, 1)
    )
    ggplot() +
      geom_hline(yintercept = 0, colour = "#E9E4D9", linewidth = 0.5) +
      geom_errorbar(data = obs,
        aes(x = t, ymin = ve_lo, ymax = ve_hi),
        width = 4, colour = "#9E9B93", linewidth = 0.8) +
      geom_point(data = obs, aes(x = t, y = ve), colour = "#33302E", size = 3.5) +
      geom_line(data = wane, aes(x = t, y = eff, colour = model), linewidth = 1.2) +
      scale_colour_manual(
        values = c("Exponential" = "#0D7680", "Erlang-2" = "#990F3D", "Erlang-3" = "#593380"),
        name = NULL
      ) +
      scale_y_continuous(
        limits = c(-0.05, 1),
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      labs(
        title    = "Fitted waning efficacy",
        subtitle = "Points: period VE \u00b1 95% CI; curves: illustrative model shapes",
        x        = "Days post-vaccination",
        y        = "Efficacy eff(t)"
      ) +
      theme_ft()
  }, bg = "#FFFFFF", res = 96)
}

# ── Launch ──────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
