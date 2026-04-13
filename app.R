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
  "Older adult — GSK Arexvy (multi-season)" = "oa_gsk",
  "Older adult — Pfizer Abrysvo"            = "oa_pfizer",
  "Older adult — Moderna mRESVIA"           = "oa_moderna",
  "Older adult — Papirovax (Papi et al.)"   = "oa_papirovax"
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
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2304891"
  ),
  oa_moderna = list(
    cite = "Wilson et al. (2023) \u2013 NEJM",
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2309079"
  ),
  oa_papirovax = list(
    cite = "Papi et al. (2023) \u2013 NEJM",
    url  = "https://www.nejm.org/doi/10.1056/NEJMoa2209604"
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
  title = tags$span(
    class = "brand-wrap",
    tags$span("Efficacy Estimator", class = "brand-main"),
    tags$span(
      class = "brand-links",
      tags$a("R package", href = "https://github.com/dchodge/effestimator",
             target = "_blank", rel = "noopener"),
      " \u00b7 ",
      tags$a("Hodgson et al. Lancet",
             href = "https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00248-X/fulltext",
             target = "_blank", rel = "noopener")
    )
  ),
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
        numericInput("chains",  "Chains",          value = 4,    min = 1, max = 8),
        numericInput("iter_w",  "Warmup / chain",  value = 1000, min = 100, step = 100),
        numericInput("iter_s",  "Samples / chain", value = 1000, min = 100, step = 100),
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
          downloadButton("dl_waning", label = NULL, icon = icon("download"),
                         class = "btn-sm btn-outline-secondary dl-btn ms-auto")
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
        card_header("MCMC convergence diagnostics"),
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
          card_header("References"),
          card_body(
            tags$p(
              class = "about-intro",
              "The statistical framework is described in:"
            ),
            tags$div(
              class = "ref-block",
              tags$strong("Hodgson et al. (2023)"),
              " Optimising RSV prophylaxis for infants in England: a health economics modelling study. ",
              tags$em("Lancet Regional Health \u2014 Europe."),
              " ",
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
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2304891",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA Moderna mRESVIA"),
                  tags$td(tags$a("Wilson et al. 2023, NEJM",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2309079",
                    target = "_blank", class = "ref-link"))
                ),
                tags$tr(
                  tags$td("OA Papirovax"),
                  tags$td(tags$a("Papi et al. 2023, NEJM",
                    href = "https://www.nejm.org/doi/10.1056/NEJMoa2209604",
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

        tryCatch(
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
          error = function(e) {
            showNotification(
              paste("Fitting failed:", conditionMessage(e)),
              type = "error", duration = 12
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
}

# ── Launch ──────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
