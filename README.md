# effestimator-dashboard

A browser-based Shiny dashboard for fitting Bayesian waning vaccine efficacy
models to randomised trial data. Powered by Stan via **RStan** (no separate
CmdStan installation required — deploys to shinyapps.io).

- **R package source**: [github.com/dchodge/effestimator](https://github.com/dchodge/effestimator)
- **Methods paper**: [Hodgson et al. 2023, Lancet Regional Health — Europe](https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00248-X/fulltext)

---

## Run locally

### 1 — Install R packages

```r
install.packages(c(
  "shiny", "bslib", "DT", "shinyjs",
  "rstan",           # Stan back-end (no extra install needed)
  "posterior", "tidybayes",
  "ggplot2", "patchwork",
  "readr", "tibble", "dplyr", "tidyr", "purrr"
))
```

### 2 — Clone and launch

```bash
git clone https://github.com/dchodge/effestimator-dashboard.git
cd effestimator-dashboard
```

```r
shiny::runApp()
```

---

## Deploy to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "<YOUR_ACCOUNT>",
  token  = "<YOUR_TOKEN>",
  secret = "<YOUR_SECRET>"
)
rsconnect::deployApp("effestimator-dashboard")
```

shinyapps.io will install all packages listed in `DESCRIPTION` automatically,
including RStan. The Stan models in `stan/` are compiled from source on first
use — this takes ~60–90 seconds the first time a fit is run on a fresh server
instance, then the compiled library is cached for the session.

---

## Workflow

| Step | What to do |
|------|-----------|
| **1 Data** | Select a built-in RSV dataset **or** upload your own pair of CSV files |
| **2 Models** | Tick the waning models to fit (Exponential, Erlang-2, Erlang-3) |
| **3 MCMC** | Expand *MCMC settings* to adjust chains / iterations (defaults are fine) |
| **Fit** | Click **Fit Models** — Stan runs; results appear when done |

---

## Built-in RSV datasets

| Dataset key | Intervention | Trial / source |
|-------------|-------------|---------------|
| `nirsevimab` | Nirsevimab (infant monoclonal antibody) | MELODY + MEDLEY pooled — Hammitt et al. 2022 |
| `maternal` | Maternal RSV vaccine — Pfizer MATISSE | Kampmann et al. 2023, *NEJM* |
| `oa_gsk` | Older-adult RSV vaccine — GSK Arexvy | Papi et al. 2023, *NEJM* |
| `oa_pfizer` | Older-adult RSV vaccine — Pfizer Abrysvo | Walsh et al. 2023, *NEJM* |
| `oa_moderna` | Older-adult RSV vaccine — Moderna mRESVIA | Wilson et al. 2023, *NEJM* |
| `oa_papirovax` | Older-adult RSV subunit vaccine candidate | Papi et al. 2023, *NEJM* |

---

## Upload your own data

Prepare **two CSV files** (vaccine arm and placebo/control arm) with at minimum:

| Column | Description |
|--------|-------------|
| `t` | Midpoint of interval in **days** post-vaccination |
| `person_time` | Person-days of follow-up in this interval |
| `n` | Number of confirmed events (cases) in this interval |

---

## Dashboard tabs

| Tab | Content |
|-----|---------|
| **Data** | Preview both trial arms — event counts, person-time, crude rates |
| **Waning Efficacy** | Posterior median + 95% CrI for each waning model; PNG download |
| **Incidence Fit** | GP-fitted incidence vs. observed rates per interval |
| **Diagnostics** | Per-model Rhat, ESS, and divergence count with colour coding |
| **About** | Compartment diagrams showing waning model structure; full references |

---

## File structure

```
effestimator-dashboard/
├── app.R            # Shiny UI + server
├── R/
│   ├── data.R       # Built-in RSV datasets + CSV loader
│   ├── fit.R        # Stan fitting via RStan
│   ├── plots.R      # ggplot2 waning/incidence plots
│   ├── theme.R      # FT-inspired chart theme + colour palette
│   └── helpers.R    # Dependency check
├── stan/
│   ├── eff_est.stan       # GP + Erlang-k, configurable wane_b bound
│   └── eff_est_none.stan  # GP + Erlang-k, fixed wane_b ≤ 0.1
├── extdata/         # Paired CSVs for built-in datasets
└── www/styles.css   # FT-styled CSS
```

---

## Quick start

### 1 — Install prerequisites

```r
# Core package (from the parent folder of this repo)
devtools::install_local("../")

# Or from GitHub once published:
# devtools::install_github("dchodge/effestimatorfull")

# Stan back-end
install.packages("cmdstanr", repos = "https://mc-stan.org/r-packages/")
cmdstanr::install_cmdstan()

# Dashboard dependencies
install.packages(c("shiny", "bslib", "DT", "shinyjs"))
```

### 2 — Launch

```r
shiny::runApp("effestimator-dashboard")

# or from inside this folder:
shiny::runApp()
```

---

## Workflow

| Step | What to do |
|------|-----------|
| **1 Data** | Select a built-in RSV dataset **or** upload your own pair of CSV files |
| **2 Models** | Tick the waning models to fit (Exponential, Erlang-2, Erlang-3) |
| **3 MCMC** | Expand *MCMC settings* to adjust chains / iterations (defaults are fine) |
| **Fit** | Click **Fit Models** — Stan runs in the background; results appear when done |

---

## Built-in RSV datasets

| Dataset key | Intervention | Trial / source |
|-------------|-------------|---------------|
| `nirsevimab` | Nirsevimab (infant monoclonal antibody) | MELODY + MEDLEY pooled — Hammitt et al. 2022 |
| `maternal` | Maternal RSV vaccine — Pfizer MATISSE | Kampmann et al. 2023, *NEJM* |
| `oa_gsk` | Older-adult RSV vaccine — GSK Arexvy | Papi et al. 2023, *NEJM* |
| `oa_pfizer` | Older-adult RSV vaccine — Pfizer Abrysvo | Walsh et al. 2023, *NEJM* |
| `oa_moderna` | Older-adult RSV vaccine — Moderna mRESVIA | Moderna Phase 3 trial |
| `oa_papirovax` | Older-adult RSV subunit vaccine candidate | — |

---

## Upload your own data

Prepare **two CSV files** (vaccine arm and placebo/control arm) with at minimum:

| Column | Description |
|--------|-------------|
| `t` | Midpoint of interval in **days** post-vaccination |
| `person_time` | Person-days of follow-up in this interval |
| `n` | Number of confirmed events (cases) in this interval |

Rows must be in chronological order. Template files are in
[`../inst/extdata/`](../inst/extdata/).

---

## Dashboard tabs

| Tab | Content |
|-----|---------|
| **Data** | Preview both trial arms — event counts, person-time, crude rates |
| **Waning Efficacy** | Posterior median + 95% CrI for each waning model; PNG download |
| **Incidence Fit** | GP-fitted incidence vs. observed rates per interval |
| **Diagnostics** | Per-model Rhat, ESS, and divergence count with colour coding |

---

## Design

Styled after the [Financial Times](https://www.ft.com) data-visualisation
aesthetic: white canvas, horizontal gridlines only, FT teal (`#0D7680`) as
the primary accent, and a warm cream sidebar.
