# Built-in RSV example datasets and CSV loading helpers

# ── KM utilities (nirsevimab only) ───────────────────────────────────────────

.km_est <- function(events) {
  N <- nrow(events)
  events$at_risk_before <- sum(events$n) -
    cumsum(c(0, events$n[seq_len(N - 1)]))

  km <- data.frame(t = 0, Proportion_Free = 1)
  j <- 0

  for (i in seq_len(N)) {
    j <- j + 1
    if (events$Type[i] == "C") {
      km <- rbind(km, c(events$t[i], km[j, 2]))
    } else {
      km <- rbind(km, c(events$t[i], km[j, 2]))
      km <- rbind(km, c(events$t[i],
                        km[j, 2] * (1 - events$n[i] / events$at_risk_before[i])))
      j <- j + 1
    }
  }
  events$pt_elapsed <- cumsum(
    events$at_risk_before * c(events$t[1] + 1, diff(events$t))
  )
  list(Events = events, KM_estimator = km)
}

.events_agg <- function(data, ts) {
  events  <- data$Events
  km      <- data$KM_estimator
  n_ts    <- length(ts)

  time_between <- diff(c(0, events$t))
  time_between[1] <- time_between[1] + 1
  events$pt_elapsed <- cumsum(events$at_risk_before * time_between)
  at_risk_f <- events$at_risk_before[1]

  out <- data.frame()
  for (i in seq_len(n_ts - 1)) {
    t_min <- ts[i]; t_max <- ts[i + 1]
    sub <- events[events$t >= t_min & events$t < t_max, ]

    if (nrow(sub) == 0) {
      pt <- at_risk_f * (t_max - t_min)
      n  <- 0
    } else {
      pt_before  <- sub$pt_elapsed[1] - sub$at_risk_before[1]
      ar_i       <- sub$at_risk_before[1]
      pt_start   <- pt_before - ar_i * (sub$t[1] - t_min)
      pt_end     <- tail(sub$pt_elapsed, 1) +
        (tail(sub$at_risk_before, 1) - tail(sub$n, 1)) * (t_max - 1 - tail(sub$t, 1))
      pt <- pt_end - pt_start
      n  <- sum(sub$n[sub$Type == "I"])
      at_risk_f <- tail(sub$at_risk_before, 1) - tail(sub$n, 1)
    }
    out <- rbind(out, data.frame(t = (t_min + t_max) / 2,
                                 person_time = pt, n = n))
  }
  out$tp <- seq_len(nrow(out))
  out[, c("tp", "t", "person_time", "n")]
}


# ── Raw event-level data (nirsevimab) ─────────────────────────────────────────

.nmab_events_pla <- function() {
  tibble::tibble(
    Type = c("C","I","I","I","I","I","I","I","C",
             "I","I","C","I","I","I","I","I","I","I",
             "I","I","I","I","I","I","I","I","C","I","I","I",
             "I","I","I","C","I","I",
             "I","I","I","C","C","C","C"),
    t    = c(1, 7, 8,15,17,19,22,23,26,
             30,33,33,37,48,49,50,51,53,57,
             63,64,66,69,73,75,77,78,79,80,82,84,
             91,92,111,113,115,117,
             121,122,132,144,149,150,151),
    n    = c(5, 1, 1, 1, 1, 2, 1, 1, 1,
             1, 2, 1, 3, 4, 1, 1, 1, 1, 1,
             2, 2, 1, 1, 1, 3, 1, 2, 1, 1, 3, 1,
             1, 2, 1, 1, 2, 1,
             1, 1, 1, 1, 1, 0, 724)
  )
}

.nmab_events_vac <- function() {
  tibble::tibble(
    Type = c("C","I","I","I",
             "C","C","C","I","I","I",
             "C","C","C","I","C","C","I","C",
             "I","I","C","C","I","I","I","I","I",
             "C","I","C","I","I","C","C","C","C"),
    t    = c(1, 7,21,28,
             35,38,45,51,54,57,
             68,71,73,74,74,75,82,85,
             92,93,97,99,102,103,108,109,112,123,127,127,
             136,138,140,143,146,151),
    n    = c(8, 1, 1, 1,
             1, 1, 1, 2, 1, 1,
             1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 2, 2, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 1, 1, 1, 1519)
  )
}


# ── Pre-tabulated aggregated data (other RSV products) ───────────────────────

.maternal_data <- function() {
  dt <- 30
  ts <- seq(15, 165, dt)
  at_risk_pla <- c(3480, 3288, 2964, 2879, 2804, 2738, 2700)
  cumul_pla   <- c(0, 15, 38, 56, 81, 99, 117)
  at_risk_vac <- c(3495, 3348, 3035, 2968, 2898, 2845, 2792)
  cumul_vac   <- c(0, 2, 14, 24, 35, 47, 57)
  l <- length(at_risk_pla)

  placebo <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_pla[1:(l-1)] + at_risk_pla[2:l]) * dt / 2,
    n           = diff(cumul_pla)
  )
  vaccine <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_vac[1:(l-1)] + at_risk_vac[2:l]) * dt / 2,
    n           = diff(cumul_vac)
  )
  list(placebo = placebo, vaccine = vaccine)
}

.oa_gsk_data <- function() {
  dt <- 30
  ts <- seq(15, 825, dt)
  at_risk_pla <- c(12498,12403,12342,12276,12225,12183,12138,12077,11977,
                   11846,11631,11332,10868,10469,10080, 9855, 9790, 9753,
                    9710, 9676, 9432, 9200, 8436, 6882, 4659, 2585,  920, 70, 0)
  cumul_pla   <- c(0, 8,20,27,32,37,39,41,43,45,47,47,51,57,70,
                   92,113,127,140,144,146,149,150,153,153,154,154,154,154)
  at_risk_vac <- c(12468,12392,12338,12293,12254,12212,12159,12114,12005,
                   11875,11265,10019, 8010, 6538, 5478, 5029, 4940, 4903,
                    4883, 4861, 4732, 4598, 4214, 3437, 2323, 1279,  444, 38, 0)
  cumul_vac   <- c(0, 1, 3, 5, 6, 7, 7, 8, 8, 8, 8, 8,10,10,11,
                   18, 22, 25, 29, 30, 30, 31, 32, 32, 32, 32, 32, 32, 32)
  l <- length(at_risk_pla)

  placebo <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_pla[1:(l-1)] + at_risk_pla[2:l]) * dt / 2,
    n           = diff(cumul_pla)
  )
  vaccine <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_vac[1:(l-1)] + at_risk_vac[2:l]) * dt / 2,
    n           = diff(cumul_vac)
  )
  list(placebo = placebo, vaccine = vaccine)
}

.oa_pfizer_data <- function() {
  ts          <- c(1, 115, 221, 324, 436, 541, 762)
  at_risk_pla <- c(18074, 17524, 16897, 15303, 14448,  7250, 1)
  cumul_pla   <- c(0, 22, 37, 62, 101, 127, 131)
  at_risk_vac <- c(18050, 17532, 16978, 15372, 14304,  7385, 0)
  cumul_vac   <- c(0,  8, 12, 24,  40,  52,  54)
  l <- length(ts)
  dt <- diff(ts)

  placebo <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = (ts[1:(l-1)] + ts[2:l]) / 2,
    person_time = (at_risk_pla[1:(l-1)] + at_risk_pla[2:l]) * dt / 2,
    n           = diff(cumul_pla)
  )
  vaccine <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = (ts[1:(l-1)] + ts[2:l]) / 2,
    person_time = (at_risk_vac[1:(l-1)] + at_risk_vac[2:l]) * dt / 2,
    n           = diff(cumul_vac)
  )
  list(placebo = placebo, vaccine = vaccine)
}

.oa_moderna_data <- function() {
  ts          <- c(0, 90, 180, 270, 345)
  at_risk_pla <- c(17474, 17428, 17389, 17369, 17358)
  cumul_pla   <- c(0, 47, 85, 105, 116)

  # Pooled highest-efficacy arm (max across dose/formulation groups)
  at_risk_vac <- c(17601, 17583, 17567, 17559, 17556)
  cumul_vac   <- c(0, 18, 34, 42, 44)
  l  <- length(ts)
  dt <- diff(ts)

  placebo <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = (ts[1:(l-1)] + ts[2:l]) / 2,
    person_time = (at_risk_pla[1:(l-1)] + at_risk_pla[2:l]) * dt / 2,
    n           = diff(cumul_pla)
  )
  vaccine <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = (ts[1:(l-1)] + ts[2:l]) / 2,
    person_time = (at_risk_vac[1:(l-1)] + at_risk_vac[2:l]) * dt / 2,
    n           = diff(cumul_vac)
  )
  list(placebo = placebo, vaccine = vaccine)
}

.oa_papirovax_data <- function() {
  dt          <- 30
  ts          <- seq(15, 315, dt)
  at_risk_pla <- c(12494, 12390, 12268, 11853, 11597, 10973, 8255, 5441, 2697, 554, 2, 0)
  cumul_pla   <- c(0, 22, 43, 62, 76, 86, 90, 95, 95, 95, 95, 95)
  at_risk_vac <- c(12466, 12390, 12282, 11881, 11641, 11029, 8305, 5481, 2717, 570, 2, 0)
  cumul_vac   <- c(0,  3,  7, 15, 19, 23, 24, 26, 27, 27, 27, 27)
  l <- length(at_risk_pla)

  placebo <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_pla[1:(l-1)] + at_risk_pla[2:l]) * dt / 2,
    n           = diff(cumul_pla)
  )
  vaccine <- tibble::tibble(
    tp          = seq_len(l - 1),
    t           = ts,
    person_time = (at_risk_vac[1:(l-1)] + at_risk_vac[2:l]) * dt / 2,
    n           = diff(cumul_vac)
  )
  list(placebo = placebo, vaccine = vaccine)
}

.nmab_data <- function() {
  ts       <- seq(0, 154, 7)   # 7-day bins (~22 intervals) for faster fitting
  pla_data <- .km_est(.nmab_events_pla())
  vac_data <- .km_est(.nmab_events_vac())
  list(
    placebo = .events_agg(pla_data, ts),
    vaccine = .events_agg(vac_data, ts)
  )
}


# ── Public interface ──────────────────────────────────────────────────────────

#' Load a built-in RSV trial dataset
#'
#' Returns aggregated interval-format trial data for one of five RSV
#' interventions studied in published randomised controlled trials.
#'
#' @param dataset Character string identifying the dataset. One of:
#'   \describe{
#'     \item{\code{"nirsevimab"}}{Nirsevimab (infant RSV monoclonal antibody).
#'       MELODY + MEDLEY pooled trial. Hammitt et al. 2022.}
#'     \item{\code{"maternal"}}{Maternal RSV vaccination (Pfizer MATISSE trial).
#'       Kampmann et al. 2023, NEJM.}
#'     \item{\code{"oa_gsk"}}{Older-adult RSV vaccine (GSK Arexvy,
#'       Papi et al. 2023, NEJM.}
#'     \item{\code{"oa_pfizer"}}{Older-adult RSV vaccine (Pfizer Abrysvo).
#'       Walsh et al. 2023, NEJM.}
#'     \item{\code{"oa_moderna"}}{Older-adult RSV vaccine (Moderna mRESVIA).
#'       Moderna Phase 3 trial.}
#'     \item{\code{"oa_papirovax"}}{Older-adult RSV subunit vaccine candidate.}
#'   }
#'
#' @return A named list with two tibbles, \code{vaccine} and \code{placebo},
#'   each with columns:
#'   \describe{
#'     \item{\code{tp}}{Integer time-point index (used by the GP covariance).}
#'     \item{\code{t}}{Midpoint time in days post-vaccination/birth.}
#'     \item{\code{person_time}}{Person-days of follow-up in this interval.}
#'     \item{\code{n}}{Number of RSV-LRTI events in this interval.}
#'   }
#'
#' @export
#' @examples
#' dat <- rsv_example("maternal")
#' head(dat$vaccine)
rsv_example <- function(dataset = c("nirsevimab", "maternal",
                                    "oa_gsk", "oa_pfizer")) {
  dataset <- match.arg(dataset)
  switch(dataset,
    nirsevimab  = .nmab_data(),
    maternal    = .maternal_data(),
    oa_gsk      = .oa_gsk_data(),
    oa_pfizer   = .oa_pfizer_data()
  )
}


#' Load trial data from CSV files
#'
#' Reads two CSV files (one per trial arm) and returns a list suitable for
#' passing directly to [fit_efficacy()].
#'
#' Expected CSV columns (other columns are ignored):
#' \describe{
#'   \item{\code{t}}{Midpoint time in days post-vaccination/birth.}
#'   \item{\code{person_time}}{Person-days of follow-up in this interval.}
#'   \item{\code{n}}{Number of events (cases) in this interval.}
#' }
#' The integer time-point index \code{tp} is assigned automatically from row
#' order (i.e. rows must be sorted in chronological order).
#'
#' @param vaccine_csv Path to a CSV file for the **vaccine/treatment** arm.
#' @param placebo_csv Path to a CSV file for the **placebo/control** arm.
#'
#' @return A named list with tibbles \code{vaccine} and \code{placebo}.
#' @export
#' @examples
#' \dontrun{
#' dat <- load_trial_csv("vaccine_arm.csv", "placebo_arm.csv")
#' fit <- fit_efficacy(dat)
#' }
load_trial_csv <- function(vaccine_csv, placebo_csv) {
  required_cols <- c("t", "person_time", "n")

  read_arm <- function(path, arm_name) {  # nolint: unused-arg
    df <- readr::read_csv(path, show_col_types = FALSE)
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      stop(sprintf(
        "CSV '%s' is missing required column(s): %s",
        path, paste(missing, collapse = ", ")
      ), call. = FALSE)
    }
    df$tp <- seq_len(nrow(df))
    tibble::tibble(tp = df$tp, t = df$t,
                   person_time = df$person_time, n = df$n)
  }

  list(
    vaccine = read_arm(vaccine_csv, "vaccine"),
    placebo = read_arm(placebo_csv, "placebo")
  )
}
