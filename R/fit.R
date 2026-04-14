# Main fitting function

#' Fit Bayesian waning vaccine efficacy models
#'
#' Fits one or more waning efficacy models to aggregated interval-format trial
#' data using Hamiltonian Monte Carlo via Stan. Incidence in the placebo arm is
#' modelled with a Gaussian process; vaccine efficacy is modelled as either
#' exponential or Erlang-k waning.
#'
#' @param data A named list with tibbles \code{vaccine} and \code{placebo},
#'   as returned by [rsv_example()] or [load_trial_csv()]. Each tibble must
#'   have columns \code{tp} (integer index), \code{t} (time in days),
#'   \code{person_time} (person-days), and \code{n} (event count).
#'
#' @param models Character vector of waning models to fit. Any combination of
#'   \code{"exp"} (exponential), \code{"er2"} (Erlang-2), and/or
#'   \code{"er3"} (Erlang-3). Default: all three.
#'
#' @param bounded Logical. If \code{TRUE} (default \code{FALSE}), use the
#'   model with a user-configurable upper bound on the waning rate
#'   \code{wane_b}; supply the bound via \code{upper_bound_b}. If
#'   \code{FALSE}, the fixed-bound Stan model is used
#'   (\code{wane_b <= 0.1}).
#'
#' @param upper_bound_b Numeric vector of length equal to \code{models}
#'   (recycled if length 1). Upper bound on the waning-rate parameter for the
#'   bounded model. Ignored when \code{bounded = FALSE}.
#'   Default: \code{0.1} for all models.
#'
#' @param output_dir Path to a directory where Stan fit objects (\code{.rda})
#'   are saved. Created if it does not exist. If \code{NULL} (default) a
#'   temporary directory is used.
#'
#' @param chains Number of MCMC chains. Default 4.
#' @param parallel_chains Number of chains to run in parallel. Default 4.
#' @param iter_warmup Warmup iterations per chain. Default 1000.
#' @param iter_sampling Sampling iterations per chain. Default 1000.
#' @param seed Random seed for reproducibility. Default 123.
#' @param refresh How often to print sampling progress (iterations). Default 500.
#' @param ... Additional arguments forwarded to \code{cmdstanr::CmdStanModel$sample()}.
#'
#' @return An object of class \code{"wanefit"}, which is a named list with:
#' \describe{
#'   \item{\code{fits}}{Named list of CmdStan fit objects (one per model).}
#'   \item{\code{data}}{The input data list.}
#'   \item{\code{models}}{Character vector of fitted model IDs.}
#'   \item{\code{bounded}}{Logical, as specified.}
#'   \item{\code{output_dir}}{Path where fit objects were saved.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' dat <- rsv_example("maternal")
#' fit <- fit_efficacy(dat)
#' plot_waning(fit)
#' }
fit_efficacy <- function(
    data,
    models         = c("exp", "er2", "er3"),
    bounded        = FALSE,
    upper_bound_b  = 0.1,
    output_dir     = NULL,
    chains         = 4,
    parallel_chains = 4,
    iter_warmup    = 1000,
    iter_sampling  = 1000,
    seed           = 123,
    refresh        = 500,
    ...
) {
  # ── Input validation ────────────────────────────────────────────────────────
  models <- match.arg(models, c("exp", "er2", "er3"), several.ok = TRUE)

  required_cols <- c("tp", "t", "person_time", "n")
  for (arm in c("vaccine", "placebo")) {
    if (!arm %in% names(data)) {
      stop(sprintf("'data' must have element '%s'.", arm), call. = FALSE)
    }
    missing <- setdiff(required_cols, names(data[[arm]]))
    if (length(missing) > 0) {
      stop(sprintf("data$%s is missing column(s): %s",
                   arm, paste(missing, collapse = ", ")), call. = FALSE)
    }
  }

  # ── Output directory ────────────────────────────────────────────────────────
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "wanefit_outputs")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ── Stan model ──────────────────────────────────────────────────────────────
  # On shinyapps.io:
  #   - auto_write=TRUE tries to write an .rds cache next to the Stan file;
  #     the app dir is read-only → "invalid connection".
  #   - Even with tempdir() copies, the auto_write path lookup can still hit
  #     the read-only dir.
  # Fix: disable auto_write AND load the Stan code as a *string* (model_code)
  # so rstan never touches any source-adjacent file paths at all.
  rstan::rstan_options(auto_write = FALSE)
  options(mc.cores = 1L)

  stan_dir <- file.path(getwd(), "stan")
  if (!dir.exists(stan_dir)) stan_dir <- file.path(normalizePath("."), "stan")

  stan_src <- if (bounded) {
    file.path(stan_dir, "eff_est.stan")
  } else {
    file.path(stan_dir, "eff_est_none.stan")
  }

  if (!file.exists(stan_src)) {
    stop(sprintf("Stan model not found: %s", stan_src), call. = FALSE)
  }

  # Read the Stan source into a string. Using model_code= instead of file=
  # means rstan never tries to write a cache file anywhere — no file
  # connections opened during compilation.
  stan_code <- paste(readLines(stan_src, warn = FALSE), collapse = "\n")

  message("Compiling Stan model from string...")
  mod <- suppressWarnings(
    rstan::stan_model(model_code = stan_code, verbose = FALSE,
                      auto_write = FALSE)
  )
  message("Compilation complete.")

  pla <- data$placebo
  vac <- data$vaccine

  base_data <- list(
    N_pla       = nrow(pla),
    tp_pla      = as.integer(pla$tp),
    t_pla       = as.numeric(pla$t),
    persons_pla = as.numeric(pla$person_time),
    n_pla       = as.integer(pla$n),

    N_vac       = nrow(vac),
    tp_vac      = as.integer(vac$tp),
    t_vac       = as.numeric(vac$t),
    persons_vac = as.numeric(vac$person_time),
    n_vac       = as.integer(vac$n)
  )

  model_ids <- c(exp = 1L, er2 = 2L, er3 = 3L)

  # Recycle upper_bound_b to length of models
  upper_bound_b <- rep_len(upper_bound_b, length(models))
  names(upper_bound_b) <- models

  # shinyapps.io blocks both forked (mclapply) and socket (PSOCK) parallel
  # processes, which causes rstan "invalid connection" errors even with cores=1
  # because rstan still sets up connection infrastructure for multi-chain runs.
  # Forcing chains=1 removes ALL parallel machinery; cores=1 is belt-and-braces.
  is_shiny <- tryCatch(shiny::isRunning(), error = function(e) FALSE)
  if (is_shiny) {
    chains  <- 1L
    n_cores <- 1L
  } else {
    n_cores <- 1L
  }

  # ── Fit each model ──────────────────────────────────────────────────────────
  fits <- list()

  for (m in models) {
    message(sprintf("\nFitting model: %s", toupper(m)))

    stan_data <- c(base_data, list(model_id = model_ids[[m]]))
    if (bounded) {
      stan_data$lower_bound <- upper_bound_b[[m]]
    }

    # Wrap sampling in capture.output + suppressMessages so rstan's C++ code
    # gets a fresh, valid text connection for stdout. show_messages=FALSE
    # prevents Stan adaptation messages from trying to write through R's
    # (Shiny-redirected) output connections.
    fit <- local({
      f <- NULL
      capture.output(
        f <- suppressMessages(
          rstan::sampling(
            mod,
            data          = stan_data,
            seed          = seed,
            chains        = chains,
            cores         = n_cores,
            warmup        = iter_warmup,
            iter          = iter_warmup + iter_sampling,
            refresh       = 0L,
            open_progress = FALSE,
            verbose       = FALSE,
            show_messages = FALSE
          )
        ),
        type = "output"
      )
      f
    })

    fits[[m]] <- fit
  }

  structure(
    list(
      fits       = fits,
      data       = data,
      models     = models,
      bounded    = bounded,
      output_dir = output_dir
    ),
    class = "wanefit"
  )
}


#' @export
print.wanefit <- function(x, ...) {
  cat("── wanefit object ─────────────────────────────────────\n")
  cat(sprintf("  Models fitted : %s\n", paste(toupper(x$models), collapse = ", ")))
  cat(sprintf("  Bounded wane_b: %s\n", x$bounded))
  cat(sprintf("  Output dir    : %s\n", x$output_dir))

  n_intervals <- nrow(x$data$placebo)
  t_max <- max(x$data$placebo$t)
  cat(sprintf("  Intervals     : %d  |  Max follow-up: %.0f days\n",
              n_intervals, t_max))

  for (m in x$models) {
    diag <- tryCatch(
      x$fits[[m]]$diagnostic_summary(quiet = TRUE),
      error = function(e) NULL
    )
    if (!is.null(diag)) {
      n_div <- sum(diag$num_divergent)
      rhat  <- round(max(x$fits[[m]]$summary()$rhat, na.rm = TRUE), 3)
      cat(sprintf("  [%s] divergences: %d  |  max Rhat: %.3f\n",
                  toupper(m), n_div, rhat))
    }
  }
  invisible(x)
}
