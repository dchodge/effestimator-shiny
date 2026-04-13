# R/helpers.R
# Auto-sourced by Shiny at startup.

# ── Dependency check ───────────────────────────────────────────────────────────
.check_deps <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "cmdstanr is not installed.\n",
      "Install with: install.packages('cmdstanr', repos = 'https://mc-stan.org/r-packages/')",
      call. = FALSE
    )
  }

  # Check CmdStan is configured
  tryCatch(
    cmdstanr::cmdstan_version(error_on_NA = TRUE),
    error = function(e) {
      stop(
        "CmdStan is not installed or not found.\n",
        "Run: cmdstanr::install_cmdstan()",
        call. = FALSE
      )
    }
  )

  invisible(TRUE)
}

.check_deps()

# ── Colour palette (mirrors effestimatorfull) ──────────────────────────────────
PALETTE <- c(
  exp     = "#0D7680",
  er2     = "#990F3D",
  er3     = "#593380",
  placebo = "#6E6259",
  vaccine = "#FF8833"
)
