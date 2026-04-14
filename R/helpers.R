# R/helpers.R
# Auto-sourced by Shiny at startup.

# ── Dependency check (non-fatal — warn only so the app still loads) ──────────
.check_deps <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    warning(
      "rstan is not installed. Stan model fitting will not work.\n",
      "Install with: install.packages('rstan')",
      call. = FALSE
    )
  }
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
