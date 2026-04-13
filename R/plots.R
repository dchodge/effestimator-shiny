# FT-style visualisation of waning efficacy fits

#' Plot waning vaccine efficacy curves
#'
#' Plots posterior median and 95% credible interval of waning efficacy over
#' time for each fitted waning model, using the Financial Times-inspired
#' chart theme.
#'
#' @param x A \code{"wanefit"} object returned by [fit_efficacy()].
#' @param models Character vector of model IDs to include. Defaults to the
#'   models present in \code{x}.
#' @param days_max Maximum number of days shown on the x-axis. Defaults to
#'   the maximum of 365 and twice the observed follow-up.
#' @param ci Credible interval width. Default \code{0.95}.
#' @param title Plot title. A sensible default is provided.
#' @param subtitle Plot subtitle. A sensible default is provided.
#' @param caption Caption (e.g. data source). Default \code{NULL}.
#' @param pct Logical. If \code{TRUE} (default), express efficacy as a
#'   percentage (0–100); otherwise express as a proportion (0–1).
#'
#' @return A \code{ggplot2} object.
#' @export
#' @examples
#' \dontrun{
#' dat <- rsv_example("maternal")
#' fit <- fit_efficacy(dat)
#' plot_waning(fit)
#' }
plot_waning <- function(
    x,
    models   = NULL,
    days_max = NULL,
    ci       = 0.95,
    title    = "Waning vaccine efficacy",
    subtitle = sprintf("Posterior median and %d%% credible interval", round(ci * 100)),
    caption  = NULL,
    pct      = TRUE
) {
  stopifnot(inherits(x, "wanefit"))

  models <- models %||% x$models
  models <- intersect(models, x$models)
  if (length(models) == 0) stop("No valid models specified.", call. = FALSE)

  cols   <- ft_colours()
  labels <- ft_model_labels()

  t_max_data <- max(x$data$placebo$t)
  if (is.null(days_max)) days_max <- max(365, 2 * t_max_data)

  draws_list <- lapply(models, function(m) {
    tidybayes::spread_draws(posterior::as_draws_df(x$fits[[m]]), waning[t]) |>
      dplyr::group_by(t) |>
      dplyr::summarise(
        median = stats::median(waning),
        lb     = stats::quantile(waning, (1 - ci) / 2),
        ub     = stats::quantile(waning, 1 - (1 - ci) / 2),
        .groups = "drop"
      ) |>
      dplyr::mutate(model = m, t = t - 1L)   # Stan indexes from 1
  })
  dat <- dplyr::bind_rows(draws_list) |>
    dplyr::filter(.data$t <= days_max) |>
    dplyr::mutate(model = factor(.data$model, levels = models))

  scale <- if (pct) 100 else 1
  dat   <- dplyr::mutate(dat,
    dplyr::across(c(median, lb, ub), ~ .x * scale)
  )
  y_lab <- if (pct) "Efficacy (%)" else "Efficacy (proportion)"

  used_cols   <- cols[models]
  used_labels <- labels[models]

  ggplot2::ggplot(dat, ggplot2::aes(x = t, colour = model, fill = model)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lb, ymax = ub),
      alpha = 0.18, colour = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median),
      linewidth = 1.1
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#9E9B93", linewidth = 0.4,
                        linetype = "dashed") +
    ggplot2::scale_colour_manual(
      values = used_cols, labels = used_labels, name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = used_cols, labels = used_labels, name = NULL
    ) +
    ggplot2::scale_y_continuous(
      limits = c(NA, if (pct) 100 else 1),
      labels = if (pct) function(x) paste0(x, "%") else scales::percent
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      limits = c(0, days_max)
    ) +
    ggplot2::labs(
      x        = "Days post-vaccination",
      y        = y_lab,
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    theme_ft()
}


#' Plot fitted incidence curves against observed data
#'
#' Plots the Gaussian-process-estimated incidence in the placebo arm overlaid
#' on the observed incidence rates per interval. Uses the exponential waning
#' fit (whose GP draws are indicative of all models).
#'
#' @param x A \code{"wanefit"} object.
#' @param model Which model's GP fit to display. Default \code{"exp"}.
#' @param ci Credible interval width. Default \code{0.95}.
#' @param title Plot title.
#' @param caption Caption text (e.g. data source). Default \code{NULL}.
#'
#' @return A \code{ggplot2} object.
#' @export
#' @examples
#' \dontrun{
#' dat <- rsv_example("nirsevimab")
#' fit <- fit_efficacy(dat)
#' plot_incidence(fit)
#' }
plot_incidence <- function(
    x,
    model   = "exp",
    ci      = 0.95,
    title   = "Fitted vs. observed incidence (placebo arm)",
    caption = NULL
) {
  stopifnot(inherits(x, "wanefit"))

  if (!model %in% x$models) {
    model <- x$models[1]
    message(sprintf("Requested model not found; using '%s'.", model))
  }

  pla <- x$data$placebo
  vac <- x$data$vaccine

  obs_pla <- data.frame(
    t    = pla$t,
    rate = pla$n / pla$person_time
  )
  obs_vac <- data.frame(
    t    = vac$t,
    rate = vac$n / vac$person_time
  )

  draws <- posterior::as_draws_df(x$fits[[model]])

  fit_pla <- tidybayes::spread_draws(draws, incidence_pla[i], time_p[i]) |>
    dplyr::group_by(time_p) |>
    dplyr::summarise(
      median = stats::median(incidence_pla),
      lb     = stats::quantile(incidence_pla, (1 - ci) / 2),
      ub     = stats::quantile(incidence_pla, 1 - (1 - ci) / 2),
      .groups = "drop"
    ) |>
    dplyr::rename(t = time_p)

  fit_vac <- tidybayes::spread_draws(draws, incidence_vac[i], time_p[i]) |>
    dplyr::group_by(time_p) |>
    dplyr::summarise(
      median = stats::median(incidence_vac),
      lb     = stats::quantile(incidence_vac, (1 - ci) / 2),
      ub     = stats::quantile(incidence_vac, 1 - (1 - ci) / 2),
      .groups = "drop"
    ) |>
    dplyr::rename(t = time_p)

  cols <- ft_colours()

  p_pla <- ggplot2::ggplot(fit_pla, ggplot2::aes(x = t)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lb, ymax = ub),
                         fill = cols["placebo"], alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = cols["placebo"], linewidth = 1) +
    ggplot2::geom_point(data = obs_pla,
                        ggplot2::aes(x = t, y = rate),
                        shape = 21, fill = cols["placebo"],
                        colour = "white", size = 2.5, stroke = 0.8) +
    ggplot2::labs(x = "Days post-vaccination", y = "Incidence rate",
                  title = title, subtitle = "Placebo", caption = caption) +
    theme_ft()

  p_vac <- ggplot2::ggplot(fit_vac, ggplot2::aes(x = t)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lb, ymax = ub),
                         fill = cols["vaccine"], alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = cols["vaccine"], linewidth = 1) +
    ggplot2::geom_point(data = obs_vac,
                        ggplot2::aes(x = t, y = rate),
                        shape = 21, fill = cols["vaccine"],
                        colour = "white", size = 2.5, stroke = 0.8) +
    ggplot2::labs(x = "Days post-vaccination", y = "Incidence rate",
                  subtitle = "Vaccine") +
    theme_ft()

  patchwork::wrap_plots(p_pla, p_vac, ncol = 2)
}


#' Combined waning + incidence diagnostic plot
#'
#' Combines [plot_waning()] and [plot_incidence()] into a single two-panel
#' figure. Useful for a quick model-fit summary.
#'
#' @param x A \code{"wanefit"} object.
#' @param ... Arguments passed to [plot_waning()].
#'
#' @return A \code{patchwork} object.
#' @export
plot_summary <- function(x, ...) {
  p_wane <- plot_waning(x, ...)
  p_inci <- plot_incidence(x)
  patchwork::wrap_plots(p_inci, p_wane, ncol = 1, heights = c(1, 1.2))
}


# ── Internal null-coalescing operator ─────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b
