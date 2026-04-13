# Financial Times-inspired ggplot2 theme and colour palette

#' Financial Times-inspired ggplot2 theme
#'
#' A clean, minimal chart theme inspired by the Financial Times data
#' visualisation style: white background, horizontal gridlines only, no chart
#' frame, and concise typography.
#'
#' @param base_size Base font size in points. Default 12.
#' @param base_family Base font family. Default \code{""} (system sans-serif).
#'
#' @return A [ggplot2::theme()] object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ft()
theme_ft <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Canvas
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),

      # Grid – horizontal lines only, soft grey
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E9E4D9", linewidth = 0.4),
      panel.grid.minor.y = ggplot2::element_blank(),

      # Axes
      axis.line.x      = ggplot2::element_line(colour = "#9E9B93", linewidth = 0.5),
      axis.line.y      = ggplot2::element_blank(),
      axis.ticks.x     = ggplot2::element_line(colour = "#9E9B93", linewidth = 0.4),
      axis.ticks.y     = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      axis.text        = ggplot2::element_text(colour = "#4A4038", size = ggplot2::rel(0.85)),
      axis.title       = ggplot2::element_text(colour = "#4A4038", size = ggplot2::rel(0.90)),

      # Legend
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.background    = ggplot2::element_blank(),
      legend.key           = ggplot2::element_blank(),
      legend.text          = ggplot2::element_text(colour = "#4A4038", size = ggplot2::rel(0.85)),
      legend.title         = ggplot2::element_text(colour = "#4A4038", size = ggplot2::rel(0.85),
                                                   face = "bold"),
      legend.margin        = ggplot2::margin(t = 4, b = 0),

      # Titles
      plot.title    = ggplot2::element_text(colour = "#33302E", size = ggplot2::rel(1.15),
                                            face = "bold", hjust = 0,
                                            margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(colour = "#4A4038", size = ggplot2::rel(0.95),
                                            hjust = 0,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption  = ggplot2::element_text(colour = "#9E9B93", size = ggplot2::rel(0.78),
                                            hjust = 0,
                                            margin = ggplot2::margin(t = 8)),
      plot.margin   = ggplot2::margin(12, 12, 8, 12),

      # Facets
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(colour = "#33302E", face = "bold",
                                               size = ggplot2::rel(0.90))
    )
}


# ── Colour palette ────────────────────────────────────────────────────────────

#' FT-inspired colour palette
#'
#' Returns a named vector of hex colours for use in waning efficacy plots.
#'
#' \describe{
#'   \item{exp}{Exponential waning – FT teal}
#'   \item{er2}{Erlang-2 waning – FT claret}
#'   \item{er3}{Erlang-3 waning – FT purple}
#'   \item{placebo}{Placebo incidence – FT slate}
#'   \item{vaccine}{Vaccine incidence – FT orange}
#' }
#'
#' @return Named character vector of hex colour codes.
#' @export
ft_colours <- function() {
  c(
    exp     = "#0D7680",   # teal
    er2     = "#990F3D",   # claret
    er3     = "#593380",   # purple
    placebo = "#6E6259",   # slate
    vaccine = "#FF8833"    # warm orange
  )
}

#' FT-inspired model display names
#'
#' Returns a named character vector mapping internal model IDs to readable
#' labels suitable for plot legends.
#'
#' @return Named character vector.
#' @keywords internal
ft_model_labels <- function() {
  c(exp = "Exponential", er2 = "Erlang-2", er3 = "Erlang-3")
}
