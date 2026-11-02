#' Plot longitudinal outcome trajectories
#'
#' Plots mean outcome values across waves with standard error bars.
#' Individual participant trajectories can optionally be displayed
#' behind the mean trajectory.
#'
#' Wave means and standard errors are calculated using non-missing
#' observations for the outcome.
#'
#' @param data A data frame containing longitudinal data.
#' @param id_var A character value giving the name of the participant
#'   identifier variable.
#' @param wave_var A character value giving the name of the wave or
#'   time variable.
#' @param outcome_var A character value giving the name of the numeric
#'   outcome variable.
#' @param show_individuals Logical. If `TRUE`, individual participant
#'   trajectories are displayed. Defaults to `FALSE`.
#' @param plot_title Optional plot title. If `NULL`, a title is generated
#'   from `outcome_var`.
#' @param y_label Optional label for the outcome axis. If `NULL`, a label
#'   is generated from `outcome_var`.
#'
#' @return A `ggplot` object containing the longitudinal trajectory plot.
#'
#' @importFrom rlang .data
#'
#' @examples
#' dat <- data.frame(
#'   id = rep(1:3, each = 3),
#'   wave = rep(1:3, times = 3),
#'   happiness = c(
#'     6, 7, 8,
#'     5, 6, 7,
#'     7, 7, 8
#'   )
#' )
#'
#' plot_longitudinal(
#'   data = dat,
#'   id_var = "id",
#'   wave_var = "wave",
#'   outcome_var = "happiness"
#' )
#'
#' @export
plot_longitudinal <- function(data,
                              id_var,
                              wave_var,
                              outcome_var,
                              show_individuals = FALSE,
                              plot_title = NULL,
                              y_label = NULL) {

  # Check inputs

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!id_var %in% names(data)) {
    stop("id_var must be a variable in data.")
  }

  if (!wave_var %in% names(data)) {
    stop("wave_var must be a variable in data.")
  }

  if (!outcome_var %in% names(data)) {
    stop("outcome_var must be a variable in data.")
  }

  if (!is.logical(show_individuals)) {
    stop("show_individuals must be TRUE or FALSE.")
  }


  # Set default labels

  if (is.null(plot_title)) {
    plot_title <- paste("Mean", outcome_var, "over waves")
  }

  if (is.null(y_label)) {
    y_label <- paste("Mean", outcome_var)
  }


  # Summarise data by wave

  plot_summary <- dplyr::group_by(
    data,
    .data[[wave_var]]
  )

  plot_summary <- dplyr::summarise(
    plot_summary,
    n = sum(!is.na(.data[[outcome_var]])),
    mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
    sd_outcome = stats::sd(.data[[outcome_var]], na.rm = TRUE),
    se_outcome = .data$sd_outcome / sqrt(.data$n),
    .groups = "drop"
  )

  plot_summary <- dplyr::filter(
    plot_summary,
    .data$n > 0
  )


  # Create base plot

  plot <- ggplot2::ggplot()


  # Add individual trajectories if requested

  if (show_individuals == TRUE) {

    plot <- plot +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes(
          x = .data[[wave_var]],
          y = .data[[outcome_var]],
          group = .data[[id_var]]
        ),
        alpha = 0.20
      )

  }


  # Add mean trajectory

  plot <- plot +
    ggplot2::geom_line(
      data = plot_summary,
      ggplot2::aes(
        x = .data[[wave_var]],
        y = .data$mean_outcome,
        group = 1
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = plot_summary,
      ggplot2::aes(
        x = .data[[wave_var]],
        y = .data$mean_outcome
      ),
      size = 2
    ) +
    ggplot2::geom_errorbar(
      data = plot_summary,
      ggplot2::aes(
        x = .data[[wave_var]],
        ymin = .data$mean_outcome - .data$se_outcome,
        ymax = .data$mean_outcome + .data$se_outcome
      ),
      width = 0.10
    ) +
    ggplot2::labs(
      title = plot_title,
      x = "Wave",
      y = y_label,
      caption = "Points show wave means; error bars show standard errors"
    ) +
    ggplot2::theme_minimal()


  # Return plot

  return(plot)

}
