#' Plot mean outcomes across countries
#'
#' Creates a horizontal bar chart comparing the mean of an outcome
#' across countries. Countries are ordered by mean outcome.
#'
#' Countries with no non-missing observations for the outcome are
#' excluded from the plot.
#'
#' @param data A data frame.
#' @param country_var A character value giving the name of the variable
#'   identifying countries.
#' @param outcome_var A character value giving the name of the numeric
#'   outcome variable to plot.
#' @param plot_title Optional plot title. If `NULL`, a title is generated
#'   from `outcome_var`.
#' @param y_label Optional label for the outcome axis. If `NULL`, a label
#'   is generated from `outcome_var`.
#'
#' @return A `ggplot` object containing a horizontal bar chart of mean
#'   outcome values by country.
#'
#' @importFrom rlang .data
#'
#' @examples
#' dat <- data.frame(
#'   country = c(
#'     "England", "England",
#'     "Scotland", "Scotland",
#'     "Wales", "Wales"
#'   ),
#'   happiness = c(7, 8, 6, 7, 5, 6)
#' )
#'
#' plot_cross_national(
#'   data = dat,
#'   country_var = "country",
#'   outcome_var = "happiness"
#' )
#'
#' @export
plot_cross_national <- function(data,
                                country_var,
                                outcome_var,
                                plot_title = NULL,
                                y_label = NULL) {

  # Check inputs

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!country_var %in% names(data)) {
    stop("country_var must be a variable in data.")
  }

  if (!outcome_var %in% names(data)) {
    stop("outcome_var must be a variable in data.")
  }


  # Set default labels

  if (is.null(plot_title)) {
    plot_title <- paste("Mean", outcome_var, "by country")
  }

  if (is.null(y_label)) {
    y_label <- paste("Mean", outcome_var)
  }


  # Summarise data

  plot_data <- dplyr::group_by(
    data,
    .data[[country_var]]
  )

  plot_data <- dplyr::summarise(
    plot_data,
    n = sum(!is.na(.data[[outcome_var]])),
    mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
    .groups = "drop"
  )

  plot_data <- dplyr::filter(
    plot_data,
    .data$n > 0
  )

  plot_data <- dplyr::arrange(
    plot_data,
    dplyr::desc(.data$mean_outcome)
  )

  plot_data <- dplyr::mutate(
    plot_data,
    country_ordered = factor(
      .data[[country_var]],
      levels = .data[[country_var]]
    )
  )


  # Create plot

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$country_ordered,
      y = .data$mean_outcome
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = y_label,
      caption = "Countries ordered by mean outcome"
    ) +
    ggplot2::theme_minimal()


  # Return plot

  return(plot)

}
