# plot_cross_national.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Compare wellbeing outcomes across countries


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
  
  plot_data <- data %>%
    dplyr::group_by(.data[[country_var]]) %>%
    dplyr::summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n > 0) %>%
    dplyr::arrange(dplyr::desc(mean_outcome)) %>%
    dplyr::mutate(
      country_ordered = factor(.data[[country_var]], levels = .data[[country_var]])
    )
  
  
  # Create plot
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = country_ordered,
      y = mean_outcome
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