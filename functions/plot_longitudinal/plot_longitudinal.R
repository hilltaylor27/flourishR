# plot_longitudinal.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Plot wellbeing trajectories over waves


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
  
  plot_summary <- data %>%
    dplyr::group_by(.data[[wave_var]]) %>%
    dplyr::summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
      sd_outcome = stats::sd(.data[[outcome_var]], na.rm = TRUE),
      se_outcome = sd_outcome / sqrt(n),
      .groups = "drop"
    ) %>%
    dplyr::filter(n > 0)
  
  
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
        y = mean_outcome,
        group = 1
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = plot_summary,
      ggplot2::aes(
        x = .data[[wave_var]],
        y = mean_outcome
      ),
      size = 2
    ) +
    ggplot2::geom_errorbar(
      data = plot_summary,
      ggplot2::aes(
        x = .data[[wave_var]],
        ymin = mean_outcome - se_outcome,
        ymax = mean_outcome + se_outcome
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