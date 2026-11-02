# develop_plot_longitudinal.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test plot_longitudinal()


# Load packages

library(tidyverse)


# Simulate UKHLS-style test data

ukhls_long_test_data <- data.frame(
  id = rep(1:8, each = 4),
  wave = rep(1:4, times = 8),
  country = rep(
    c("England", "Scotland", "Wales", "Northern Ireland"),
    each = 8
  ),
  life_satisfaction = c(
    6, 7, 7, 8,
    5, 6, 6, 7,
    7, 7, 8, 8,
    6, NA, 7, 7,
    5, 5, 6, 6,
    6, 6, NA, 7,
    7, 8, 8, 9,
    6, 7, 7, NA
  )
)


# Simulate GFS-style test data

gfs_long_test_data <- data.frame(
  participant_id = rep(1:10, each = 3),
  wave = rep(1:3, times = 10),
  country = rep(
    c("Canada", "United States", "United Kingdom", "Australia", "Brazil"),
    each = 6
  ),
  happiness = c(
    6, 7, 7,
    7, 7, 8,
    6, 6, 7,
    7, NA, 8,
    5, 6, 6,
    6, 7, 7,
    7, 8, 8,
    6, 7, NA,
    6, 6, 7,
    7, 7, 8
  )
)


# Inspect test data

ukhls_long_test_data

gfs_long_test_data


# Create plot_longitudinal function

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


# Test with UKHLS-style data

ukhls_long_plot <- plot_longitudinal(
  data = ukhls_long_test_data,
  id_var = "id",
  wave_var = "wave",
  outcome_var = "life_satisfaction",
  plot_title = "Mean life satisfaction over waves",
  y_label = "Mean life satisfaction"
)

ukhls_long_plot


# Test with individual trajectories

ukhls_long_plot_individuals <- plot_longitudinal(
  data = ukhls_long_test_data,
  id_var = "id",
  wave_var = "wave",
  outcome_var = "life_satisfaction",
  show_individuals = TRUE,
  plot_title = "Life satisfaction trajectories over waves",
  y_label = "Life satisfaction"
)

ukhls_long_plot_individuals


# Test with GFS-style data

gfs_long_plot <- plot_longitudinal(
  data = gfs_long_test_data,
  id_var = "participant_id",
  wave_var = "wave",
  outcome_var = "happiness",
  plot_title = "Mean happiness over waves",
  y_label = "Mean happiness"
)

gfs_long_plot


# Check wave summary manually

ukhls_long_summary <- ukhls_long_test_data %>%
  dplyr::group_by(wave) %>%
  dplyr::summarise(
    n = sum(!is.na(life_satisfaction)),
    mean_life_satisfaction = mean(life_satisfaction, na.rm = TRUE),
    .groups = "drop"
  )

ukhls_long_summary


# Test missing id variable error

try(
  plot_longitudinal(
    data = ukhls_long_test_data,
    id_var = "missing_id",
    wave_var = "wave",
    outcome_var = "life_satisfaction"
  )
)


# Test missing wave variable error

try(
  plot_longitudinal(
    data = ukhls_long_test_data,
    id_var = "id",
    wave_var = "missing_wave",
    outcome_var = "life_satisfaction"
  )
)


# Test missing outcome variable error

try(
  plot_longitudinal(
    data = ukhls_long_test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "missing_outcome"
  )
)


# Test show_individuals error

try(
  plot_longitudinal(
    data = ukhls_long_test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "life_satisfaction",
    show_individuals = "yes"
  )
)