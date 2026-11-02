# develop_plot_cross_national.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test plot_cross_national()


# Load packages

library(tidyverse)


# Simulate UKHLS-style test data

ukhls_test_data <- data.frame(
  id = 1:40,
  country = rep(
    c("England", "Scotland", "Wales", "Northern Ireland"),
    each = 10
  ),
  life_satisfaction = c(
    7, 8, 6, 7, 8, 7, 6, 8, NA, 7,
    6, 7, 6, 7, NA, 6, 7, 6, 6, 7,
    5, 6, 5, NA, 6, 5, 6, 5, 6, 5,
    8, 8, 7, 8, 7, NA, 8, 7, 8, 8
  )
)


# Simulate GFS-style test data

gfs_test_data <- data.frame(
  id = 1:66,
  country = rep(
    c(
      "Argentina", "Australia", "Brazil", "Canada", "Chile", "China",
      "Egypt", "France", "Germany", "India", "Indonesia", "Italy",
      "Japan", "Kenya", "Mexico", "Netherlands", "Nigeria", "Poland",
      "South Africa", "Spain", "United Kingdom", "United States"
    ),
    each = 3
  ),
  happiness = c(
    6, 7, NA,
    7, 8, 7,
    6, 6, 7,
    8, 8, 7,
    6, 7, 6,
    7, NA, 7,
    5, 6, 5,
    6, 7, 7,
    7, 8, 7,
    7, 7, NA,
    6, 6, 6,
    6, 7, 6,
    5, 6, 6,
    6, NA, 7,
    7, 7, 6,
    7, 8, 8,
    6, 7, 6,
    6, 6, NA,
    6, 7, 6,
    6, 7, 7,
    7, 7, 8,
    7, 8, 8
  )
)


# Inspect test data

ukhls_test_data

gfs_test_data


# Create plot_cross_national function

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
    group_by(.data[[country_var]]) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n > 0) %>%
    arrange(desc(mean_outcome)) %>%
    mutate(
      country_ordered = factor(.data[[country_var]], levels = .data[[country_var]])
    )
  
  
  # Create plot
  
  plot <- ggplot(
    plot_data,
    aes(
      x = country_ordered,
      y = mean_outcome
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = plot_title,
      x = NULL,
      y = y_label,
      caption = "Countries ordered by mean outcome"
    ) +
    theme_minimal()
  
  
  # Return plot
  
  return(plot)
  
}


# Test with UKHLS-style data

ukhls_plot <- plot_cross_national(
  data = ukhls_test_data,
  country_var = "country",
  outcome_var = "life_satisfaction",
  plot_title = "Mean life satisfaction by UK country",
  y_label = "Mean life satisfaction"
)

ukhls_plot


# Test with GFS-style data

gfs_plot <- plot_cross_national(
  data = gfs_test_data,
  country_var = "country",
  outcome_var = "happiness",
  plot_title = "Mean happiness by country",
  y_label = "Mean happiness"
)

gfs_plot


# Test missing country variable error

try(
  plot_cross_national(
    data = ukhls_test_data,
    country_var = "missing_country",
    outcome_var = "life_satisfaction"
  )
)


# Test missing outcome variable error

try(
  plot_cross_national(
    data = ukhls_test_data,
    country_var = "country",
    outcome_var = "missing_outcome"
  )
)