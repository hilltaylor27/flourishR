# develop_score_swemwbs.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-12
# Purpose: Develop and test score_swemwbs()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  sw1 = c(1, 1, 1, NA, 5, 5),
  sw2 = c(2, 2, 2, NA, 5, 5),
  sw3 = c(3, 3, 3, NA, 5, 5),
  sw4 = c(4, 4, 4, NA, 5, 5),
  sw5 = c(5, 5, NA, NA, 5, 5),
  sw6 = c(1, 1, NA, NA, 5, 5),
  sw7 = c(2, 2, NA, NA, 5, 5)
)


# Inspect test data

test_data


# Create expected scores by hand

expected_means <- c(
  2.571429,
  2.571429,
  NA,
  NA,
  5,
  5
)

expected_sums <- c(
  18,
  18,
  NA,
  NA,
  35,
  35
)


# Create score_swemwbs function

score_swemwbs <- function(data,
                          items,
                          min_items = 5,
                          sum_name = "swemwbs_sum",
                          mean_name = "swemwbs_mean",
                          n_valid_name = "swemwbs_n_valid") {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (length(items) != 7) {
    stop("SWEMWBS requires exactly 7 items.")
  }
  
  missing_items <- items[!items %in% names(data)]
  
  if (length(missing_items) > 0) {
    stop(
      "Missing variables: ",
      paste(missing_items, collapse = ", ")
    )
  }
  
  if (min_items < 1 | min_items > 7) {
    stop("min_items must be between 1 and 7.")
  }
  
  
  # Extract items
  
  item_data <- data[, items]
  
  
  # Check item range
  
  invalid_values <- unlist(item_data)
  invalid_values <- invalid_values[!is.na(invalid_values)]
  invalid_values <- invalid_values[!invalid_values %in% 1:5]
  
  if (length(invalid_values) > 0) {
    warning(
      "Some SWEMWBS values fall outside the expected range of 1 to 5."
    )
  }
  
  
  # Count valid responses
  
  n_valid <- rowSums(!is.na(item_data))
  
  
  # Calculate mean score
  
  item_mean <- rowMeans(item_data, na.rm = TRUE)
  
  
  # Calculate prorated sum score
  
  item_sum <- item_mean * 7
  
  
  # Apply minimum item rule
  
  item_mean[n_valid < min_items] <- NA
  item_sum[n_valid < min_items] <- NA
  
  
  # Add scored variables to dataset
  
  data[[n_valid_name]] <- n_valid
  data[[mean_name]] <- item_mean
  data[[sum_name]] <- item_sum
  
  
  # Return data
  
  return(data)
  
}


# Run function on test data

scored_test_data <- score_swemwbs(
  data = test_data,
  items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
)


# Inspect scored data

scored_test_data


# Check means against expected means

scored_test_data$expected_mean <- expected_means

scored_test_data$mean_check <- round(scored_test_data$swemwbs_mean, 6) == round(scored_test_data$expected_mean, 6)

scored_test_data$mean_check[
  is.na(scored_test_data$swemwbs_mean) &
    is.na(scored_test_data$expected_mean)
] <- TRUE


# Check sums against expected sums

scored_test_data$expected_sum <- expected_sums

scored_test_data$sum_check <- scored_test_data$swemwbs_sum == scored_test_data$expected_sum

scored_test_data$sum_check[
  is.na(scored_test_data$swemwbs_sum) &
    is.na(scored_test_data$expected_sum)
] <- TRUE


# Inspect checks

scored_test_data


# Check whether all tests passed

all(scored_test_data$mean_check)

all(scored_test_data$sum_check)


# Test different minimum item rule

scored_test_data_min4 <- score_swemwbs(
  data = test_data,
  items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7"),
  min_items = 4
)

scored_test_data_min4


# Test missing variable error

try(
  score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "missing_variable")
  )
)


# Test too few items error

try(
  score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3")
  )
)