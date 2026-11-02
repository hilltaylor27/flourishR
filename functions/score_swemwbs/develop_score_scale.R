# develop_score_scale.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-12
# Purpose: Develop and test score_scale()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  item1 = c(1, 1, 1, NA, 5, 5),
  item2 = c(2, 2, 2, NA, 5, 5),
  item3 = c(3, 3, 3, NA, 5, 5),
  item4 = c(4, 4, 4, NA, 5, 5),
  item5 = c(5, 5, NA, NA, 5, 5)
)


# Inspect test data

test_data


# Create expected scores by hand

expected_n_valid <- c(5, 5, 4, 0, 5, 5)

expected_mean <- c(
  3,
  3,
  2.5,
  NA,
  5,
  5
)

expected_sum <- c(
  15,
  15,
  NA,
  NA,
  25,
  25
)

expected_prorated_sum <- c(
  15,
  15,
  12.5,
  NA,
  25,
  25
)


# Create score_scale function

score_scale <- function(data,
                        items,
                        min_items = NULL,
                        valid_range = NULL,
                        score_prefix = "scale") {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (length(items) < 2) {
    stop("items must include at least 2 variables.")
  }
  
  missing_items <- items[!items %in% names(data)]
  
  if (length(missing_items) > 0) {
    stop(
      "Missing variables: ",
      paste(missing_items, collapse = ", ")
    )
  }
  
  if (is.null(min_items)) {
    min_items <- length(items)
  }
  
  if (min_items < 1 | min_items > length(items)) {
    stop("min_items must be between 1 and the number of items.")
  }
  
  
  # Extract items
  
  item_data <- data[, items]
  
  
  # Check valid range
  
  if (!is.null(valid_range)) {
    
    invalid_values <- unlist(item_data)
    invalid_values <- invalid_values[!is.na(invalid_values)]
    invalid_values <- invalid_values[
      invalid_values < min(valid_range) |
        invalid_values > max(valid_range)
    ]
    
    if (length(invalid_values) > 0) {
      warning(
        "Some values fall outside the expected range."
      )
    }
    
  }
  
  
  # Count valid responses
  
  n_valid <- rowSums(!is.na(item_data))
  
  
  # Calculate scores
  
  item_mean <- rowMeans(item_data, na.rm = TRUE)
  
  item_sum <- rowSums(item_data, na.rm = TRUE)
  
  prorated_sum <- item_mean * length(items)
  
  
  # Apply minimum item rule
  
  item_mean[n_valid < min_items] <- NA
  item_sum[n_valid < min_items] <- NA
  prorated_sum[n_valid < min_items] <- NA
  
  
  # Fix rows with no valid responses
  
  item_mean[n_valid == 0] <- NA
  item_sum[n_valid == 0] <- NA
  prorated_sum[n_valid == 0] <- NA
  
  
  # Add scored variables to dataset
  
  data[[paste0(score_prefix, "_n_valid")]] <- n_valid
  data[[paste0(score_prefix, "_mean")]] <- item_mean
  data[[paste0(score_prefix, "_sum")]] <- item_sum
  data[[paste0(score_prefix, "_prorated_sum")]] <- prorated_sum
  
  
  # Return data
  
  return(data)
  
}


# Run function on test data

scored_test_data <- score_scale(
  data = test_data,
  items = c("item1", "item2", "item3", "item4", "item5"),
  min_items = 5,
  valid_range = c(1, 5),
  score_prefix = "test_scale"
)


# Inspect scored data

scored_test_data


# Add expected scores

scored_test_data$expected_n_valid <- expected_n_valid
scored_test_data$expected_mean <- expected_mean
scored_test_data$expected_sum <- expected_sum
scored_test_data$expected_prorated_sum <- expected_prorated_sum


# Check n valid

scored_test_data$n_valid_check <- scored_test_data$test_scale_n_valid == scored_test_data$expected_n_valid


# Check mean score

scored_test_data$mean_check <- round(scored_test_data$test_scale_mean, 6) == round(scored_test_data$expected_mean, 6)

scored_test_data$mean_check[
  is.na(scored_test_data$test_scale_mean) &
    is.na(scored_test_data$expected_mean)
] <- TRUE


# Check sum score

scored_test_data$sum_check <- scored_test_data$test_scale_sum == scored_test_data$expected_sum

scored_test_data$sum_check[
  is.na(scored_test_data$test_scale_sum) &
    is.na(scored_test_data$expected_sum)
] <- TRUE


# Check prorated sum score

scored_test_data$prorated_sum_check <- round(scored_test_data$test_scale_prorated_sum, 6) == round(scored_test_data$expected_prorated_sum, 6)

scored_test_data$prorated_sum_check[
  is.na(scored_test_data$test_scale_prorated_sum) &
    is.na(scored_test_data$expected_prorated_sum)
] <- TRUE


# Inspect checks

scored_test_data


# Check whether all tests passed

all(scored_test_data$n_valid_check)

all(scored_test_data$mean_check)

all(scored_test_data$sum_check)

all(scored_test_data$prorated_sum_check)


# Test more permissive minimum item rule

scored_test_data_min4 <- score_scale(
  data = test_data,
  items = c("item1", "item2", "item3", "item4", "item5"),
  min_items = 4,
  valid_range = c(1, 5),
  score_prefix = "test_scale"
)

scored_test_data_min4


# Test invalid values warning

bad_data <- test_data

bad_data$item1[1] <- 6

score_scale(
  data = bad_data,
  items = c("item1", "item2", "item3", "item4", "item5"),
  min_items = 5,
  valid_range = c(1, 5),
  score_prefix = "test_scale"
)


# Test missing variable error

try(
  score_scale(
    data = test_data,
    items = c("item1", "item2", "item3", "item4", "missing_variable"),
    min_items = 5,
    valid_range = c(1, 5),
    score_prefix = "test_scale"
  )
)


# Test too few items error

try(
  score_scale(
    data = test_data,
    items = c("item1"),
    min_items = 1,
    valid_range = c(1, 5),
    score_prefix = "test_scale"
  )
)
