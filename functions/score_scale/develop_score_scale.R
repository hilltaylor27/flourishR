# develop_score_scale.R
# flourishR
# Author: Taylor Hill
# Created: 2026-07-19
# Purpose: Develop and test score_scale()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  id = 1:7,
  item.1 = c(1, 2, 3, 4, 5, NA, 1),
  item.2 = c(2, 3, 4, 5, NA, NA, 8),
  item.3 = c(3, 4, 5, NA, NA, NA, 3),
  item.4 = c(4, 5, 1, NA, 2, NA, 4)
)


# Inspect test data

test_data


# Create function

score_scale <- function(data,
                        items,
                        prefix,
                        min_items = length(items),
                        valid_range = NULL) {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (!is.character(items) || length(items) < 1) {
    stop("items must be a character vector containing at least one variable name.")
  }
  
  missing_items <- setdiff(items, names(data))
  
  if (length(missing_items) > 0) {
    stop(
      paste0(
        "The following items are not present in data: ",
        paste(missing_items, collapse = ", "),
        "."
      )
    )
  }
  
  if (anyDuplicated(items)) {
    stop("items must not contain duplicate variable names.")
  }
  
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix) || prefix == "") {
    stop("prefix must be a single non-empty character value.")
  }
  
  if (!is.numeric(min_items) || length(min_items) != 1 || is.na(min_items) ||
      min_items %% 1 != 0 || min_items < 1 || min_items > length(items)) {
    stop("min_items must be a whole number between 1 and the number of items.")
  }
  
  if (!is.null(valid_range)) {
    if (!is.numeric(valid_range) || length(valid_range) != 2 ||
        any(is.na(valid_range)) || valid_range[1] > valid_range[2]) {
      stop("valid_range must be NULL or a numeric vector containing the minimum and maximum valid values.")
    }
  }
  
  non_numeric_items <- items[!vapply(data[items], is.numeric, logical(1))]
  
  if (length(non_numeric_items) > 0) {
    stop(
      paste0(
        "The following items are not numeric: ",
        paste(non_numeric_items, collapse = ", "),
        "."
      )
    )
  }
  
  output_names <- paste0(prefix, c(".n.valid", ".sum", ".mean", ".prorated"))
  existing_names <- intersect(output_names, names(data))
  
  if (length(existing_names) > 0) {
    stop(
      paste0(
        "The following output variables already exist in data: ",
        paste(existing_names, collapse = ", "),
        ". Choose a different prefix or remove the existing variables."
      )
    )
  }
  
  
  # Main function logic
  
  item_data <- data[items]
  
  if (!is.null(valid_range)) {
    invalid_values <- !is.na(item_data) &
      (item_data < valid_range[1] | item_data > valid_range[2])
    
    item_data[invalid_values] <- NA_real_
  }
  
  n_valid <- rowSums(!is.na(item_data))
  score_sum <- rowSums(item_data, na.rm = TRUE)
  score_mean <- rowMeans(item_data, na.rm = TRUE)
  score_prorated <- score_mean * length(items)
  
  insufficient_items <- n_valid < min_items
  
  score_sum[insufficient_items] <- NA_real_
  score_mean[insufficient_items] <- NA_real_
  score_prorated[insufficient_items] <- NA_real_
  
  score_mean[is.nan(score_mean)] <- NA_real_
  
  data[[paste0(prefix, ".n.valid")]] <- n_valid
  data[[paste0(prefix, ".sum")]] <- score_sum
  data[[paste0(prefix, ".mean")]] <- score_mean
  data[[paste0(prefix, ".prorated")]] <- score_prorated
  
  
  # Return data
  
  return(data)
  
}


# Run function

test_output <- score_scale(
  data = test_data,
  items = c("item.1", "item.2", "item.3", "item.4"),
  prefix = "scale",
  min_items = 3,
  valid_range = c(1, 5)
)


# Inspect results

test_output


# Check expected output

stopifnot(
  identical(test_output$scale.n.valid, c(4, 4, 4, 2, 2, 0, 3)),
  identical(test_output$scale.sum, c(10, 14, 13, NA, NA, NA, 8)),
  isTRUE(all.equal(test_output$scale.mean, c(2.5, 3.5, 3.25, NA, NA, NA, 8 / 3))),
  isTRUE(all.equal(test_output$scale.prorated, c(10, 14, 13, NA, NA, NA, 32 / 3)))
)


# Check default complete-case rule

complete_output <- score_scale(
  data = test_data,
  items = c("item.1", "item.2", "item.3", "item.4"),
  prefix = "complete",
  valid_range = c(1, 5)
)

stopifnot(
  !is.na(complete_output$complete.mean[1]),
  !is.na(complete_output$complete.mean[2]),
  !is.na(complete_output$complete.mean[3]),
  is.na(complete_output$complete.mean[4]),
  is.na(complete_output$complete.mean[7])
)


# Test errors

try(
  score_scale(
    data = "not a dataframe",
    items = c("item.1", "item.2"),
    prefix = "scale"
  )
)

try(
  score_scale(
    data = test_data,
    items = c("item.1", "missing.item"),
    prefix = "scale"
  )
)

try(
  score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "scale",
    min_items = 3
  )
)

try(
  score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "scale",
    valid_range = c(5, 1)
  )
)
