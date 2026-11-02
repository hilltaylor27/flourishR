# score_scale.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-12
# Purpose: Score multi-item scales with configurable missing-data rules


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