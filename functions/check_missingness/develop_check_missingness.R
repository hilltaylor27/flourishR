# develop_check_missingness.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test check_missingness()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  id = 1:6,
  life_satisfaction = c(7, 8, NA, 6, NA, 5),
  happiness = c(8, NA, 7, NA, 6, 5),
  age = c(25, 30, 35, 40, 45, 50)
)


# Inspect test data

test_data


# Create check_missingness function

check_missingness <- function(data,
                              variables = NULL) {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  
  # Select variables
  
  if (is.null(variables)) {
    variables <- names(data)
  }
  
  missing_variables <- variables[!variables %in% names(data)]
  
  if (length(missing_variables) > 0) {
    stop(
      "Missing variables: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  
  # Calculate missingness
  
  output <- data.frame(
    variable = variables,
    n_missing = NA,
    pct_missing = NA
  )
  
  for (i in seq_along(variables)) {
    
    var <- variables[i]
    
    n_missing <- sum(is.na(data[[var]]))
    
    pct_missing <- (n_missing / nrow(data)) * 100
    
    output$n_missing[i] <- n_missing
    output$pct_missing[i] <- round(pct_missing, 2)
    
  }
  
  
  # Return summary table
  
  return(output)
  
}


# Run function

missing_summary <- check_missingness(test_data)


# Inspect results

missing_summary


# Create expected values

expected_missing <- c(0, 2, 2, 0)

expected_pct <- c(0, 33.33, 33.33, 0)


# Check results

all(missing_summary$n_missing == expected_missing)

all(round(missing_summary$pct_missing, 2) == expected_pct)


# Test selected variables

check_missingness(
  data = test_data,
  variables = c("life_satisfaction", "happiness")
)


# Test missing variable error

try(
  check_missingness(
    data = test_data,
    variables = c("life_satisfaction", "missing_variable")
  )
)