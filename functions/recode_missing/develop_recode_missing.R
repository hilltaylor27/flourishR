# develop_recode_missing.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test recode_missing()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  id = 1:5,
  life_satisfaction = c(7, 8, -9, 6, -8),
  happiness = c(8, -9, 7, -1, 6),
  age = c(25, 30, 35, 40, 45)
)


# Inspect test data

test_data


# Create recode_missing function

recode_missing <- function(data,
                           variables,
                           missing_codes = c(-9, -8, -7, -2, -1)) {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  missing_variables <- variables[!variables %in% names(data)]
  
  if (length(missing_variables) > 0) {
    stop(
      "Missing variables: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  
  # Recode missing values
  
  for (var in variables) {
    
    data[[var]][data[[var]] %in% missing_codes] <- NA
    
  }
  
  
  # Return data
  
  return(data)
  
}


# Run function

recoded_data <- recode_missing(
  data = test_data,
  variables = c("life_satisfaction", "happiness")
)


# Inspect results

recoded_data


# Check expected output

expected_ls <- c(7, 8, NA, 6, NA)

expected_happiness <- c(8, NA, 7, NA, 6)


all.equal(
  recoded_data$life_satisfaction,
  expected_ls
)

all.equal(
  recoded_data$happiness,
  expected_happiness
)


# Test custom missing codes

test_data2 <- data.frame(
  score = c(1, 2, 999, 4, 999)
)

recode_missing(
  data = test_data2,
  variables = "score",
  missing_codes = 999
)


# Test missing variable error

try(
  recode_missing(
    data = test_data,
    variables = c("life_satisfaction", "missing_variable")
  )
)