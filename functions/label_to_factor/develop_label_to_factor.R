# develop_label_to_factor.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test label_to_factor()


# Load packages

library(tidyverse)
library(haven)


# Simulate labelled data

test_data <- data.frame(
  id = 1:5,
  gender = labelled(
    c(1, 2, 1, 3, NA),
    labels = c(
      "Woman" = 1,
      "Man" = 2,
      "Other" = 3
    )
  ),
  employment = labelled(
    c(1, 1, 2, 3, NA),
    labels = c(
      "Full-time" = 1,
      "Part-time" = 2,
      "Student" = 3
    )
  )
)


# Inspect test data

test_data


# Create label_to_factor function

label_to_factor <- function(data,
                            variables) {
  
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
  
  
  # Convert labelled variables to factors
  
  for (var in variables) {
    
    data[[var]] <- haven::as_factor(data[[var]])
    
  }
  
  
  # Return data
  
  return(data)
  
}


# Run function

factor_data <- label_to_factor(
  data = test_data,
  variables = c("gender", "employment")
)


# Inspect results

factor_data

str(factor_data)


# Check expected output

is.factor(factor_data$gender)

is.factor(factor_data$employment)

levels(factor_data$gender)

levels(factor_data$employment)


# Test missing variable error

try(
  label_to_factor(
    data = test_data,
    variables = c("gender", "missing_variable")
  )
)