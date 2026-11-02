# develop_function_name.R
# flourishR
# Author: Taylor Hill
# Created: YYYY-MM-DD
# Purpose: Develop and test function_name()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  x = c(1, 2, 3),
  y = c(4, 5, 6)
)


# Inspect test data

test_data


# Create function

function_name <- function(data) {
  
  # Check inputs
  
  
  # Main function logic
  
  
  # Return data
  
  return(data)
  
}


# Run function

test_output <- function_name(test_data)


# Inspect results

test_output


# Check expected output

# Add tests here


# Test errors

try(
  function_name("not a dataframe")
)