# develop_loop_countries.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Develop and test loop_countries()


# Load packages

library(tidyverse)


# Simulate test data

test_data <- data.frame(
  id = 1:12,
  country = c(
    "England", "England", "England",
    "Scotland", "Scotland", "Scotland",
    "Wales", "Wales", "Wales",
    "Northern Ireland", "Northern Ireland", "Northern Ireland"
  ),
  happiness = c(7, 8, NA, 6, NA, 7, 5, 6, NA, 8, 7, NA),
  life_satisfaction = c(8, NA, 7, 6, 6, NA, 5, NA, 6, 8, 8, 7)
)


# Inspect test data

test_data


# Create helper analysis function

check_missingness_simple <- function(data,
                                     variables) {
  
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
  
  return(output)
  
}


# Create loop_countries function

loop_countries <- function(data,
                           country_var,
                           analysis_function,
                           ...) {
  
  # Check inputs
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (!country_var %in% names(data)) {
    stop("country_var must be a variable in data.")
  }
  
  if (!is.function(analysis_function)) {
    stop("analysis_function must be a function.")
  }
  
  
  # Identify countries
  
  countries <- unique(data[[country_var]])
  
  countries <- countries[!is.na(countries)]
  
  
  # Run analysis by country
  
  output_list <- list()
  
  for (i in seq_along(countries)) {
    
    current_country <- countries[i]
    
    country_data <- data[data[[country_var]] == current_country, ]
    
    country_output <- analysis_function(
      data = country_data,
      ...
    )
    
    country_output[[country_var]] <- current_country
    
    output_list[[i]] <- country_output
    
  }
  
  
  # Bind outputs
  
  output <- bind_rows(output_list)
  
  
  # Move country variable to front
  
  output <- output %>%
    relocate(all_of(country_var))
  
  
  # Return output
  
  return(output)
  
}


# Run function

missing_by_country <- loop_countries(
  data = test_data,
  country_var = "country",
  analysis_function = check_missingness_simple,
  variables = c("happiness", "life_satisfaction")
)


# Inspect results

missing_by_country


# Check expected number of rows

nrow(missing_by_country) == 8


# Check expected countries

unique(missing_by_country$country)


# Check expected variables

unique(missing_by_country$variable)


# Test with check_missingness if already loaded

if (exists("check_missingness")) {
  
  missing_by_country2 <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = check_missingness,
    variables = c("happiness", "life_satisfaction")
  )
  
  missing_by_country2
  
}


# Test missing country variable error

try(
  loop_countries(
    data = test_data,
    country_var = "missing_country",
    analysis_function = check_missingness_simple,
    variables = c("happiness", "life_satisfaction")
  )
)


# Test analysis function error

try(
  loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = "not_a_function",
    variables = c("happiness", "life_satisfaction")
  )
)
