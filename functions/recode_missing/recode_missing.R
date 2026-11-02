# recode_missing.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Recode survey-specific missing values to NA


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