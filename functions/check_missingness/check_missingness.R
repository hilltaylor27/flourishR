# check_missingness.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Summarise missing data patterns for selected variables


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