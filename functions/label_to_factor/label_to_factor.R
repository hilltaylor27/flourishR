# label_to_factor.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Convert labelled survey variables to factors


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