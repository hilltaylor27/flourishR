# loop_countries.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-13
# Purpose: Loop analyses across countries


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
  
  output <- dplyr::bind_rows(output_list)
  
  
  # Move country variable to front
  
  output <- output %>%
    dplyr::relocate(dplyr::all_of(country_var))
  
  
  # Return output
  
  return(output)
  
}
