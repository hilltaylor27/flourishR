# score_swemwbs.R
# flourishR
# Author: Taylor Hill
# Created: 2026-06-12
# Purpose: Score the Short Warwick-Edinburgh Mental Wellbeing Scale (SWEMWBS)


score_swemwbs <- function(data,
                          items,
                          min_items = 5) {
  
  # Check number of items
  
  if (length(items) != 7) {
    stop("SWEMWBS requires exactly 7 items.")
  }
  
  
  # Score SWEMWBS
  
  data <- score_scale(
    data = data,
    items = items,
    min_items = min_items,
    valid_range = c(1, 5),
    score_prefix = "swemwbs"
  )
  
  
  # Return data
  
  return(data)
  
}