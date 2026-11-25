#' Recode survey-specific missing values to NA
#'
#' Replaces specified survey missing-value codes with `NA` in selected
#' variables.
#'
#' Many survey datasets use numeric codes such as `-9`, `-8`, or `-1`
#' to represent different forms of missing data. `recode_missing()`
#' converts these codes to standard R missing values before analysis.
#'
#' Only variables specified in `variables` are modified. All other
#' variables are returned unchanged.
#'
#' @param data A data frame containing the variables to recode.
#' @param variables A character vector giving the names of variables
#'   in which missing-value codes should be recoded.
#' @param missing_codes A vector of values to replace with `NA`.
#'   Defaults to `c(-9, -8, -7, -2, -1)`.
#'
#' @return The original data frame with specified missing-value codes
#'   replaced by `NA` in the selected variables.
#'
#' @details
#' By default, `recode_missing()` treats `-9`, `-8`, `-7`, `-2`, and
#' `-1` as missing-value codes. Alternative codes can be supplied using
#' `missing_codes`.
#'
#' Existing `NA` values are preserved. Values in variables not specified
#' in `variables` are not modified.
#'
#' The function returns an error if `data` is not a data frame, if
#' `variables` is not supplied as a character vector, or if any requested
#' variable is not present in the data.
#'
#' @examples
#' dat <- data.frame(
#'   life_satisfaction = c(7, 8, -9, 6, -8),
#'   happiness = c(8, -9, 7, -1, 6),
#'   age = c(25, 30, 35, 40, 45)
#' )
#'
#' recode_missing(
#'   data = dat,
#'   variables = c("life_satisfaction", "happiness")
#' )
#'
#' @export

recode_missing <- function(data,
                           variables,
                           missing_codes = c(-9, -8, -7, -2, -1)) {

  # Check inputs

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!is.character(variables)) {
    stop("variables must be a character vector.")
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