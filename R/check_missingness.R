#' Summarise missing data patterns
#'
#' Summarises the number and percentage of missing values for each
#' variable in a data frame, or for a selected set of variables.
#'
#' @param data A data frame.
#' @param variables An optional character vector containing the names of
#'   variables to check. If `NULL`, all variables in `data` are checked.
#'
#' @return A data frame with one row per variable and three columns:
#'   \describe{
#'     \item{variable}{Variable name.}
#'     \item{n_missing}{Number of missing values.}
#'     \item{pct_missing}{Percentage of values that are missing, rounded
#'       to two decimal places.}
#'   }
#'
#' @examples
#' dat <- data.frame(
#'   id = 1:6,
#'   life_satisfaction = c(7, 8, NA, 6, NA, 5),
#'   happiness = c(8, NA, 7, NA, 6, 5)
#' )
#'
#' check_missingness(dat)
#'
#' check_missingness(
#'   dat,
#'   variables = c("life_satisfaction", "happiness")
#' )
#'
#' @export
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
