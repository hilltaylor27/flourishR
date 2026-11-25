#' Convert labelled survey variables to factors
#'
#' Converts selected labelled survey variables to factors using
#' [haven::as_factor()]. This is useful for survey datasets in which
#' categorical variables are stored as numeric values with attached
#' value labels.
#'
#' Only variables specified in `variables` are converted. All other
#' variables are returned unchanged.
#'
#' @param data A data frame containing the variables to convert.
#' @param variables A character vector giving the names of variables
#'   to convert to factors.
#'
#' @return The original data frame with the selected variables converted
#'   to factors.
#'
#' @details
#' Survey data imported from formats such as SPSS or Stata may contain
#' labelled variables in which numeric values have descriptive value
#' labels attached. `label_to_factor()` converts these labelled values
#' into factor levels, making them easier to use in descriptive analyses,
#' tables, plots, and models.
#'
#' The function returns an error if `data` is not a data frame, if
#' `variables` is not supplied as a character vector, or if any requested
#' variable is not present in the data.
#'
#' @examples
#' dat <- data.frame(
#'   id = 1:4,
#'   gender = haven::labelled(
#'     c(1, 2, 1, 3),
#'     labels = c(
#'       "Woman" = 1,
#'       "Man" = 2,
#'       "Other" = 3
#'     )
#'   )
#' )
#'
#' label_to_factor(
#'   data = dat,
#'   variables = "gender"
#' )
#'
#' @export

label_to_factor <- function(data,
                            variables) {

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


  # Convert labelled variables to factors

  for (var in variables) {

    data[[var]] <- haven::as_factor(data[[var]])

  }


  # Return data

  return(data)

}
