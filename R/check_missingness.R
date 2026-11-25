#' Summarise missing data patterns
#'
#' Summarises the number and percentage of missing values for each
#' variable in a data frame, or for a selected set of variables.
#'
#' @param data A data frame.
#' @param variables An optional character vector containing the names of
#'   variables to check. If `NULL`, all variables in `data` are checked.
#'
#' @param by Optional character vector of grouping variables. Grouped output includes the group size n.
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
                              variables = NULL, by = NULL) {
  if (!is.null(by)) {
    stopifnot(is.data.frame(data), is.character(by), length(by) > 0,
              !anyDuplicated(by), all(by %in% names(data)))
    if (any(by %in% c("variable", "n_missing", "pct_missing", "n"))) stop("Reserved grouping name.")
    if (is.null(variables)) variables <- setdiff(names(data), by)
    grouped <- dplyr::group_by(data, !!!rlang::syms(by))
    result <- dplyr::group_modify(grouped, function(.x, .y) {
      out <- check_missingness(.x, variables = setdiff(variables, by))
      out$n <- rep(nrow(.x), nrow(out))
      out
    })
    return(as.data.frame(dplyr::ungroup(result)))
  }

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
    n_missing = rep(NA_integer_, length(variables)),
    pct_missing = rep(NA_real_, length(variables))
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
