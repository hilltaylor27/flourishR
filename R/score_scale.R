#' Score a multi-item scale
#'
#' Scores a multi-item scale using a specified minimum-item rule and,
#' optionally, a valid response range. Values outside the valid range are
#' treated as missing for scoring.
#'
#' The function returns the original data with variables containing the
#' number of valid items, sum score, mean score, and prorated sum score.
#'
#' @param data A data frame containing the scale items.
#' @param items A character vector containing the names of the scale items.
#' @param prefix A single non-empty character value used to name the
#'   generated scoring variables.
#' @param min_items Minimum number of valid items required to calculate a
#'   score. Defaults to the total number of items, requiring complete data.
#' @param valid_range Optional numeric vector of length two giving the
#'   minimum and maximum valid item values. Values outside this range are
#'   treated as missing for scoring. If `NULL`, no range restriction is
#'   applied.
#'
#' @return The original data frame with four additional variables:
#'   \describe{
#'     \item{<prefix>.n.valid}{Number of valid item responses.}
#'     \item{<prefix>.sum}{Sum of valid item responses when the minimum-item
#'       rule is met.}
#'     \item{<prefix>.mean}{Mean of valid item responses when the minimum-item
#'       rule is met.}
#'     \item{<prefix>.prorated}{Mean of valid item responses multiplied by
#'       the total number of scale items.}
#'   }
#'
#' @examples
#' dat <- data.frame(
#'   item.1 = c(1, 2, 3),
#'   item.2 = c(2, 3, NA),
#'   item.3 = c(3, 4, 5),
#'   item.4 = c(4, 5, 4)
#' )
#'
#' score_scale(
#'   data = dat,
#'   items = c("item.1", "item.2", "item.3", "item.4"),
#'   prefix = "scale",
#'   min_items = 3,
#'   valid_range = c(1, 5)
#' )
#'
#' @export
score_scale <- function(data,
                        items,
                        prefix,
                        min_items = length(items),
                        valid_range = NULL) {

  # Check inputs

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!is.character(items) || length(items) < 1) {
    stop("items must be a character vector containing at least one variable name.")
  }

  missing_items <- setdiff(items, names(data))

  if (length(missing_items) > 0) {
    stop(
      paste0(
        "The following items are not present in data: ",
        paste(missing_items, collapse = ", "),
        "."
      )
    )
  }

  if (anyDuplicated(items)) {
    stop("items must not contain duplicate variable names.")
  }

  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix) || prefix == "") {
    stop("prefix must be a single non-empty character value.")
  }

  if (!is.numeric(min_items) || length(min_items) != 1 || is.na(min_items) ||
      min_items %% 1 != 0 || min_items < 1 || min_items > length(items)) {
    stop("min_items must be a whole number between 1 and the number of items.")
  }

  if (!is.null(valid_range)) {
    if (!is.numeric(valid_range) || length(valid_range) != 2 ||
        any(is.na(valid_range)) || valid_range[1] > valid_range[2]) {
      stop("valid_range must be NULL or a numeric vector containing the minimum and maximum valid values.")
    }
  }

  non_numeric_items <- items[!vapply(data[items], is.numeric, logical(1))]

  if (length(non_numeric_items) > 0) {
    stop(
      paste0(
        "The following items are not numeric: ",
        paste(non_numeric_items, collapse = ", "),
        "."
      )
    )
  }

  output_names <- paste0(prefix, c(".n.valid", ".sum", ".mean", ".prorated"))
  existing_names <- intersect(output_names, names(data))

  if (length(existing_names) > 0) {
    stop(
      paste0(
        "The following output variables already exist in data: ",
        paste(existing_names, collapse = ", "),
        ". Choose a different prefix or remove the existing variables."
      )
    )
  }


  # Main function logic

  item_data <- data[items]

  if (!is.null(valid_range)) {
    invalid_values <- !is.na(item_data) &
      (item_data < valid_range[1] | item_data > valid_range[2])

    item_data[invalid_values] <- NA_real_
  }

  n_valid <- rowSums(!is.na(item_data))
  score_sum <- rowSums(item_data, na.rm = TRUE)
  score_mean <- rowMeans(item_data, na.rm = TRUE)
  score_prorated <- score_mean * length(items)

  insufficient_items <- n_valid < min_items

  score_sum[insufficient_items] <- NA_real_
  score_mean[insufficient_items] <- NA_real_
  score_prorated[insufficient_items] <- NA_real_

  score_mean[is.nan(score_mean)] <- NA_real_

  data[[paste0(prefix, ".n.valid")]] <- n_valid
  data[[paste0(prefix, ".sum")]] <- score_sum
  data[[paste0(prefix, ".mean")]] <- score_mean
  data[[paste0(prefix, ".prorated")]] <- score_prorated


  # Return data

  return(data)

}
