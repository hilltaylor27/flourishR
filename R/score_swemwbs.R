
#' Score the Short Warwick-Edinburgh Mental Wellbeing Scale
#'
#' Scores the seven-item Short Warwick-Edinburgh Mental Wellbeing Scale
#' (SWEMWBS) using `score_scale()`.
#'
#' SWEMWBS items are expected to have values from 1 to 5. Values outside
#' this range are treated as missing for scoring. By default, at least
#' five of the seven items must be valid for a score to be calculated.
#'
#' The function calculates raw sum, mean, and prorated sum scores. It does
#' not apply the published SWEMWBS raw-to-metric score conversion. The
#' resulting scores should not be interpreted as transformed SWEMWBS
#' metric scores.
#'
#' @param data A data frame containing the SWEMWBS items.
#' @param items A character vector containing the names of exactly seven
#'   SWEMWBS items.
#' @param min_items Minimum number of valid items required to calculate a
#'   score. Defaults to 5.
#'
#' @return The original data frame with four additional variables:
#'   \describe{
#'     \item{swemwbs.n.valid}{Number of valid SWEMWBS item responses.}
#'     \item{swemwbs.sum}{Sum of valid item responses when the minimum-item
#'       rule is met.}
#'     \item{swemwbs.mean}{Mean of valid item responses when the minimum-item
#'       rule is met.}
#'     \item{swemwbs.prorated}{Mean of valid item responses multiplied by
#'       seven.}
#'   }
#'
#' @examples
#' dat <- data.frame(
#'   sw1 = c(1, 5),
#'   sw2 = c(2, 5),
#'   sw3 = c(3, 5),
#'   sw4 = c(4, 5),
#'   sw5 = c(5, 5),
#'   sw6 = c(1, 5),
#'   sw7 = c(2, 5)
#' )
#'
#' score_swemwbs(
#'   data = dat,
#'   items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
#' )
#'
#' @export
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
		prefix = "swemwbs",
		min_items = min_items,
		valid_range = c(1, 5)
	)


	# Return data

	return(data)

}
