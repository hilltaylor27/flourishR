#' Global Flourishing Study country labels
#'
#' Return GFS country identifiers and labels or convert
#' numerical country identifiers into readable labels.
#'
#' @param x Optional vector of GFS country identifiers.
#'   If NULL, the complete country lookup table is returned.
#' @param as_factor Logical. Return country labels as a factor.
#' @param unknown How to handle unrecognised country identifiers.
#'
#' @return A data frame, character vector or factor.
#'
#' @examples
#' gfs_countries()
#' gfs_countries(c(1, 9, 20, 22))
#' gfs_countries(c(1, 9, 20), as_factor = TRUE)
#'
#' @export

gfs_countries <- function(
		x = NULL,
		as_factor = FALSE,
		unknown = c("NA", "keep", "error")
) {

	unknown <- match.arg(unknown)

	lookup <- gfs_country_codes

	if (is.null(x)) {
		return(lookup)
	}

	original <- as.character(x)

	index <- match(
		original,
		as.character(lookup$country_code)
	)

	labels <- lookup$country_label[index]

	# preserve existing country labels

	existing <- match(
		original,
		lookup$country_label
	)

	keep <- is.na(index) & !is.na(existing)

	labels[keep] <- lookup$country_label[
		existing[keep]
	]

	# unrecognised identifiers

	unmatched <- is.na(labels) & !is.na(x)

	if (any(unmatched)) {

		if (unknown == "error") {

			stop(
				"Unrecognised country identifiers: ",
				paste(
					unique(original[unmatched]),
					collapse = ", "
				)
			)

		}

		if (unknown == "keep") {
			labels[unmatched] <- original[unmatched]
		}

	}

	if (as_factor) {

		levels <- sort(unique(c(
			lookup$country_label,
			labels[!is.na(labels)]
		)))

		labels <- factor(labels, levels = levels)

	}

	labels

}
