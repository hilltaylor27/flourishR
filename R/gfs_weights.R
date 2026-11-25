#' Select Global Flourishing Study survey weights
#'
#' Identify the appropriate annual GFS survey weight for
#' cross-sectional, longitudinal or all-record analyses.
#'
#' @param waves Integer vector specifying annual waves.
#' @param type Character. Weight type. If NULL, the type is
#'   determined from the requested waves.
#' @param data Optional data frame in which to validate the
#'   selected weight.
#'
#' @details
#' GFS survey weights are intended for within-country
#' inference. Longitudinal weights correspond to specific
#' combinations of annual survey participation.
#'
#' @return A character string containing the weight variable name.
#'
#' @examples
#' gfs_weights(1)
#' gfs_weights(3)
#' gfs_weights(1:3)
#' gfs_weights(3, type = "all records")
#'
#' @export

gfs_weights <- function(
		waves = 1,
		type = NULL,
		data = NULL
) {

	if (
		!is.numeric(waves) ||
		length(waves) == 0 ||
		anyNA(waves) ||
		!all(waves %in% 1:3)
	) {
		stop("waves must contain annual wave numbers from 1 to 3.")
	}

	waves <- sort(unique(as.integer(waves)))

	if (is.null(type)) {

		type <- if (length(waves) == 1) {
			"cross-sectional"
		} else {
			"longitudinal"
		}

	}

	type <- match.arg(
		type,
		c("cross-sectional", "longitudinal", "all records")
	)

	# select weight

	if (type == "cross-sectional") {

		if (length(waves) != 1) {
			stop("Cross-sectional weights require a single wave.")
		}

		weight <- paste0("ANNUAL_WEIGHT_C", waves)

	} else if (type == "longitudinal") {

		if (identical(waves, 1:2)) {

			weight <- "ANNUAL_WEIGHT_L2"

		} else if (identical(waves, 1:3)) {

			weight <- "ANNUAL_WEIGHT_L3"

		} else {

			stop(
				"No dedicated longitudinal weight exists for ",
				"the requested combination of waves."
			)

		}

	} else {

		if (length(waves) != 1 || !waves %in% c(2, 3)) {
			stop("All-record weights require Wave 2 or Wave 3.")
		}

		weight <- paste0("ANNUAL_WEIGHT_R", waves)

	}

	# validate weight

	if (!is.null(data)) {

		if (!is.data.frame(data)) {
			stop("data must be a data frame.")
		}

		if (!weight %in% names(data)) {
			stop("Weight variable not found: ", weight)
		}

		values <- data[[weight]]

		if (!is.numeric(values)) {
			stop("Survey weights must be numeric.")
		}

		observed <- values[!is.na(values)]

		if (length(observed) == 0) {
			stop("No nonmissing survey weights are available.")
		}

		if (any(!is.finite(observed) | observed <= 0)) {
			stop("Survey weights must be finite and positive.")
		}

	}

	weight

}
