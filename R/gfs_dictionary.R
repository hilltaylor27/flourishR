#' Global Flourishing Study variable dictionary
#'
#' Access variable availability, missing response codes,
#' country identifiers and survey weight metadata for
#' the Global Flourishing Study.
#'
#' @param variable Character vector of GFS variable names.
#'   If NULL, all variables are returned.
#' @param wave Integer vector specifying annual survey
#'   waves (1, 2 or 3).
#' @param search Character string used to search variable names.
#' @param type Metadata table to return.
#' @param release Codebook release for missing response
#'   metadata, either 2 or 3.
#'
#' @return A data frame containing the requested metadata.
#'
#' @references
#' Global Flourishing Study documentation:
#' \url{https://www.cos.io/gfs-resources}
#'
#' @examples
#' gfs_dictionary()
#' gfs_dictionary(variable = "LIFE_SAT")
#' gfs_dictionary(wave = 3)
#' gfs_dictionary(search = "REL")
#' gfs_dictionary(type = "missing", variable = "AGE")
#' gfs_dictionary(type = "weights")
#'
#' @export

gfs_dictionary <- function(
		variable = NULL,
		wave = NULL,
		search = NULL,
		type = c(
			"variables",
			"missing",
			"countries",
			"weights"
		),
		release = 3L
) {

	type <- match.arg(type)

	if (!is.null(wave)) {

		if (
			!is.numeric(wave) ||
			anyNA(wave) ||
			!all(wave %in% 1:3)
		) {
			stop("wave must contain 1, 2 or 3.")
		}

		if (!type %in% c("variables", "weights")) {
			stop("wave filtering is available for variables and weights.")
		}

	}

	if (
		length(release) != 1 ||
		is.na(release) ||
		!release %in% c(2, 3)
	) {
		stop("release must be 2 or 3.")
	}

	out <- switch(
		type,
		variables = gfs_availability,
		missing = gfs_missing_codes,
		countries = gfs_country_codes,
		weights = gfs_weight_metadata
	)

	if (type == "missing") {

		out <- out[
			out$release == release,
			,
			drop = FALSE
		]

	}

	if (!is.null(variable)) {

		if (!"variable" %in% names(out)) {
			stop("Variable filtering is unavailable for this metadata type.")
		}

		variable <- toupper(variable)

		keep <- out$variable %in% variable

		if ("base_variable" %in% names(out)) {

			keep <- keep |
				out$base_variable %in% variable

		}

		out <- out[keep, , drop = FALSE]

	}

	if (!is.null(wave)) {

		columns <- paste0("year", unique(wave))

		keep <- rowSums(
			out[, columns, drop = FALSE]
		) == length(columns)

		out <- out[keep, , drop = FALSE]

	}

	if (!is.null(search)) {

		if (
			!is.character(search) ||
			length(search) != 1 ||
			is.na(search)
		) {
			stop("search must be a single character string.")
		}

		column <- if (type == "countries") {
			"country_label"
		} else {
			"variable"
		}

		keep <- grepl(
			tolower(search),
			tolower(out[[column]]),
			fixed = TRUE
		)

		out <- out[keep, , drop = FALSE]

	}

	rownames(out) <- NULL

	out

}
