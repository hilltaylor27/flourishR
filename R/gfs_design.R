#' Construct a Global Flourishing Study survey design
#'
#' Construct a country-specific survey design using GFS
#' sampling weights, strata and primary sampling units.
#'
#' @param data Prepared GFS data in wide or long format.
#' @param country Country identifier or country label.
#' @param waves Annual waves determining the survey weight.
#' @param type Optional survey weight type.
#' @param analysis_wave Wave to retain from long-format data.
#' @param nest Logical. Nest PSU identifiers within strata.
#'
#' @details
#' The returned design contains one record per participant
#' with a valid survey weight. For long-format data,
#' analysis_wave determines which participant record is
#' retained.
#'
#' Weights support within-country inference. Variances use the supplied strata
#' and PSU identifiers with the survey package's variance settings. The
#' function does not prescribe a singleton-PSU adjustment; choose one based
#' on the sampling design. Records with missing weights are excluded.
#'
#' This function constructs a stratified and clustered
#' survey design. It does not construct a repeated-measures
#' or multilevel longitudinal survey model.
#'
#' @return A survey.design object.
#'
#' @export

#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   d <- data.frame(ID = 1:8, COUNTRY = "Japan", STRATA = rep(1:2, each = 4),
#'                   PSU = rep(1:4, each = 2), ANNUAL_WEIGHT_C1 = 1, x = 1:8)
#'   survey::svymean(~x, gfs_design(d, "Japan"))
#' }
gfs_design <- function(
		data,
		country,
		waves = 1,
		type = NULL,
		analysis_wave = max(waves),
		nest = TRUE
) {

	if (!requireNamespace("survey", quietly = TRUE)) {
		stop("The survey package must be installed.")
	}

	if (!is.data.frame(data)) {
		stop("data must be a data frame.")
	}

	if (length(country) != 1 || is.na(country)) {
		stop("Specify a single country.")
	}

	required <- c("ID", "COUNTRY", "STRATA", "PSU")

	if (!all(required %in% names(data))) {

		stop(
			"Variables not found: ",
			paste(
				setdiff(required, names(data)),
				collapse = ", "
			)
		)

	}

	weight <- gfs_weights(
		waves = waves,
		type = type
	)

	if (!weight %in% names(data)) {
		stop("Weight variable not found: ", weight)
	}

	# select country

	country_label <- gfs_countries(
		country,
		unknown = "error"
	)

	labels <- gfs_countries(
		data$COUNTRY,
		unknown = "error"
	)

	data <- data[
		!is.na(labels) & labels == country_label,
		,
		drop = FALSE
	]

	# select participant records

	if ("wave" %in% names(data)) {

		if (
			length(analysis_wave) != 1 ||
			is.na(analysis_wave) ||
			!analysis_wave %in% waves
		) {
			stop("analysis_wave must be one of the requested waves.")
		}

		data <- data[
			!is.na(data$wave) &
				data$wave == analysis_wave,
			,
			drop = FALSE
		]

	}

	if (nrow(data) == 0) {
		stop("No records found for the requested country and wave.")
	}

	if (anyNA(data$ID) || anyDuplicated(data$ID)) {
		stop("Survey design requires unique nonmissing participant IDs.")
	}

	# validate survey weights

	gfs_weights(
		waves = waves,
		type = type,
		data = data
	)

	data <- data[
		!is.na(data[[weight]]),
		,
		drop = FALSE
	]

	if (anyNA(data$STRATA) || anyNA(data$PSU)) {
		stop("Missing survey design identifiers detected.")
	}

	# construct survey design

	survey::svydesign(
		ids = stats::as.formula("~PSU"),
		strata = stats::as.formula("~STRATA"),
		weights = stats::reformulate(weight),
		data = data,
		nest = nest
	)

}
