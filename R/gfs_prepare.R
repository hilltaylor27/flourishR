#' Prepare Global Flourishing Study data
#'
#' Prepare combined annual Global Flourishing Study data for
#' cross-sectional and longitudinal analyses. The function
#' identifies wave-specific variables, recodes documented
#' missing values, labels countries and returns data in
#' wide or long format.
#'
#' @param data A data frame or path to a GFS CSV file.
#' @param waves Integer vector specifying annual waves.
#' @param variables Character vector of variable names without
#'   annual wave suffixes. NULL retains all available variables.
#' @param format Character. Either "long" or "wide".
#' @param recode_missing Logical. Recode documented GFS
#'   missing-value codes to NA.
#' @param label_countries Logical. Convert country identifiers
#'   into country labels.
#' @param keep_weights Logical. Retain available survey weights.
#' @param keep_design Logical. Retain strata and PSU identifiers.
#' @param release Integer. GFS codebook release, either 2 or 3.
#'
#' @details
#' Long-format data contain one record per participant per
#' selected wave. Unsuffixed participant-level variables are
#' retained across waves. Variables measured exclusively at
#' baseline retain their original Year 1 suffix.
#'
#' Wave-specific variables are not carried forward between
#' waves. Records are retained irrespective of participation
#' in individual waves.
#'
#' Survey weights retain their original names and values.
#' Item response directions are not changed.
#'
#' @return A data frame containing prepared GFS data.
#'
#' @examples
#' data <- data.frame(
#'   ID = 1:3,
#'   COUNTRY = c(1, 9, 20),
#'   LIFE_SAT_Y1 = c(7, 8, 99),
#'   LIFE_SAT_Y2 = c(8, 9, 6),
#'   LIFE_SAT_Y3 = c(9, 8, 7)
#' )
#'
#' gfs_prepare(data)
#' gfs_prepare(data, format = "wide")
#' gfs_prepare(data, waves = c(1, 3))
#'
#' @export

gfs_prepare <- function(
		data,
		waves = 1:3,
		variables = NULL,
		format = c("long", "wide"),
		recode_missing = TRUE,
		label_countries = TRUE,
		keep_weights = TRUE,
		keep_design = TRUE,
		release = 3L
) {

	format <- match.arg(format)

	# validate arguments

	if (
		!is.numeric(waves) ||
		length(waves) == 0 ||
		anyNA(waves) ||
		!all(waves %in% 1:3)
	) {
		stop("waves must contain annual wave numbers from 1 to 3.")
	}

	waves <- sort(unique(as.integer(waves)))

	if (
		length(release) != 1 ||
		is.na(release) ||
		!release %in% c(2, 3)
	) {
		stop("release must be 2 or 3.")
	}

	if (any(waves > release)) {
		stop("Requested waves exceed the selected codebook release.")
	}

	flags <- list(
		recode_missing = recode_missing,
		label_countries = label_countries,
		keep_weights = keep_weights,
		keep_design = keep_design
	)

	for (argument in names(flags)) {

		value <- flags[[argument]]

		if (
			!is.logical(value) ||
			length(value) != 1 ||
			is.na(value)
		) {
			stop(argument, " must be TRUE or FALSE.")
		}

	}

	# import data

	if (is.character(data) && length(data) == 1) {

		if (!file.exists(data)) {
			stop("The specified data file does not exist.")
		}

		data <- utils::read.csv(
			data,
			check.names = FALSE,
			stringsAsFactors = FALSE,
			colClasses = c(ID = "character")
		)

	}

	if (!is.data.frame(data)) {
		stop("data must be a data frame or CSV file path.")
	}

	data <- as.data.frame(data)

	if (anyDuplicated(names(data))) {
		stop("Duplicate column names detected.")
	}

	names(data) <- toupper(names(data))

	if (anyDuplicated(names(data))) {
		stop("Duplicate column names detected after standardisation.")
	}

	# participant identifiers

	required <- c("ID", "COUNTRY")

	if (!all(required %in% names(data))) {
		stop("The dataset must contain ID and COUNTRY.")
	}

	if (
		anyNA(data$ID) ||
		any(trimws(as.character(data$ID)) == "")
	) {
		stop("Missing participant identifiers detected.")
	}

	if (anyDuplicated(data$ID)) {
		stop("Duplicate participant identifiers detected.")
	}

	if ("WAVE" %in% names(data)) {
		stop("Input must be a combined wide-format GFS dataset.")
	}

	# identify variable structure

	dictionary <- gfs_dictionary()

	original_names <- names(data)

	annual <- grepl("_Y[123]$", original_names)

	base_names <- sub(
		"_Y[123]$",
		"",
		original_names
	)

	available_waves <- unique(as.integer(
		sub(
			"^.*_Y",
			"",
			original_names[annual]
		)
	))

	if (!all(waves %in% available_waves)) {
		stop("One or more requested waves are absent from the data.")
	}

	weights <- grepl(
		"^(ANNUAL_WEIGHT|RETENTION_WEIGHT)",
		original_names
	)

	design <- original_names %in% c("STRATA", "PSU")

	system <- base_names %in% c(
		"ID",
		"COUNTRY",
		"WAVE",
		"FULL_PARTIAL",
		"RECON_STATUS"
	)

	baseline_variables <- dictionary$variable[
		dictionary$year1 &
			!dictionary$year2 &
			!dictionary$year3 &
			!dictionary$weight
	]

	baseline <- annual &
		grepl("_Y1$", original_names) &
		base_names %in% baseline_variables

	# select variables

	if (!is.null(variables)) {

		if (!is.character(variables) || anyNA(variables)) {
			stop("variables must be a character vector.")
		}

		variables <- unique(toupper(variables))

		unknown <- setdiff(
			variables,
			unique(base_names)
		)

		if (length(unknown) > 0) {
			stop(
				"Variables not found: ",
				paste(unknown, collapse = ", ")
			)
		}

		selected <- base_names %in% variables

	} else {

		selected <- rep(TRUE, length(original_names))

	}

	selected <- selected | system

	selected[weights] <- keep_weights
	selected[design] <- keep_design

	requested_wave <- grepl(
		paste0("_Y[", paste(waves, collapse = ""), "]$"),
		original_names
	)

	selected <- selected &
		(!annual | requested_wave | baseline)

	data <- data[, selected, drop = FALSE]

	# recode documented missing values

	if (recode_missing) {

		missing_codes <- gfs_dictionary(
			type = "missing",
			release = release
		)

		protected <- c(
			"ID",
			"COUNTRY",
			"STRATA",
			"PSU"
		)

		for (variable in names(data)) {

			if (
				variable %in% protected ||
				grepl("^(ANNUAL_WEIGHT|RETENTION_WEIGHT)", variable)
			) {
				next
			}

			base <- sub("_Y[123]$", "", variable)

			codes <- missing_codes$value[
				missing_codes$variable == variable
			]

			if (length(codes) == 0) {

				codes <- missing_codes$value[
					missing_codes$variable == base
				]

			}

			if (length(codes) > 0) {

				index <- !is.na(data[[variable]]) &
					as.character(data[[variable]]) %in%
					as.character(codes)

				data[[variable]][index] <- NA

			}

		}

	}

	# country labels

	if (label_countries) {

		data$COUNTRY <- gfs_countries(
			data$COUNTRY,
			unknown = "error"
		)

	}

	# wide format

	if (format == "wide") {

		rownames(data) <- NULL

		return(data)

	}

	# long format

	annual_names <- names(data)[
		grepl("_Y[123]$", names(data))
	]

	baseline_names <- annual_names[
		grepl("_Y1$", annual_names) &
			sub("_Y1$", "", annual_names) %in%
			baseline_variables
	]

	repeated_names <- setdiff(
		annual_names,
		baseline_names
	)

	repeated_variables <- unique(
		sub("_Y[123]$", "", repeated_names)
	)

	static_names <- setdiff(
		names(data),
		repeated_names
	)

	collisions <- intersect(
		static_names,
		repeated_variables
	)

	if (length(collisions) > 0) {

		stop(
			"Conflicting static and wave-specific variables: ",
			paste(collisions, collapse = ", ")
		)

	}

	output <- vector("list", length(waves))

	for (i in seq_along(waves)) {

		wave <- waves[i]

		current <- data[, static_names, drop = FALSE]

		current$wave <- rep(wave, nrow(data))

		for (variable in repeated_variables) {

			source <- paste0(variable, "_Y", wave)

			if (source %in% names(data)) {

				current[[variable]] <- data[[source]]

			} else {

				current[[variable]] <- rep(NA, nrow(data))

			}

		}

		output[[i]] <- current

	}

	output <- dplyr::bind_rows(output)

	first <- c("ID", "COUNTRY", "wave")

	output <- output[, c(
		first,
		setdiff(names(output), first)
	), drop = FALSE]

	output <- output[
		order(output$ID, output$wave),
		,
		drop = FALSE
	]

	rownames(output) <- NULL

	output

}
