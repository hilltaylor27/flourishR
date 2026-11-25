#' Examine longitudinal survey retention
#'
#' Summarise survey participation and retention across
#' longitudinal waves, optionally within groups.
#'
#' @param data Long-format longitudinal data.
#' @param id Character. Participant identifier variable.
#' @param wave Character. Survey wave variable.
#' @param participation Character. Participation indicator
#'   variable. By default, nonmissing values indicate
#'   participation.
#' @param participated_values Optional vector of values
#'   indicating participation.
#' @param group Optional grouping variable.
#' @param baseline Baseline wave identifier.
#'
#' @details
#' Retention is calculated relative to participants observed
#' at baseline. Cumulative retention requires participation
#' in every supplied wave from baseline to the current wave.
#'
#' For prepared GFS data, the WAVE variable identifies
#' participation in each annual survey.
#'
#' @return A data frame containing retention statistics.
#'
#' @examples
#' data <- data.frame(
#'   ID = rep(1:3, each = 3),
#'   wave = rep(1:3, 3),
#'   WAVE = c(1, 2, 3, 1, NA, 3, 1, 2, NA)
#' )
#'
#' check_retention(data)
#'
#' @export

check_retention <- function(
		data,
		id = "ID",
		wave = "wave",
		participation = "WAVE",
		participated_values = NULL,
		group = NULL,
		baseline = 1
) {

	if (!is.data.frame(data)) {
		stop("data must be a data frame.")
	}

	required <- c(id, wave, participation, group)

	if (!all(required %in% names(data))) {

		stop(
			"Variables not found: ",
			paste(
				setdiff(required, names(data)),
				collapse = ", "
			)
		)

	}

	if (anyNA(data[[id]]) || anyNA(data[[wave]])) {
		stop("Participant and wave identifiers cannot be missing.")
	}

	if (anyDuplicated(data[, c(id, wave)])) {
		stop("Duplicate participant-wave records detected.")
	}

	waves <- sort(unique(data[[wave]]))

	if (!baseline %in% waves) {
		stop("Baseline wave not found.")
	}

	waves <- waves[waves >= baseline]

	# identify participation

	observed <- if (is.null(participated_values)) {

		!is.na(data[[participation]])

	} else {

		!is.na(data[[participation]]) &
			data[[participation]] %in% participated_values

	}

	# establish baseline cohort

	baseline_rows <- data[[wave]] == baseline & observed

	cohort <- data[baseline_rows, , drop = FALSE]

	if (nrow(cohort) == 0) {
		stop("No participating baseline records found.")
	}

	if (is.null(group)) {

		groups <- list(Overall = cohort[[id]])

	} else {

		if (anyNA(cohort[[group]])) {
			stop("Baseline grouping values cannot be missing.")
		}

		groups <- split(
			cohort[[id]],
			cohort[[group]],
			drop = TRUE
		)

	}

	# summarise retention

	results <- list()
	index <- 1L

	for (group_name in names(groups)) {

		cohort_ids <- groups[[group_name]]

		n_baseline <- length(cohort_ids)

		cumulative_ids <- cohort_ids

		for (current_wave in waves) {

			current_ids <- data[[id]][
				data[[wave]] == current_wave &
					observed
			]

			retained_ids <- intersect(
				cohort_ids,
				current_ids
			)

			cumulative_ids <- intersect(
				cumulative_ids,
				current_ids
			)

			n_retained <- length(retained_ids)

			n_cumulative <- length(cumulative_ids)

			results[[index]] <- data.frame(
				group = group_name,
				wave = current_wave,
				n_baseline = n_baseline,
				n_participants = n_retained,
				n_nonparticipants = n_baseline - n_retained,
				retention_pct = 100 * n_retained / n_baseline,
				n_cumulative = n_cumulative,
				cumulative_pct = 100 * n_cumulative / n_baseline
			)

			index <- index + 1L

		}

	}

	result <- dplyr::bind_rows(results)

	if (!is.null(group)) {
		names(result)[names(result) == "group"] <- group
	}

	rownames(result) <- NULL

	result

}
