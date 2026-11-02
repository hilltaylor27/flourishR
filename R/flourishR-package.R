#' flourishR: Tools for Reproducible Wellbeing and Survey Data Analysis
#'
#' `flourishR` provides lightweight tools for common tasks in wellbeing
#' and survey data analysis, including data preparation, scale scoring,
#' missing-data checks, cross-national analysis, and longitudinal
#' visualisation.
#'
#' The package is designed to support transparent and reproducible
#' workflows with large-scale survey and longitudinal data.
#'
#' @section Data preparation:
#'
#' - [label_to_factor()] converts labelled survey variables to factors.
#' - [recode_missing()] recodes survey-specific missing-value codes to `NA`.
#' - [check_missingness()] summarises missingness across variables.
#'
#' @section Scale scoring:
#'
#' - [score_scale()] scores general multi-item scales.
#' - [score_swemwbs()] provides a wrapper for scoring the seven SWEMWBS items.
#'
#' @section Cross-national analysis:
#'
#' - [loop_countries()] applies an analysis function separately by country.
#' - [plot_cross_national()] visualises mean outcomes across countries.
#'
#' @section Longitudinal analysis:
#'
#' - [plot_longitudinal()] visualises outcome trajectories across waves.
#'
#' @seealso
#' See the package README for an example workflow combining the data
#' preparation, scoring, and visualisation functions.
#'
#' @keywords internal
"_PACKAGE"
