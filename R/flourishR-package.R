#' flourishR: Tools for Reproducible Wellbeing and Survey Data Analysis
#'
#' Tools for preparing, analysing and visualising cross-national and longitudinal
#' survey data, with dedicated support for the Global Flourishing Study (GFS).
#' Survey-weighted analyses use country-specific designs; mixed models are
#' unweighted. See the GFS workflow vignette for a reproducible example.
#' @section Data preparation:
#'
#' [label_to_factor()], [recode_missing()], [check_missingness()], [gfs_dictionary()], [gfs_countries()], [gfs_prepare()], [join_country_data()].
#'
#' @section Survey design and retention:
#'
#' [gfs_weights()], [gfs_design()], [check_retention()].
#'
#' @section Cross-national analysis:
#'
#' [describe_countries()], [loop_countries()], [country_moderation()], [meta_countries()].
#'
#' @section Longitudinal analysis:
#'
#' [create_lags()], [change_scores()], [within_between()], [loop_longitudinal()].
#'
#' @section Visualisation:
#'
#' [plot_cross_national()], [plot_longitudinal()], [plot_country_effects()], [plot_retention()], [plot_missingness()].
#'
#' @section Personality scoring:
#'
#' [score_tipi()].
#'
#' @keywords internal
"_PACKAGE"
