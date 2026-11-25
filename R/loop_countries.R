#' Apply an analysis function across countries
#'
#' Applies a user-supplied analysis function separately to each country
#' represented in a data frame and combines the results into a single
#' output.
#'
#' Missing values in the country variable are excluded. Additional
#' arguments supplied through `...` are passed to the analysis function.
#'
#' @param data A data frame.
#' @param country_var A character value giving the name of the variable
#'   identifying countries.
#' @param analysis_function A function to apply separately to the data for
#'   each country. The function must accept a `data` argument and return an
#'   object that can be combined using `dplyr::bind_rows()`.
#' @param ... Additional arguments passed to `analysis_function`.
#'
#' @param formula Optional model formula, instead of analysis_function.
#' @param wave Annual wave to analyse.
#' @param weighted Use a country-specific GFS survey design.
#' @param family GLM family.
#' @param conf.level Confidence level.
#' @details In formula mode, returns coefficient estimates, standard errors, confidence intervals, sample sizes and model status. Additional arguments are passed to glm or svyglm.
#' @return A data frame containing the combined outputs from
#'   `analysis_function`, with the country variable as the first column.
#'
#' @examples
#' dat <- data.frame(
#'   country = c(
#'     "England", "England",
#'     "Scotland", "Scotland",
#'     "Wales", "Wales"
#'   ),
#'   happiness = c(7, NA, 6, 7, 5, NA)
#' )
#'
#' mean_happiness <- function(data) {
#'   data.frame(
#'     mean_happiness = mean(data$happiness, na.rm = TRUE)
#'   )
#' }
#'
#' loop_countries(
#'   data = dat,
#'   country_var = "country",
#'   analysis_function = mean_happiness
#' )
#'
#' @export
#' @param lonely_psu Optional survey single-PSU variance method; NULL preserves current settings.
loop_countries <- function(data,
                           country_var,
                           analysis_function = NULL,
                           ..., formula = NULL, wave = NULL, weighted = FALSE, family = stats::gaussian(), conf.level = 0.95, lonely_psu = NULL) {
  old_options <- set_lonely_psu(lonely_psu)
  on.exit(options(old_options), add = TRUE)
  if (!is.null(formula)) {
    if (!is.null(analysis_function)) stop("Supply formula or analysis_function, not both.")
    return(country_models(data, country_var, formula, wave, weighted, family, conf.level, ...))
  }
  if (weighted || !is.null(wave)) stop("wave and weighted require formula mode.")

  # Check inputs

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!country_var %in% names(data)) {
    stop("country_var must be a variable in data.")
  }

  if (!is.function(analysis_function)) {
    stop("analysis_function must be a function.")
  }


  # Identify countries

  countries <- unique(data[[country_var]])

  countries <- countries[!is.na(countries)]


  # Run analysis by country

  output_list <- list()

  for (i in seq_along(countries)) {

    current_country <- countries[i]

    country_data <- data[
      !is.na(data[[country_var]]) &
        data[[country_var]] == current_country,
      ,
      drop = FALSE
    ]

    country_output <- analysis_function(
      data = country_data,
      ...
    )

    country_output[[country_var]] <- current_country

    output_list[[i]] <- country_output

  }


  # Bind outputs

  output <- dplyr::bind_rows(output_list)


  # Move country variable to front

  output <- dplyr::relocate(
    output,
    dplyr::all_of(country_var)
  )


  # Return output

  return(output)

}
