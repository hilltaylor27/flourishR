
set_lonely_psu <- function(method) {
  if (is.null(method)) return(options('survey.lonely.psu'))
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c('fail', 'adjust', 'average', 'certainty', 'remove')) stop('Invalid lonely_psu method.')
  options(survey.lonely.psu = method)
}

panel_check <- function(data, variables, id, wave) {
  if (!is.data.frame(data) || !is.character(variables) || !length(variables) || anyNA(variables) ||
      anyDuplicated(variables) || !all(c(id, wave, variables) %in% names(data))) stop('Invalid panel columns.')
  if (anyNA(data[[id]]) || anyNA(data[[wave]]) || anyDuplicated(data[c(id, wave)])) stop('Expected unique nonmissing participant-wave keys.')
  if (!is.numeric(data[[wave]]) || any(!is.finite(data[[wave]]) | data[[wave]] != floor(data[[wave]]))) stop('Wave must contain finite integers.')
  invisible(NULL)
}

panel_match <- function(data, id, wave, target) {
  lookup <- data.frame(.id = data[[id]], .wave = data[[wave]], .row = seq_len(nrow(data)))
  keys <- data.frame(.id = data[[id]], .wave = target)
  dplyr::left_join(keys, lookup, by = c('.id', '.wave'))$.row
}

panel_names <- function(data, new) {
  if (anyDuplicated(new) || any(new %in% names(data))) stop('Generated column names already exist.')
}

#' Create exact-wave lagged variables
#' @param data Long-format participant-wave data.
#' @param variables Character vector of variables.
#' @param id Participant identifier column.
#' @param wave Integer wave column.
#' @param lag Positive integer number of waves.
#' @return Original data with columns ending in _lag followed by the lag number. Missing target waves produce missing values.
#' @export
#' @examples
#' d <- data.frame(ID = rep(1:2, each = 3), wave = rep(1:3, 2), x = 1:6)
#' create_lags(d, "x")
create_lags <- function(data, variables, id = 'ID', wave = 'wave', lag = 1L) {
  panel_check(data, variables, id, wave)
  if (!is.numeric(lag) || length(lag) != 1L || !is.finite(lag) || lag < 1 || lag != floor(lag)) stop('lag must be a positive integer.')
  new <- paste0(variables, '_lag', lag)
  panel_names(data, new)
  idx <- panel_match(data, id, wave, data[[wave]] - lag)
  for (i in seq_along(variables)) data[[new[i]]] <- data[[variables[i]]][idx]
  data
}

#' Calculate change from an earlier wave
#' @inheritParams create_lags
#' @param from Earlier integer wave.
#' @param to Later integer wave.
#' @return Rows at the later wave with change columns named variable_change_from_to. Change is later minus earlier.
#' @export
#' @examples
#' d <- data.frame(ID = rep(1:2, each = 3), wave = rep(1:3, 2), x = 1:6)
#' change_scores(d, "x")
change_scores <- function(data, variables, from = 1L, to = 3L, id = 'ID', wave = 'wave') {
  panel_check(data, variables, id, wave)
  if (!is.numeric(from) || !is.numeric(to) || length(from) != 1L || length(to) != 1L ||
      any(!is.finite(c(from,to))) || any(c(from,to) != floor(c(from,to))) || from >= to) stop('Specify ordered integer waves.')
  if (!all(c(from,to) %in% data[[wave]])) stop('Requested waves not present.')
  if (!all(vapply(data[variables], is.numeric, logical(1)))) stop('Variables must be numeric.')
  new <- paste0(variables, '_change_', from, '_', to)
  panel_names(data, new)
  idx <- panel_match(data, id, wave, rep(from, nrow(data)))
  for (i in seq_along(variables)) data[[new[i]]] <- data[[variables[i]]] - data[[variables[i]]][idx]
  data[data[[wave]] == to, , drop = FALSE]
}

#' Decompose time-varying variables into within- and between-person components
#' @inheritParams create_lags
#' @return Original data with _between person means and _within deviations. Means use observed values in the supplied panel.
#' @details Person means are calculated before any subsequent model-specific
#' complete-case selection. Restrict the panel first if means should reflect
#' only the intended analytical sample. A single observed value gives a zero
#' within-person deviation; entirely missing histories give missing components.
#' @export
#' @examples
#' d <- data.frame(ID = rep(1:2, each = 3), wave = rep(1:3, 2), x = 1:6)
#' within_between(d, "x")
within_between <- function(data, variables, id = 'ID', wave = 'wave') {
  panel_check(data, variables, id, wave)
  if (!all(vapply(data[variables], is.numeric, logical(1)))) stop('Variables must be numeric.')
  panel_names(data, c(paste0(variables,'_between'), paste0(variables,'_within')))
  for (v in variables) {
    m <- stats::ave(data[[v]], data[[id]], FUN = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
    data[[paste0(v,'_between')]] <- m
    data[[paste0(v,'_within')]] <- data[[v]] - m
  }
  data
}

#' Fit prospective country-specific survey regressions
#' @param data Prepared long-format GFS data, including any lagged or change variables used in the formula.
#' @param formula Model formula evaluated at the final selected wave.
#' @param waves Annual waves defining longitudinal weight eligibility; either 1:2 or 1:3.
#' @param family GLM family.
#' @param conf.level Confidence level.
#' @param lonely_psu Optional single-PSU variance method; NULL preserves current settings.
#' @param ... Additional arguments to survey::svyglm.
#' @return Country coefficient table, with one model per country and one record per eligible participant in each model.
#' @details Prospective mode uses the longitudinal weight for the selected waves
#' and fits endpoint regressions. Prepare lagged predictors before calling.
#' Mixed mode fits unweighted Gaussian participant random-intercept models
#' with Wald confidence intervals. It does not incorporate the GFS survey design.
#' Singularity is reported separately from other convergence problems.
#' In prospective mode, lonely_psu controls the survey variance adjustment
#' temporarily; the caller's setting is restored. The appropriate adjustment
#' depends on the sampling design.
#' @export
#' @param model Prospective survey regression or unweighted Gaussian random-intercept model.
#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   d <- data.frame(ID = rep(1:12, each = 2), COUNTRY = "Japan",
#'                   wave = rep(1:2, 12), STRATA = rep(rep(1:2, each = 6), each = 2),
#'                   PSU = rep(rep(1:6, each = 2), each = 2), ANNUAL_WEIGHT_L2 = 1)
#'   d$x <- rep(1:12, each = 2)
#'   d$y <- d$x + sin(d$ID) + d$wave
#'   loop_longitudinal(d, y ~ x, waves = 1:2)
#' }
loop_longitudinal <- function(data, formula, waves = 1:3, family = stats::gaussian(), conf.level = .95, lonely_psu = NULL, ..., model = c('prospective', 'mixed')) {
  model <- match.arg(model)
  if (model == 'mixed') {
    if (!identical(family$family, 'gaussian') || !identical(family$link, 'identity')) stop('Mixed mode requires Gaussian identity family.')
    return(country_mixed(data, formula, waves, conf.level, ...))
  }
  if (!requireNamespace('survey', quietly = TRUE)) stop('Install survey.')
  if (!inherits(formula,'formula') || !all(all.vars(formula) %in% names(data))) stop('Invalid formula.')
  if (!all(c('ID','COUNTRY','wave') %in% names(data))) stop('Prepared long-format GFS data required.')
  if (length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) stop('Invalid confidence level.')
  weight <- gfs_weights(waves, type = 'longitudinal', data = data)
  old <- set_lonely_psu(lonely_psu)
  on.exit(options(old), add = TRUE)
  d <- country_wave_data(data, max(waves))
  countries <- unique(d$COUNTRY[!is.na(d$COUNTRY)])
  if (!length(countries)) stop('No countries available.')
  out <- dplyr::bind_rows(lapply(countries, function(country) {
    warnings <- character()
    r <- tryCatch(withCallingHandlers({
      design <- gfs_design(d, country, waves = waves, type = 'longitudinal')
      fit <- survey::svyglm(formula, design = design, family = family, ...)
      if (!isTRUE(fit$converged)) stop('Model did not converge.')
      b <- stats::coef(fit)
      se <- sqrt(diag(as.matrix(stats::vcov(fit))))[names(b)]
      df <- stats::df.residual(fit)
      q <- if (is.infinite(df)) stats::qnorm((1+conf.level)/2) else if (df > 0) stats::qt((1+conf.level)/2,df) else NA_real_
      data.frame(term=names(b), estimate=unname(b), std.error=unname(se), conf.low=unname(b-q*se), conf.high=unname(b+q*se), n=stats::nobs(fit), status=ifelse(is.finite(b)&is.finite(se)&se>0&is.finite(q),'ok','not estimable'))
    }, warning=function(w) {warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')}),
    error=function(e) data.frame(term=NA_character_,estimate=NA_real_,std.error=NA_real_,conf.low=NA_real_,conf.high=NA_real_,n=NA_integer_,status=conditionMessage(e)))
    data.frame(COUNTRY=country,r,warning=paste(unique(warnings),collapse='; '),weight=weight)
  }))
  rownames(out) <- NULL
  out$lonely_psu <- getOption('survey.lonely.psu','fail')
  out
}
