
# select a single wave and exclude nonparticipants
country_wave_data <- function(data, wave) {
  if (!is.data.frame(data)) stop('data must be a data frame.')
  if (!is.null(wave) && (length(wave) != 1L || is.na(wave) || !wave %in% 1:3)) stop('Specify one annual wave.')
  if ('wave' %in% names(data)) {
    if (is.null(wave)) stop('Specify wave for long-format data.')
    data <- data[!is.na(data$wave) & data$wave == wave, , drop = FALSE]
    if ('WAVE' %in% names(data)) data <- data[!is.na(data$WAVE) & data$WAVE == wave, , drop = FALSE]
  } else if (!is.null(wave) && paste0('WAVE_Y', wave) %in% names(data)) {
    v <- data[[paste0('WAVE_Y', wave)]]
    data <- data[!is.na(v) & v == wave, , drop = FALSE]
  }
  if ('ID' %in% names(data) && (anyNA(data$ID) || anyDuplicated(data$ID))) stop('Expected one record per participant.')
  data
}

country_models <- function(data, country_var, formula, wave, weighted, family, conf.level, ...) {
  if (!inherits(formula, 'formula')) stop('formula must be a formula.')
  if (!country_var %in% names(data)) stop('country_var must be a variable in data.')
  if (weighted && (country_var != 'COUNTRY' || is.null(wave))) stop('Weighted GFS models require COUNTRY and wave.')
  if (weighted && !requireNamespace('survey', quietly = TRUE)) stop('Install survey.')
  if (!all(all.vars(formula) %in% names(data))) stop('Formula variables not found.')
  if (length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) stop('Invalid confidence level.')
  data <- country_wave_data(data, wave)
  countries <- unique(data[[country_var]][!is.na(data[[country_var]])])
  if (!length(countries)) stop('No countries available.')
  results <- lapply(countries, function(country) {
    d <- data[!is.na(data[[country_var]]) & data[[country_var]] == country, , drop = FALSE]
    warnings <- character()
    out <- tryCatch(withCallingHandlers({
      fit <- if (weighted) {
        survey::svyglm(formula, design = gfs_design(d, country, waves = wave), family = family, ...)
      } else stats::glm(formula, data = d, family = family, ...)
      if (!isTRUE(fit$converged)) stop('Model did not converge.')
      b <- stats::coef(fit)
      se <- sqrt(diag(as.matrix(stats::vcov(fit))))[names(b)]
      df <- stats::df.residual(fit)
      q <- if (is.infinite(df)) stats::qnorm((1 + conf.level)/2) else if (df > 0) stats::qt((1 + conf.level)/2, df) else NA_real_
      data.frame(term = names(b), estimate = unname(b), std.error = unname(se),
                 conf.low = unname(b-q*se), conf.high = unname(b+q*se),
                 n = stats::nobs(fit), status = ifelse(is.finite(b) & is.finite(se) & se > 0 & is.finite(q), 'ok', 'not estimable'))
    }, warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') }),
    error = function(e) data.frame(term = NA_character_, estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_, conf.high = NA_real_, n = NA_integer_, status = conditionMessage(e)))
    out$warning <- paste(unique(warnings), collapse = '; ')
    out[[country_var]] <- country
    out[, c(country_var, setdiff(names(out), country_var)), drop = FALSE]
  })
  dplyr::bind_rows(results)
}

#' Describe numeric variables by GFS country
#' @param data Prepared GFS data.
#' @param variables Numeric variable names.
#' @param wave Annual wave.
#' @param weighted Use the annual cross-sectional survey weight and design.
#' @param conf.level Confidence level.
#' @return Data frame of country means, standard errors, intervals, sample sizes and status.
#' @export
#' @param lonely_psu Optional survey single-PSU variance method; NULL preserves current settings.
#' @examples
#' d <- data.frame(COUNTRY = rep(c("Japan", "Sweden"), each = 4), x = 1:8)
#' describe_countries(d, "x", weighted = FALSE)
describe_countries <- function(data, variables, wave = 1, weighted = TRUE, conf.level = 0.95, lonely_psu = NULL) {
  old_options <- set_lonely_psu(lonely_psu)
  on.exit(options(old_options), add = TRUE)
  if (!'COUNTRY' %in% names(data)) stop('COUNTRY is required.')
  if (!is.character(variables) || !length(variables) || anyNA(variables) || !all(variables %in% names(data))) stop('Invalid variables.')
  if (!all(vapply(data[variables], is.numeric, logical(1)))) stop('Variables must be numeric.')
  if (length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) stop('Invalid confidence level.')
  if (weighted && !requireNamespace('survey', quietly = TRUE)) stop('Install survey.')
  data <- country_wave_data(data, wave)
  countries <- unique(data$COUNTRY[!is.na(data$COUNTRY)])
  if (!length(countries)) stop('No countries available.')
  dplyr::bind_rows(lapply(countries, function(country) {
    d <- data[!is.na(data$COUNTRY) & data$COUNTRY == country, , drop = FALSE]
    design <- if (weighted) tryCatch(gfs_design(d, country, waves = wave), error = identity) else NULL
    dplyr::bind_rows(lapply(variables, function(v) {
      n <- NA_integer_
      warnings <- character()
      out <- tryCatch(withCallingHandlers({
        if (inherits(design, 'error')) stop(conditionMessage(design))
        x <- if (weighted) design$variables[[v]] else d[[v]]
        n <- sum(!is.na(x))
        if (n < 2) stop('Fewer than two observed values.')
        if (weighted) {
          fit <- survey::svymean(stats::reformulate(v), design, na.rm = TRUE)
          m <- as.numeric(stats::coef(fit)); se <- sqrt(as.numeric(stats::vcov(fit)))
          df <- survey::degf(design)
        } else {
          m <- mean(x, na.rm = TRUE); se <- stats::sd(x, na.rm = TRUE)/sqrt(n); df <- n-1
        }
        if (df <= 0 || !is.finite(se)) stop('Variance not estimable.')
        q <- stats::qt((1+conf.level)/2, df)
        data.frame(mean=m, std.error=se, conf.low=m-q*se, conf.high=m+q*se, status='ok')
      }, warning=function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning')}),
      error=function(e) data.frame(mean=NA_real_, std.error=NA_real_, conf.low=NA_real_, conf.high=NA_real_, status=conditionMessage(e)))
      data.frame(COUNTRY=country, wave=wave, variable=v, n=n, out, warning=paste(unique(warnings), collapse='; '))
    }))
  }))
}

# select comparable country coefficients; never silently omit failed countries
country_effect_data <- function(results, term, country_var) {
  required <- c(country_var, 'term', 'estimate', 'std.error', 'status')
  if (!is.data.frame(results) || !all(required %in% names(results))) stop('Invalid country model results.')
  if (length(term) != 1L || is.na(term)) stop('Specify one term.')
  d <- results[!is.na(results$term) & results$term == term, , drop=FALSE]
  if (anyNA(d[[country_var]]) || anyDuplicated(d[[country_var]])) stop('Expected one coefficient per country.')
  valid <- !is.na(d$status) & d$status == 'ok' & is.finite(d$estimate) & is.finite(d$std.error) & d$std.error > 0
  excluded <- setdiff(unique(as.character(results[[country_var]])), as.character(d[[country_var]][valid]))
  if (length(excluded)) warning('Countries excluded: ', paste(excluded, collapse=', '), call.=FALSE)
  d <- d[valid, , drop=FALSE]
  if (nrow(d) < 2) stop('At least two estimable country coefficients are required.')
  d
}

#' Test heterogeneity of country-specific coefficients
#' @param results Country model results from loop_countries.
#' @param term Coefficient name.
#' @param country_var Country column name.
#' @return Data frame containing Cochran Q, degrees of freedom and p-value.
#' @details Tests equality of independent, comparable country coefficients using an asymptotic inverse-variance test.
#' This is a heterogeneity test across separately fitted models, not a pooled
#' individual-level country interaction model.
#' @export
#' @examples
#' r <- data.frame(COUNTRY = c("Japan", "Sweden", "Brazil"), term = "x",
#'                 estimate = c(.2, .4, .3), std.error = .1, status = "ok")
#' country_moderation(r, "x")
country_moderation <- function(results, term, country_var='COUNTRY') {
  d <- country_effect_data(results, term, country_var)
  w <- 1/d$std.error^2
  q <- sum(w*(d$estimate-sum(w*d$estimate)/sum(w))^2)
  df <- nrow(d)-1L
  data.frame(term=term, k=nrow(d), Q=q, df=df, p.value=stats::pchisq(q, df, lower.tail=FALSE))
}

#' Pool country-specific coefficients
#' @param results Country model results from loop_countries.
#' @param term Coefficient name.
#' @param country_var Country column name.
#' @param conf.level Confidence level.
#' @return Data frame containing REML pooled estimate, confidence interval and heterogeneity statistics.
#' @details Uses random-effects meta-analysis with Knapp-Hartung inference. Estimates describe the distribution of included country effects, not a population-weighted global effect. Coefficients must be on comparable scales with consistent model coding.
#' @export
#' @param moderators Optional one-sided formula for country-level meta-regression using columns in results.
#' @examples
#' r <- data.frame(COUNTRY = c("Japan", "Sweden", "Brazil"), term = "x",
#'                 estimate = c(.2, .4, .3), std.error = .1, status = "ok")
#' if (requireNamespace("metafor", quietly = TRUE)) meta_countries(r, "x")
meta_countries <- function(results, term, country_var='COUNTRY', conf.level=0.95, moderators = NULL) {
  if (!is.null(moderators)) return(country_meta_regression(results, term, country_var, conf.level, moderators))
  if (!requireNamespace('metafor', quietly=TRUE)) stop('Install metafor.')
  if (length(conf.level)!=1L || !is.finite(conf.level) || conf.level<=0 || conf.level>=1) stop('Invalid confidence level.')
  d <- country_effect_data(results, term, country_var)
  fit <- metafor::rma.uni(yi=d$estimate, sei=d$std.error, method='REML', test='knha', level=100*conf.level)
  data.frame(term=term, k=fit$k, estimate=as.numeric(stats::coef(fit)), std.error=fit$se,
             conf.low=fit$ci.lb, conf.high=fit$ci.ub, tau2=fit$tau2, I2=fit$I2,
             Q=fit$QE, p.heterogeneity=fit$QEp)
}
