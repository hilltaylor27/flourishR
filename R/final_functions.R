
#' Join country-level contextual data
#' @param data Participant or country-level data.
#' @param context One row per country containing contextual variables.
#' @param country_var Country column in data.
#' @param context_country Country column in context.
#' @param gfs_labels Translate GFS codes to labels before matching.
#' @param unmatched Treatment of unmatched countries.
#' @return Data with contextual columns appended, preserving row order.
#' @export
#' @examples
#' d <- data.frame(COUNTRY = c("Japan", "Sweden"), x = 1:2)
#' context <- data.frame(COUNTRY = c("Japan", "Sweden"), z = c(3, 4))
#' join_country_data(d, context)
join_country_data <- function(data, context, country_var='COUNTRY', context_country=country_var, gfs_labels=TRUE, unmatched=c('error','keep')) {
  unmatched <- match.arg(unmatched)
  if (!is.data.frame(data) || !is.data.frame(context) || !country_var %in% names(data) || !context_country %in% names(context)) stop('Invalid country columns.')
  key <- function(x) if (gfs_labels) gfs_countries(x,unknown='error') else as.character(x)
  a <- key(data[[country_var]]); b <- key(context[[context_country]])
  if (anyNA(b) || anyDuplicated(b)) stop('Context must have one nonmissing key per country.')
  cols <- setdiff(names(context),context_country)
  if (any(cols %in% names(data))) stop('Context column names overlap data.')
  idx <- match(a,b)
  if (unmatched=='error' && anyNA(idx)) stop('Unmatched countries: ',paste(unique(a[is.na(idx)]),collapse=', '))
  for (v in cols) data[[v]] <- context[[v]][idx]
  data
}

mixed_convergence_problem <- function(convergence) {
  messages <- as.character(convergence$lme4$messages)
  other <- messages[!startsWith(messages, 'boundary (singular) fit')]
  code <- convergence$lme4$code
  opt <- convergence$opt
  length(other) > 0L || anyNA(opt) || any(opt != 0) ||
    (length(code) > 0L && (anyNA(code) || any(code != 0)))
}

country_mixed <- function(data, formula, waves, conf.level, ...) {
  if (!requireNamespace('lme4',quietly=TRUE)) stop('Install lme4.')
  if (!inherits(formula,'formula') || length(formula)!=3L || length(lme4::findbars(formula))) stop('Supply a fixed-effects formula.')
  if (!all(c('ID','COUNTRY','wave',all.vars(formula)) %in% names(data))) stop('Missing model columns.')
  if (anyNA(data[c('ID','wave','COUNTRY')]) || anyDuplicated(data[c('ID','wave')])) stop('Invalid panel keys.')
  if (!is.numeric(waves) || !length(waves) || anyNA(waves) || !all(waves %in% 1:3)) stop('Invalid waves.')
  if (length(conf.level)!=1L || !is.finite(conf.level) || conf.level<=0 || conf.level>=1) stop('Invalid confidence level.')
  dots <- list(...)
  if (any(names(dots) %in% c('weights','subset','data','formula'))) stop('Mixed mode does not accept weights or subset; filter data first.')
  d <- data[data$wave %in% waves,,drop=FALSE]
  if ('WAVE' %in% names(d)) d <- d[!is.na(d$WAVE),,drop=FALSE]
  f <- stats::update(formula, . ~ . + (1|ID))
  q <- stats::qnorm((1+conf.level)/2)
  out <- lapply(unique(d$COUNTRY),function(country) {
    warnings <- character()
    r <- tryCatch(withCallingHandlers({
      fit <- lme4::lmer(f,data=d[d$COUNTRY==country,,drop=FALSE],...)
      b <- lme4::fixef(fit,add.dropped=TRUE)
      se <- sqrt(diag(as.matrix(stats::vcov(fit))))[names(b)]
      singular <- lme4::isSingular(fit)
      convergence <- fit@optinfo$conv
      bad <- mixed_convergence_problem(convergence)
      data.frame(term=names(b),estimate=unname(b),std.error=unname(se),conf.low=unname(b-q*se),conf.high=unname(b+q*se),n=stats::nobs(fit),n_people=unname(lme4::ngrps(fit)[1]),status=ifelse(bad,'nonconverged',ifelse(is.finite(b)&is.finite(se)&se>0,'ok','not estimable')),singular=singular)
    },warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart('muffleWarning')}),error=function(e) data.frame(term=NA_character_,estimate=NA_real_,std.error=NA_real_,conf.low=NA_real_,conf.high=NA_real_,n=NA_integer_,n_people=NA_integer_,status=conditionMessage(e),singular=NA))
    data.frame(COUNTRY=country,r,warning=paste(unique(warnings),collapse='; '),model='mixed',weight=NA_character_)
  })
  dplyr::bind_rows(out)
}

country_meta_regression <- function(results,term,country_var,conf.level,moderators) {
  if (!requireNamespace('metafor',quietly=TRUE)) stop('Install metafor.')
  if (!inherits(moderators,'formula') || length(moderators)!=2L) stop('Supply a one-sided moderator formula.')
  if (length(conf.level)!=1L || !is.finite(conf.level) || conf.level<=0 || conf.level>=1) stop('Invalid confidence level.')
  d <- country_effect_data(results,term,country_var)
  mf <- stats::model.frame(moderators,d,na.action=stats::na.pass)
  keep <- stats::complete.cases(mf)
  if (any(!keep)) warning('Missing moderators: ',paste(d[[country_var]][!keep],collapse=', '),call.=FALSE)
  d <- d[keep,,drop=FALSE]
  mm <- stats::model.matrix(moderators,d)
  if (any(!is.finite(mm)) || qr(mm)$rank<ncol(mm) || nrow(mm)<=ncol(mm)) stop('Insufficient countries or rank-deficient moderators.')
  fit <- metafor::rma.uni(yi=d$estimate,sei=d$std.error,mods=moderators,data=d,method='REML',test='knha',level=100*conf.level)
  b <- stats::coef(fit)
  data.frame(term=term,moderator=names(b),k=fit$k,estimate=unname(b),std.error=fit$se,conf.low=fit$ci.lb,conf.high=fit$ci.ub,p.value=fit$pval,tau2=fit$tau2,I2=fit$I2,Q=fit$QE,p.heterogeneity=fit$QEp)
}

plot_require <- function(data,cols) {
  if (!is.data.frame(data) || !all(cols %in% names(data))) stop('Missing plotting columns.')
}

country_plot_data <- function(data, country_var, outcome_var, wave_var = NULL) {
  d <- data[which(data$variable == outcome_var), , drop = FALSE]
  keep <- !is.na(d$status) & d$status == 'ok' & is.finite(d$mean) &
    is.finite(d$conf.low) & is.finite(d$conf.high)
  if (any(!keep)) {
    keys <- as.character(d[[country_var]][!keep])
    if (!is.null(wave_var)) keys <- paste0(keys, ' (wave ', d[[wave_var]][!keep], ')')
    warning('Country estimates omitted: ', paste(unique(keys), collapse = ', '), call. = FALSE)
  }
  d[keep, , drop = FALSE]
}

country_mean_plot <- function(data,country_var,outcome_var,plot_title,y_label) {
  plot_require(data,c(country_var,'variable','mean','conf.low','conf.high','status'))
  d <- country_plot_data(data, country_var, outcome_var)
  if (!nrow(d) || anyDuplicated(d[[country_var]])) stop('Select one wave with one estimate per country.')
  d$.country <- factor(d[[country_var]],levels=d[[country_var]][order(d$mean)])
  ggplot2::ggplot(d,ggplot2::aes(x=.data$mean,y=.data$.country)) + ggplot2::geom_errorbar(ggplot2::aes(xmin=.data$conf.low,xmax=.data$conf.high),orientation='y',width=.15) + ggplot2::geom_point() + ggplot2::labs(x=if(is.null(y_label)) outcome_var else y_label,y=NULL,title=plot_title) + ggplot2::theme_minimal()
}

country_trajectory_plot <- function(data,country_var,wave_var,outcome_var,estimates,plot_title,y_label) {
  if (estimates) {
    plot_require(data,c(country_var,wave_var,'variable','mean','conf.low','conf.high','status'))
    d <- country_plot_data(data, country_var, outcome_var, wave_var)
  } else {
    plot_require(data,c(country_var,wave_var,outcome_var))
    d <- dplyr::group_by(data,.data[[country_var]],.data[[wave_var]])
    d <- dplyr::summarise(d,mean=mean(.data[[outcome_var]],na.rm=TRUE),n=sum(!is.na(.data[[outcome_var]])),se=stats::sd(.data[[outcome_var]],na.rm=TRUE)/sqrt(.data$n),.groups='drop')
    d$conf.low <- d$mean-1.96*d$se; d$conf.high <- d$mean+1.96*d$se
    d <- d[d$n>0,,drop=FALSE]
  }
  if (!nrow(d) || anyDuplicated(d[c(country_var,wave_var)])) stop('Expected one estimate per country-wave.')
  ggplot2::ggplot(d,ggplot2::aes(x=.data[[wave_var]],y=.data$mean,group=.data[[country_var]])) + ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$conf.low,ymax=.data$conf.high),width=.1) + ggplot2::facet_wrap(stats::reformulate(country_var)) + ggplot2::labs(x='Wave',y=if(is.null(y_label)) outcome_var else y_label,title=plot_title,caption=if(!estimates) 'Unweighted means and approximate pointwise 95% confidence intervals' else NULL) + ggplot2::theme_minimal()
}

#' Plot country-specific coefficients
#' @param results Country model results.
#' @param term Coefficient to plot.
#' @param pooled Optional intercept-only meta_countries result for the same term.
#' @param country_var Country column.
#' @param x_label Coefficient axis label.
#' @return A ggplot object.
#' @export
#' @examples
#' r <- data.frame(COUNTRY = c("Japan", "Sweden"), term = "x",
#'                 estimate = c(.2, .4), std.error = .1, status = "ok",
#'                 conf.low = c(.004, .204), conf.high = c(.396, .596))
#' plot_country_effects(r, "x")
plot_country_effects <- function(results,term,pooled=NULL,country_var='COUNTRY',x_label='Regression coefficient') {
  d <- country_effect_data(results,term,country_var)
  plot_require(d,c('conf.low','conf.high'))
  d$.country <- as.character(d[[country_var]])
  d <- d[order(d$estimate),,drop=FALSE]
  d$.pooled <- FALSE
  if (!is.null(pooled)) {
    plot_require(pooled,c('term','k','estimate','conf.low','conf.high'))
    if (nrow(pooled)!=1L || pooled$term!=term || pooled$k!=nrow(d) || 'moderator' %in% names(pooled)) stop('Pooled result must match country effects.')
    d <- dplyr::bind_rows(d,data.frame(.country='Pooled',estimate=pooled$estimate,conf.low=pooled$conf.low,conf.high=pooled$conf.high,.pooled=TRUE))
  }
  d$.country <- factor(d$.country,levels=c(if(!is.null(pooled)) 'Pooled',as.character(d$.country[!d$.pooled])))
  ggplot2::ggplot(d,ggplot2::aes(x=.data$estimate,y=.data$.country,shape=.data$.pooled)) + ggplot2::geom_vline(xintercept=0,linetype=2,colour='grey60') + ggplot2::geom_errorbar(ggplot2::aes(xmin=.data$conf.low,xmax=.data$conf.high),orientation='y',width=.15) + ggplot2::geom_point(size=2) + ggplot2::scale_shape_manual(values=c('FALSE'=16,'TRUE'=18),guide='none') + ggplot2::labs(x=x_label,y=NULL) + ggplot2::theme_minimal()
}

#' Plot longitudinal retention
#' @param data Output of check_retention.
#' @param group Group column.
#' @param cumulative Plot cumulative rather than wave-specific retention.
#' @return A ggplot object.
#' @export
#' @examples
#' d <- data.frame(ID = rep(1:3, each = 2), wave = rep(1:2, 3), WAVE = c(1,2,1,2,1,NA))
#' plot_retention(check_retention(d))
plot_retention <- function(data,group='group',cumulative=FALSE) {
  v <- if(cumulative) 'cumulative_pct' else 'retention_pct'
  plot_require(data,c(group,'wave',v))
  ggplot2::ggplot(data,ggplot2::aes(x=.data$wave,y=.data[[v]],group=.data[[group]])) + ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::facet_wrap(stats::reformulate(group)) + ggplot2::coord_cartesian(ylim=c(0,100)) + ggplot2::labs(x='Wave',y='Retention (%)') + ggplot2::theme_minimal()
}

#' Plot item missingness
#' @param data Grouped check_missingness output.
#' @param country_var Country column.
#' @param wave_var Optional wave column.
#' @return A ggplot object.
#' @export
#' @examples
#' d <- data.frame(COUNTRY = rep(c("Japan", "Sweden"), each = 2), wave = 1, x = c(1, NA, 2, 3))
#' plot_missingness(check_missingness(d, "x", by = c("COUNTRY", "wave")))
plot_missingness <- function(data,country_var='COUNTRY',wave_var='wave') {
  plot_require(data,c(country_var,'variable','pct_missing',wave_var))
  if (anyDuplicated(data[c(country_var,'variable',wave_var)])) stop('Expected unique plotting keys.')
  p <- ggplot2::ggplot(data,ggplot2::aes(x=.data$variable,y=.data[[country_var]],fill=.data$pct_missing)) + ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low='#F5F2F6',high='#713B76',name='Missing (%)') + ggplot2::labs(x=NULL,y=NULL) + ggplot2::theme_minimal() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))
  if (!is.null(wave_var)) p <- p + ggplot2::facet_wrap(stats::reformulate(wave_var))
  p
}
