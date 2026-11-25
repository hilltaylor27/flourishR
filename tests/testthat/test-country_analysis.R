
test_that('grouped missingness retains group denominators', {
  d <- data.frame(country=c('A','A','B'), wave=c(1,1,1), x=c(1,NA,2))
  r <- check_missingness(d, 'x', by=c('country','wave'))
  expect_equal(r$n, c(2L,1L))
  expect_equal(r$pct_missing,c(50,0))
})

country_analysis_data <- function() {
  data.frame(ID=1:12, COUNTRY=rep(c(1,9),each=6), wave=1, WAVE=1,
             STRATA=1, PSU=1:12, ANNUAL_WEIGHT_C1=1,
             x=rep(1:6,2), y=c(2,5,5,8,11,11,3,4,8,10,11,15))
}

test_that('formula mode agrees with direct glm', {
  d <- country_analysis_data()
  r <- loop_countries(d,'COUNTRY',formula=y~x,wave=1)
  fit <- stats::glm(y~x,data=d[d$COUNTRY==1,])
  expect_equal(r$estimate[r$COUNTRY==1], unname(stats::coef(fit)))
  expect_true(all(r$status=='ok'))
  expect_equal(r$n,rep(6L,4))
  expect_error(loop_countries(d,'COUNTRY',formula=y~x))
})

test_that('weighted models agree with direct survey models', {
  skip_if_not_installed('survey')
  d <- country_analysis_data()
  r <- loop_countries(d,'COUNTRY',formula=y~x,wave=1,weighted=TRUE)
  fit <- survey::svyglm(y~x,design=gfs_design(d,1))
  expect_equal(r$estimate[r$COUNTRY==1],unname(stats::coef(fit)))
  expect_equal(r$std.error[r$COUNTRY==1],unname(sqrt(diag(stats::vcov(fit)))))
})

test_that('country descriptions use the requested wave', {
  d <- country_analysis_data()
  r <- describe_countries(d,'y',weighted=FALSE)
  expect_equal(r$mean,c(mean(d$y[1:6]),mean(d$y[7:12])))
  expect_equal(r$n,c(6L,6L))
  expect_error(describe_countries(d,'unknown'))
  skip_if_not_installed('survey')
  w <- describe_countries(d,'y')
  expect_equal(w$mean,r$mean)
  expect_equal(w$std.error[1],sqrt(as.numeric(stats::vcov(survey::svymean(~y,gfs_design(d,1))))))
})

test_that('heterogeneity uses inverse variance weights', {
  d <- data.frame(COUNTRY=1:3,term='x',estimate=c(1,2,3),std.error=1,status='ok')
  r <- country_moderation(d,'x')
  expect_equal(r$Q,2)
  expect_equal(r$df,2)
  expect_equal(r$p.value,stats::pchisq(2,2,lower.tail=FALSE))
  expect_error(suppressWarnings(country_moderation(d,'unknown')), 'At least two')
  expect_error(country_moderation(rbind(d,d[1,]),'x'))
})

test_that('meta-analysis agrees with direct metafor', {
  skip_if_not_installed('metafor')
  d <- data.frame(COUNTRY=1:3,term='x',estimate=c(1,2,3),std.error=c(.2,.3,.4),status='ok')
  r <- meta_countries(d,'x')
  fit <- metafor::rma.uni(yi=d$estimate,sei=d$std.error,method='REML',test='knha')
  expect_equal(r$estimate,as.numeric(stats::coef(fit)))
  expect_equal(r$tau2,fit$tau2)
  expect_equal(r$conf.low,fit$ci.lb)
})
