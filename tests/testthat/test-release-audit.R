test_that('singularity does not mask other convergence problems', {
  singular <- 'boundary (singular) fit: see help("isSingular")'
  expect_false(mixed_convergence_problem(list(opt=0L)))
  expect_false(mixed_convergence_problem(list(opt=0L,lme4=list(messages=singular))))
  expect_true(mixed_convergence_problem(list(opt=0L,lme4=list(messages=c(singular,'gradient failure')))))
  expect_true(mixed_convergence_problem(list(opt=1L)))
  expect_true(mixed_convergence_problem(list(opt=0L,lme4=list(code=-3L))))
})

test_that('failed country estimates are reported and omitted', {
  d <- data.frame(COUNTRY=c('Japan','Argentina'),wave=1,variable='x',mean=c(4,NA),conf.low=c(3,NA),conf.high=c(5,NA),status=c('ok','failed'))
  expect_warning(p <- plot_cross_national(d,'COUNTRY','x',estimates=TRUE),'Argentina')
  expect_equal(nrow(p$data),1L)
  expect_warning(p <- plot_longitudinal(d,'ID','wave','x',country_var='COUNTRY',estimates=TRUE),'Argentina')
  expect_equal(nrow(p$data),1L)
})

test_that('wave two preparation preserves valid ages and recodes missing responses', {
  d <- data.frame(ID=c('001','002'),COUNTRY=c(1,9),AGE_Y1=c(99,25),AGE_Y2=c(99,26),LIFE_SAT_Y1=c(99,8),LIFE_SAT_Y2=c(7,98),WAVE_Y1=1,WAVE_Y2=c(2,NA))
  r <- gfs_prepare(d,waves=1:2,release=2)
  expect_equal(nrow(r),4L)
  expect_equal(r$AGE[r$ID=='001'],c(99,99))
  expect_equal(r$LIFE_SAT[r$ID=='001'],c(NA,7))
  expect_true(is.na(r$LIFE_SAT[r$ID=='002' & r$wave==2]))
  expect_true(is.na(r$WAVE[r$ID=='002' & r$wave==2]))
  expect_error(gfs_prepare(d,waves=1:3,release=2),'exceed')
})

test_that('empty missingness selections return an empty table', {
  expect_equal(nrow(check_missingness(data.frame(x=1),character())),0L)
})

test_that('original missingness and callback interfaces remain available', {
  d <- data.frame(country=c('A','A','B'),x=c(1,NA,3))
  expect_equal(check_missingness(d,'x'),data.frame(variable='x',n_missing=1L,pct_missing=33.33))
  r <- loop_countries(d,'country',function(data) data.frame(m=mean(data$x,na.rm=TRUE)))
  expect_equal(r$m,c(1,3))
})

test_that('original plots and estimate plots build', {
  d <- data.frame(ID=rep(1:2,each=2),wave=rep(1:2,2),COUNTRY=rep(c('Japan','Argentina'),each=2),x=1:4)
  expect_s3_class(ggplot2::ggplot_build(plot_cross_national(d,'COUNTRY','x')),'ggplot_built')
  expect_s3_class(ggplot2::ggplot_build(plot_longitudinal(d,'ID','wave','x')),'ggplot_built')
  e <- data.frame(COUNTRY=c('Japan','Argentina'),wave=1,variable='x',mean=c(4,5),conf.low=c(3,4),conf.high=c(5,6),status='ok')
  expect_s3_class(ggplot2::ggplot_build(plot_cross_national(e,'COUNTRY','x',estimates=TRUE)),'ggplot_built')
  expect_s3_class(ggplot2::ggplot_build(plot_longitudinal(e,'ID','wave','x',country_var='COUNTRY',estimates=TRUE)),'ggplot_built')
})
