
test_that("TIPI scoring respects response direction and item pairing", {
  d <- as.data.frame(as.list(setNames(c(1,7,2,6,3,7,1,6,2,5), paste0("TRAITS",1:10))))
  traits <- c("extraversion","agreeableness","conscientiousness","emotional_stability","openness")
  r <- score_tipi(d)
  expect_equal(unlist(r[traits], use.names=FALSE), c(7,7,6,6,5))
  expect_identical(r[names(d)], d)
  expect_equal(unlist(score_tipi(d,coding="standard")[traits],use.names=FALSE), c(1,1,2,2,3))
  d2 <- d; names(d2) <- paste0(names(d2),"_Y1")
  expect_equal(score_tipi(d2)[traits],r[traits])
  names(d2) <- letters[1:10]
  expect_equal(score_tipi(d2,items=letters[1:10])[traits],r[traits])
  expect_error(score_tipi(cbind(d,d2),items=rep("TRAITS1",10)),"distinct")
  expect_error(score_tipi(d[-1]),"distinct")
  expect_error(score_tipi(r),"already exist")
  expect_error(score_tipi(cbind(d,setNames(d,paste0(names(d),"_Y1")))),"Both item sets")
  for (code in c(-98,98,99,NA_real_)) {
    m <- d; m$TRAITS1 <- code
    expect_true(is.na(score_tipi(m)$extraversion))
    expect_equal(score_tipi(m)$agreeableness,7)
  }
  m <- d; m$TRAITS1 <- 8
  expect_error(score_tipi(m),"Invalid response")
  m$TRAITS1 <- "1"
  expect_error(score_tipi(m),"Non-numeric")
  expect_equal(nrow(score_tipi(d[FALSE,])),0L)
})
