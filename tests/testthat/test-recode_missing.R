test_that("recode_missing recodes default missing codes to NA", {

  test_data <- data.frame(
    life_satisfaction = c(7, 8, -9, -8, -7, -2, -1, 6)
  )

  result <- recode_missing(
    data = test_data,
    variables = "life_satisfaction"
  )

  expect_equal(
    result$life_satisfaction,
    c(7, 8, NA, NA, NA, NA, NA, 6)
  )

})


test_that("recode_missing recodes multiple variables", {

  test_data <- data.frame(
    life_satisfaction = c(7, -9, 6),
    happiness = c(-8, 7, -1)
  )

  result <- recode_missing(
    data = test_data,
    variables = c("life_satisfaction", "happiness")
  )

  expect_equal(
    result$life_satisfaction,
    c(7, NA, 6)
  )

  expect_equal(
    result$happiness,
    c(NA, 7, NA)
  )

})


test_that("recode_missing supports custom missing codes", {

  test_data <- data.frame(
    score = c(1, 2, 999, 4, 999)
  )

  result <- recode_missing(
    data = test_data,
    variables = "score",
    missing_codes = 999
  )

  expect_equal(
    result$score,
    c(1, 2, NA, 4, NA)
  )

})


test_that("recode_missing supports multiple custom missing codes", {

  test_data <- data.frame(
    score = c(1, 99, 999, 4)
  )

  result <- recode_missing(
    data = test_data,
    variables = "score",
    missing_codes = c(99, 999)
  )

  expect_equal(
    result$score,
    c(1, NA, NA, 4)
  )

})


test_that("recode_missing preserves existing NA values", {

  test_data <- data.frame(
    score = c(1, NA, -9, 4)
  )

  result <- recode_missing(
    data = test_data,
    variables = "score"
  )

  expect_equal(
    result$score,
    c(1, NA, NA, 4)
  )

})


test_that("recode_missing leaves unselected variables unchanged", {

  test_data <- data.frame(
    score = c(1, -9, 3),
    age = c(25, -9, 35)
  )

  result <- recode_missing(
    data = test_data,
    variables = "score"
  )

  expect_equal(
    result$score,
    c(1, NA, 3)
  )

  expect_equal(
    result$age,
    test_data$age
  )

})


test_that("recode_missing returns a data frame", {

  test_data <- data.frame(
    score = c(1, -9, 3)
  )

  result <- recode_missing(
    data = test_data,
    variables = "score"
  )

  expect_s3_class(
    result,
    "data.frame"
  )

})


test_that("recode_missing errors when data is not a data frame", {

  expect_error(
    recode_missing(
      c(1, -9, 3),
      "score"
    ),
    "data must be a data frame"
  )

})


test_that("recode_missing errors when variables is not a character vector", {

  test_data <- data.frame(
    score = c(1, -9, 3)
  )

  expect_error(
    recode_missing(
      test_data,
      1
    ),
    "variables must be a character vector"
  )

})


test_that("recode_missing errors when a requested variable is missing", {

  test_data <- data.frame(
    score = c(1, -9, 3)
  )

  expect_error(
    recode_missing(
      test_data,
      "happiness"
    ),
    "Missing variables: happiness"
  )

})


test_that("recode_missing reports multiple missing variables", {

  test_data <- data.frame(
    score = c(1, -9, 3)
  )

  expect_error(
    recode_missing(
      test_data,
      c("happiness", "life_satisfaction")
    ),
    "Missing variables: happiness, life_satisfaction"
  )

})