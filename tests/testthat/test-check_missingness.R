test_that("check_missingness summarises all variables by default", {

  test_data <- data.frame(
    id = 1:6,
    life_satisfaction = c(7, 8, NA, 6, NA, 5),
    happiness = c(8, NA, 7, NA, 6, 5),
    age = c(25, 30, 35, 40, 45, 50)
  )

  result <- check_missingness(test_data)

  expect_s3_class(result, "data.frame")

  expect_equal(
    names(result),
    c("variable", "n_missing", "pct_missing")
  )

  expect_equal(
    result$variable,
    c("id", "life_satisfaction", "happiness", "age")
  )

  expect_equal(
    result$n_missing,
    c(0, 2, 2, 0)
  )

  expect_equal(
    result$pct_missing,
    c(0, 33.33, 33.33, 0)
  )

})


test_that("check_missingness can check selected variables", {

  test_data <- data.frame(
    id = 1:6,
    life_satisfaction = c(7, 8, NA, 6, NA, 5),
    happiness = c(8, NA, 7, NA, 6, 5),
    age = c(25, 30, 35, 40, 45, 50)
  )

  result <- check_missingness(
    data = test_data,
    variables = c("life_satisfaction", "happiness")
  )

  expect_equal(
    result$variable,
    c("life_satisfaction", "happiness")
  )

  expect_equal(
    result$n_missing,
    c(2, 2)
  )

  expect_equal(
    result$pct_missing,
    c(33.33, 33.33)
  )

})


test_that("check_missingness handles variables with no missing values", {

  test_data <- data.frame(
    x = 1:5,
    y = 6:10
  )

  result <- check_missingness(test_data)

  expect_equal(
    result$n_missing,
    c(0, 0)
  )

  expect_equal(
    result$pct_missing,
    c(0, 0)
  )

})


test_that("check_missingness handles variables with all values missing", {

  test_data <- data.frame(
    x = c(NA, NA, NA, NA)
  )

  result <- check_missingness(test_data)

  expect_equal(
    result$n_missing,
    4
  )

  expect_equal(
    result$pct_missing,
    100
  )

})


test_that("check_missingness rounds percentages to two decimal places", {

  test_data <- data.frame(
    x = c(1, 2, NA)
  )

  result <- check_missingness(test_data)

  expect_equal(
    result$pct_missing,
    33.33
  )

})


test_that("check_missingness works across variable types", {

  test_data <- data.frame(
    numeric = c(1, NA, 3),
    character = c("a", NA, "c"),
    logical = c(TRUE, FALSE, NA)
  )

  result <- check_missingness(test_data)

  expect_equal(
    result$n_missing,
    c(1, 1, 1)
  )

})


test_that("check_missingness rejects non-data-frame input", {

  expect_error(
    check_missingness(c(1, NA, 3)),
    "data must be a data frame.",
    fixed = TRUE
  )

})


test_that("check_missingness reports requested variables that are absent", {

  test_data <- data.frame(
    x = 1:5,
    y = 6:10
  )

  expect_error(
    check_missingness(
      test_data,
      variables = c("x", "missing_variable")
    ),
    "Missing variables: missing_variable",
    fixed = TRUE
  )

})


test_that("check_missingness reports multiple absent variables", {

  test_data <- data.frame(
    x = 1:5
  )

  expect_error(
    check_missingness(
      test_data,
      variables = c("missing_one", "x", "missing_two")
    ),
    "Missing variables: missing_one, missing_two",
    fixed = TRUE
  )

})
