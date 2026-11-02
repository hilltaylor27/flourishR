test_that("score_swemwbs calculates expected scores", {

  test_data <- data.frame(
    sw1 = c(1, 1, 1, NA, 5, 5),
    sw2 = c(2, 2, 2, NA, 5, 5),
    sw3 = c(3, 3, 3, NA, 5, 5),
    sw4 = c(4, 4, 4, NA, 5, 5),
    sw5 = c(5, 5, NA, NA, 5, 5),
    sw6 = c(1, 1, NA, NA, 5, 5),
    sw7 = c(2, 2, NA, NA, 5, 5)
  )

  result <- score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
  )

  expect_equal(
    result$swemwbs.n.valid,
    c(7, 7, 4, 0, 7, 7)
  )

  expect_equal(
    result$swemwbs.sum,
    c(18, 18, NA, NA, 35, 35)
  )

  expect_equal(
    result$swemwbs.mean,
    c(18 / 7, 18 / 7, NA, NA, 5, 5)
  )

  expect_equal(
    result$swemwbs.prorated,
    c(18, 18, NA, NA, 35, 35)
  )

})


test_that("score_swemwbs uses five valid items by default", {

  test_data <- data.frame(
    sw1 = c(1, 1),
    sw2 = c(2, 2),
    sw3 = c(3, 3),
    sw4 = c(4, 4),
    sw5 = c(5, NA_real_),
    sw6 = c(NA_real_, NA_real_),
    sw7 = c(NA_real_, NA_real_)
  )

  result <- score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
  )

  expect_equal(
    result$swemwbs.n.valid,
    c(5, 4)
  )

  expect_false(
    is.na(result$swemwbs.mean[1])
  )

  expect_true(
    is.na(result$swemwbs.mean[2])
  )

})


test_that("score_swemwbs allows a different minimum-item rule", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = NA_real_,
    sw6 = NA_real_,
    sw7 = NA_real_
  )

  result <- score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7"),
    min_items = 4
  )

  expect_equal(
    result$swemwbs.n.valid,
    4
  )

  expect_equal(
    result$swemwbs.mean,
    2.5
  )

  expect_equal(
    result$swemwbs.prorated,
    17.5
  )

})


test_that("score_swemwbs applies the SWEMWBS valid range", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = 5,
    sw6 = 99,
    sw7 = 2
  )

  result <- score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
  )

  expect_equal(
    result$swemwbs.n.valid,
    6
  )

  expect_equal(
    result$swemwbs.sum,
    17
  )

  expect_equal(
    result$swemwbs.mean,
    17 / 6
  )

  expect_equal(
    result$swemwbs.prorated,
    (17 / 6) * 7
  )

})


test_that("score_swemwbs preserves original item values", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = 5,
    sw6 = 99,
    sw7 = 2
  )

  result <- score_swemwbs(
    data = test_data,
    items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
  )

  expect_equal(
    result$sw6,
    99
  )

})


test_that("score_swemwbs requires exactly seven items", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3
  )

  expect_error(
    score_swemwbs(
      data = test_data,
      items = c("sw1", "sw2", "sw3")
    ),
    "SWEMWBS requires exactly 7 items.",
    fixed = TRUE
  )

})


test_that("score_swemwbs reports missing variables", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = 5,
    sw6 = 1
  )

  expect_error(
    score_swemwbs(
      data = test_data,
      items = c(
        "sw1",
        "sw2",
        "sw3",
        "sw4",
        "sw5",
        "sw6",
        "missing_variable"
      )
    ),
    "The following items are not present in data: missing_variable.",
    fixed = TRUE
  )

})


test_that("score_swemwbs inherits score_scale min_items validation", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = 5,
    sw6 = 1,
    sw7 = 2
  )

  expect_error(
    score_swemwbs(
      data = test_data,
      items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7"),
      min_items = 8
    ),
    "min_items must be a whole number between 1 and the number of items.",
    fixed = TRUE
  )

})


test_that("score_swemwbs rejects non-numeric items through score_scale", {

  test_data <- data.frame(
    sw1 = 1,
    sw2 = 2,
    sw3 = 3,
    sw4 = 4,
    sw5 = 5,
    sw6 = 1,
    sw7 = "two"
  )

  expect_error(
    score_swemwbs(
      data = test_data,
      items = c("sw1", "sw2", "sw3", "sw4", "sw5", "sw6", "sw7")
    ),
    "The following items are not numeric: sw7.",
    fixed = TRUE
  )

})
