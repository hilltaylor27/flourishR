test_that("score_scale calculates expected scale scores", {

  test_data <- data.frame(
    id = 1:7,
    item.1 = c(1, 2, 3, 4, 5, NA, 1),
    item.2 = c(2, 3, 4, 5, NA, NA, 8),
    item.3 = c(3, 4, 5, NA, NA, NA, 3),
    item.4 = c(4, 5, 1, NA, 2, NA, 4)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2", "item.3", "item.4"),
    prefix = "scale",
    min_items = 3,
    valid_range = c(1, 5)
  )

  expect_equal(
    result$scale.n.valid,
    c(4, 4, 4, 2, 2, 0, 3)
  )

  expect_equal(
    result$scale.sum,
    c(10, 14, 13, NA, NA, NA, 8)
  )

  expect_equal(
    result$scale.mean,
    c(2.5, 3.5, 3.25, NA, NA, NA, 8 / 3)
  )

  expect_equal(
    result$scale.prorated,
    c(10, 14, 13, NA, NA, NA, 32 / 3)
  )

})


test_that("score_scale preserves the original data", {

  test_data <- data.frame(
    id = 1:3,
    item.1 = c(1, 2, 3),
    item.2 = c(2, 3, 4)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "scale"
  )

  expect_equal(result$id, test_data$id)
  expect_equal(result$item.1, test_data$item.1)
  expect_equal(result$item.2, test_data$item.2)

})


test_that("score_scale creates correctly named output variables", {

  test_data <- data.frame(
    item.1 = c(1, 2),
    item.2 = c(2, 3)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "wellbeing"
  )

  expect_true(
    all(
      c(
        "wellbeing.n.valid",
        "wellbeing.sum",
        "wellbeing.mean",
        "wellbeing.prorated"
      ) %in% names(result)
    )
  )

})


test_that("score_scale uses complete-case scoring by default", {

  test_data <- data.frame(
    item.1 = c(1, 2, 3),
    item.2 = c(2, NA, 4),
    item.3 = c(3, 4, 5)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2", "item.3"),
    prefix = "scale"
  )

  expect_false(is.na(result$scale.mean[1]))
  expect_true(is.na(result$scale.mean[2]))
  expect_false(is.na(result$scale.mean[3]))

})


test_that("score_scale treats values outside valid_range as missing for scoring", {

  test_data <- data.frame(
    item.1 = c(1, 2),
    item.2 = c(99, 3),
    item.3 = c(3, 4)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2", "item.3"),
    prefix = "scale",
    min_items = 2,
    valid_range = c(1, 5)
  )

  expect_equal(result$scale.n.valid, c(2, 3))
  expect_equal(result$scale.sum, c(4, 9))
  expect_equal(result$scale.mean, c(2, 3))
  expect_equal(result$scale.prorated, c(6, 9))

})


test_that("score_scale does not alter original out-of-range values", {

  test_data <- data.frame(
    item.1 = c(1, 2),
    item.2 = c(99, 3)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "scale",
    min_items = 1,
    valid_range = c(1, 5)
  )

  expect_equal(result$item.2, c(99, 3))

})


test_that("score_scale returns missing scores when minimum-item rule is not met", {

  test_data <- data.frame(
    item.1 = c(1, 1),
    item.2 = c(2, NA),
    item.3 = c(3, NA)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2", "item.3"),
    prefix = "scale",
    min_items = 2
  )

  expect_equal(result$scale.n.valid, c(3, 1))
  expect_false(is.na(result$scale.sum[1]))
  expect_true(is.na(result$scale.sum[2]))
  expect_true(is.na(result$scale.mean[2]))
  expect_true(is.na(result$scale.prorated[2]))

})


test_that("score_scale handles rows with all items missing", {

  test_data <- data.frame(
    item.1 = c(NA, 1),
    item.2 = c(NA, 2)
  )

  result <- score_scale(
    data = test_data,
    items = c("item.1", "item.2"),
    prefix = "scale",
    min_items = 1
  )

  expect_equal(result$scale.n.valid, c(0, 2))
  expect_true(is.na(result$scale.sum[1]))
  expect_true(is.na(result$scale.mean[1]))
  expect_true(is.na(result$scale.prorated[1]))

})


test_that("score_scale rejects non-data-frame input", {

  expect_error(
    score_scale(
      data = "not a dataframe",
      items = c("item.1"),
      prefix = "scale"
    ),
    "data must be a data frame.",
    fixed = TRUE
  )

})


test_that("score_scale requires a non-empty character items vector", {

  test_data <- data.frame(item.1 = 1:3)

  expect_error(
    score_scale(
      data = test_data,
      items = numeric(0),
      prefix = "scale"
    ),
    "items must be a character vector containing at least one variable name.",
    fixed = TRUE
  )

})


test_that("score_scale reports missing items", {

  test_data <- data.frame(item.1 = 1:3)

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "missing.item"),
      prefix = "scale"
    ),
    "The following items are not present in data: missing.item.",
    fixed = TRUE
  )

})


test_that("score_scale rejects duplicate item names", {

  test_data <- data.frame(item.1 = 1:3)

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "item.1"),
      prefix = "scale"
    ),
    "items must not contain duplicate variable names.",
    fixed = TRUE
  )

})


test_that("score_scale requires a valid prefix", {

  test_data <- data.frame(item.1 = 1:3)

  expect_error(
    score_scale(
      data = test_data,
      items = "item.1",
      prefix = ""
    ),
    "prefix must be a single non-empty character value.",
    fixed = TRUE
  )

})


test_that("score_scale validates min_items", {

  test_data <- data.frame(
    item.1 = 1:3,
    item.2 = 2:4
  )

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "item.2"),
      prefix = "scale",
      min_items = 3
    ),
    "min_items must be a whole number between 1 and the number of items.",
    fixed = TRUE
  )

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "item.2"),
      prefix = "scale",
      min_items = 1.5
    ),
    "min_items must be a whole number between 1 and the number of items.",
    fixed = TRUE
  )

})


test_that("score_scale validates valid_range", {

  test_data <- data.frame(item.1 = 1:3)

  expect_error(
    score_scale(
      data = test_data,
      items = "item.1",
      prefix = "scale",
      valid_range = c(5, 1)
    ),
    "valid_range must be NULL or a numeric vector containing the minimum and maximum valid values.",
    fixed = TRUE
  )

})


test_that("score_scale rejects non-numeric scale items", {

  test_data <- data.frame(
    item.1 = c(1, 2, 3),
    item.2 = c("a", "b", "c")
  )

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "item.2"),
      prefix = "scale"
    ),
    "The following items are not numeric: item.2.",
    fixed = TRUE
  )

})


test_that("score_scale prevents overwriting existing output variables", {

  test_data <- data.frame(
    item.1 = 1:3,
    item.2 = 2:4,
    scale.mean = c(1, 2, 3)
  )

  expect_error(
    score_scale(
      data = test_data,
      items = c("item.1", "item.2"),
      prefix = "scale"
    ),
    "The following output variables already exist in data: scale.mean.",
    fixed = TRUE
  )

})
