test_that("plot_cross_national returns a ggplot object", {

  test_data <- data.frame(
    country = c(
      "England", "England",
      "Scotland", "Scotland",
      "Wales", "Wales"
    ),
    happiness = c(7, 8, 6, 7, 5, 6)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_s3_class(
    result,
    "ggplot"
  )

})


test_that("plot_cross_national calculates country means correctly", {

  test_data <- data.frame(
    country = c(
      "England", "England",
      "Scotland", "Scotland",
      "Wales", "Wales"
    ),
    happiness = c(7, 8, 6, 7, 5, 6)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_equal(
    result$data$mean_outcome,
    c(7.5, 6.5, 5.5)
  )

  expect_equal(
    result$data$n,
    c(2, 2, 2)
  )

})


test_that("plot_cross_national orders countries by descending mean", {

  test_data <- data.frame(
    country = c(
      "Wales", "Wales",
      "England", "England",
      "Scotland", "Scotland"
    ),
    happiness = c(5, 6, 8, 9, 6, 7)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_equal(
    as.character(result$data$country),
    c("England", "Scotland", "Wales")
  )

  expect_equal(
    levels(result$data$country_ordered),
    c("England", "Scotland", "Wales")
  )

})


test_that("plot_cross_national handles missing outcome values", {

  test_data <- data.frame(
    country = c(
      "England", "England", "England",
      "Scotland", "Scotland", "Scotland"
    ),
    happiness = c(
      7, 8, NA,
      5, NA, 7
    )
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_equal(
    result$data$n,
    c(2, 2)
  )

  expect_equal(
    result$data$mean_outcome,
    c(7.5, 6)
  )

})


test_that("plot_cross_national excludes countries with no valid outcome values", {

  test_data <- data.frame(
    country = c(
      "England", "England",
      "Scotland", "Scotland",
      "Wales", "Wales"
    ),
    happiness = c(
      7, 8,
      NA, NA,
      5, 6
    )
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_equal(
    as.character(result$data$country),
    c("England", "Wales")
  )

  expect_false(
    "Scotland" %in% result$data$country
  )

})


test_that("plot_cross_national creates default labels", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    happiness = c(7, 6)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "happiness"
  )

  expect_equal(
    result$labels$title,
    "Mean happiness by country"
  )

  expect_equal(
    result$labels$y,
    "Mean happiness"
  )

  expect_equal(
    result$labels$x,
    NULL
  )

  expect_equal(
    result$labels$caption,
    "Countries ordered by mean outcome"
  )

})


test_that("plot_cross_national uses custom labels", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    life_satisfaction = c(7, 6)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "country",
    outcome_var = "life_satisfaction",
    plot_title = "Mean life satisfaction by UK country",
    y_label = "Mean life satisfaction"
  )

  expect_equal(
    result$labels$title,
    "Mean life satisfaction by UK country"
  )

  expect_equal(
    result$labels$y,
    "Mean life satisfaction"
  )

})


test_that("plot_cross_national preserves the requested country variable", {

  test_data <- data.frame(
    nation = c(
      "England", "England",
      "Scotland", "Scotland"
    ),
    happiness = c(7, 8, 6, 7)
  )

  result <- plot_cross_national(
    data = test_data,
    country_var = "nation",
    outcome_var = "happiness"
  )

  expect_true(
    "nation" %in% names(result$data)
  )

  expect_equal(
    as.character(result$data$nation),
    c("England", "Scotland")
  )

})


test_that("plot_cross_national rejects non-data-frame input", {

  expect_error(
    plot_cross_national(
      data = 1:5,
      country_var = "country",
      outcome_var = "happiness"
    ),
    "data must be a data frame.",
    fixed = TRUE
  )

})


test_that("plot_cross_national reports a missing country variable", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    happiness = c(7, 6)
  )

  expect_error(
    plot_cross_national(
      data = test_data,
      country_var = "missing_country",
      outcome_var = "happiness"
    ),
    "country_var must be a variable in data.",
    fixed = TRUE
  )

})


test_that("plot_cross_national reports a missing outcome variable", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    happiness = c(7, 6)
  )

  expect_error(
    plot_cross_national(
      data = test_data,
      country_var = "country",
      outcome_var = "missing_outcome"
    ),
    "outcome_var must be a variable in data.",
    fixed = TRUE
  )

})
