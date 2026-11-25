test_that("loop_countries applies an analysis separately by country", {

  test_data <- data.frame(
    id = 1:12,
    country = c(
      "England", "England", "England",
      "Scotland", "Scotland", "Scotland",
      "Wales", "Wales", "Wales",
      "Northern Ireland", "Northern Ireland", "Northern Ireland"
    ),
    happiness = c(
      7, 8, NA,
      6, NA, 7,
      5, 6, NA,
      8, 7, NA
    )
  )

  mean_happiness <- function(data) {
    data.frame(
      mean_happiness = mean(data$happiness, na.rm = TRUE)
    )
  }

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = mean_happiness
  )

  expect_s3_class(
    result,
    "data.frame"
  )

  expect_equal(
    nrow(result),
    4
  )

  expect_equal(
    result$country,
    c(
      "England",
      "Scotland",
      "Wales",
      "Northern Ireland"
    )
  )

  expect_equal(
    result$mean_happiness,
    c(7.5, 6.5, 5.5, 7.5)
  )

})


test_that("loop_countries places the country variable first", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    value = c(1, 2)
  )

  analysis <- function(data) {
    data.frame(
      mean_value = mean(data$value)
    )
  }

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = analysis
  )

  expect_equal(
    names(result)[1],
    "country"
  )

})


test_that("loop_countries passes additional arguments to the analysis function", {

  test_data <- data.frame(
    country = c(
      "England", "England",
      "Scotland", "Scotland"
    ),
    x = c(1, 2, 3, 4),
    y = c(5, 6, 7, 8)
  )

  calculate_mean <- function(data, variable) {
    data.frame(
      variable = variable,
      mean = mean(data[[variable]])
    )
  }

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = calculate_mean,
    variable = "x"
  )

  expect_equal(
    result$variable,
    c("x", "x")
  )

  expect_equal(
    result$mean,
    c(1.5, 3.5)
  )

})


test_that("loop_countries works with check_missingness", {

  test_data <- data.frame(
    id = 1:12,
    country = c(
      "England", "England", "England",
      "Scotland", "Scotland", "Scotland",
      "Wales", "Wales", "Wales",
      "Northern Ireland", "Northern Ireland", "Northern Ireland"
    ),
    happiness = c(
      7, 8, NA,
      6, NA, 7,
      5, 6, NA,
      8, 7, NA
    ),
    life_satisfaction = c(
      8, NA, 7,
      6, 6, NA,
      5, NA, 6,
      8, 8, 7
    )
  )

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = check_missingness,
    variables = c("happiness", "life_satisfaction")
  )

  expect_equal(
    nrow(result),
    8
  )

  expect_equal(
    unique(result$country),
    c(
      "England",
      "Scotland",
      "Wales",
      "Northern Ireland"
    )
  )

  expect_equal(
    unique(result$variable),
    c("happiness", "life_satisfaction")
  )

  expect_equal(
    result$n_missing,
    c(1, 1, 1, 1, 1, 1, 1, 0)
  )

  expect_equal(
    result$pct_missing,
    c(
      33.33, 33.33,
      33.33, 33.33,
      33.33, 33.33,
      33.33, 0
    )
  )

})


test_that("loop_countries excludes missing country values", {

  test_data <- data.frame(
    country = c(
      "England",
      NA,
      "Scotland",
      NA
    ),
    value = c(1, 100, 3, 200)
  )

  calculate_mean <- function(data) {
    data.frame(
      mean_value = mean(data$value)
    )
  }

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = calculate_mean
  )

  expect_equal(
    result$country,
    c("England", "Scotland")
  )

  expect_equal(
    result$mean_value,
    c(1, 3)
  )

})


test_that("loop_countries preserves country order of appearance", {

  test_data <- data.frame(
    country = c(
      "Wales",
      "England",
      "Scotland",
      "Wales",
      "England"
    ),
    value = 1:5
  )

  count_rows <- function(data) {
    data.frame(
      n = nrow(data)
    )
  }

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = count_rows
  )

  expect_equal(
    result$country,
    c("Wales", "England", "Scotland")
  )

})


test_that("loop_countries supports analysis functions returning multiple rows", {

  test_data <- data.frame(
    country = c(
      "England", "England",
      "Scotland", "Scotland"
    ),
    x = c(1, NA, 3, 4),
    y = c(NA, 2, 5, 6)
  )

  result <- loop_countries(
    data = test_data,
    country_var = "country",
    analysis_function = check_missingness,
    variables = c("x", "y")
  )

  expect_equal(
    nrow(result),
    4
  )

  expect_equal(
    result$country,
    c(
      "England", "England",
      "Scotland", "Scotland"
    )
  )

  expect_equal(
    result$variable,
    c("x", "y", "x", "y")
  )

})


test_that("loop_countries rejects non-data-frame input", {

  expect_error(
    loop_countries(
      data = 1:5,
      country_var = "country",
      analysis_function = mean
    ),
    "data must be a data frame.",
    fixed = TRUE
  )

})


test_that("loop_countries reports a missing country variable", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    value = c(1, 2)
  )

  analysis <- function(data) {
    data.frame(n = nrow(data))
  }

  expect_error(
    loop_countries(
      data = test_data,
      country_var = "missing_country",
      analysis_function = analysis
    ),
    "country_var must be a variable in data.",
    fixed = TRUE
  )

})


test_that("loop_countries requires an analysis function", {

  test_data <- data.frame(
    country = c("England", "Scotland"),
    value = c(1, 2)
  )

  expect_error(
    loop_countries(
      data = test_data,
      country_var = "country",
      analysis_function = "not_a_function"
    ),
    "analysis_function must be a function.",
    fixed = TRUE
  )

})
