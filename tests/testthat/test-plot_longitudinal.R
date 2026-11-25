test_that("plot_longitudinal returns a ggplot object", {

  test_data <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, times = 3),
    happiness = c(
      6, 7, 8,
      5, 6, 7,
      7, 7, 8
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  expect_s3_class(
    result,
    "ggplot"
  )

})


test_that("plot_longitudinal calculates wave means correctly", {

  test_data <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, times = 3),
    happiness = c(
      6, 7, 8,
      5, 6, 7,
      7, 7, 8
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  point_data <- ggplot2::layer_data(result, 2)

  expect_equal(
    point_data$x,
    c(1, 2, 3)
  )

  expect_equal(
    point_data$y,
    c(6, 20 / 3, 23 / 3)
  )

})


test_that("plot_longitudinal calculates standard errors correctly", {

  test_data <- data.frame(
    id = rep(1:3, each = 2),
    wave = rep(1:2, times = 3),
    happiness = c(
      5, 6,
      7, 8,
      9, 10
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  errorbar_data <- ggplot2::layer_data(result, 3)

  expected_se <- stats::sd(c(5, 7, 9)) / sqrt(3)

  expect_equal(
    errorbar_data$ymin[1],
    7 - expected_se
  )

  expect_equal(
    errorbar_data$ymax[1],
    7 + expected_se
  )

})


test_that("plot_longitudinal handles missing outcome values", {

  test_data <- data.frame(
    id = rep(1:3, each = 2),
    wave = rep(1:2, times = 3),
    happiness = c(
      5, NA,
      7, 8,
      9, 10
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  point_data <- ggplot2::layer_data(result, 2)

  expect_equal(
    point_data$y,
    c(7, 9)
  )

})


test_that("plot_longitudinal excludes waves with no valid outcome values", {

  test_data <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, times = 3),
    happiness = c(
      5, NA, 7,
      6, NA, 8,
      7, NA, 9
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  point_data <- ggplot2::layer_data(result, 2)

  expect_equal(
    point_data$x,
    c(1, 3)
  )

  expect_equal(
    point_data$y,
    c(6, 8)
  )

})


test_that("plot_longitudinal does not show individual trajectories by default", {

  test_data <- data.frame(
    id = rep(1:2, each = 3),
    wave = rep(1:3, times = 2),
    happiness = c(
      5, 6, 7,
      7, 8, 9
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  expect_equal(
    length(result$layers),
    3
  )

})


test_that("plot_longitudinal can show individual trajectories", {

  test_data <- data.frame(
    id = rep(1:2, each = 3),
    wave = rep(1:3, times = 2),
    happiness = c(
      5, 6, 7,
      7, 8, 9
    )
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness",
    show_individuals = TRUE
  )

  expect_equal(
    length(result$layers),
    4
  )

  individual_data <- ggplot2::layer_data(result, 1)

  expect_equal(
    nrow(individual_data),
    6
  )

  expect_equal(
    length(unique(individual_data$group)),
    2
  )

})


test_that("plot_longitudinal creates default labels", {

  test_data <- data.frame(
    id = c(1, 1, 2, 2),
    wave = c(1, 2, 1, 2),
    happiness = c(6, 7, 7, 8)
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "happiness"
  )

  expect_equal(
    result$labels$title,
    "Mean happiness over waves"
  )

  expect_equal(
    result$labels$x,
    "Wave"
  )

  expect_equal(
    result$labels$y,
    "Mean happiness"
  )

  expect_equal(
    result$labels$caption,
    "Points show wave means; error bars show standard errors"
  )

})


test_that("plot_longitudinal uses custom labels", {

  test_data <- data.frame(
    id = c(1, 1, 2, 2),
    wave = c(1, 2, 1, 2),
    life_satisfaction = c(6, 7, 7, 8)
  )

  result <- plot_longitudinal(
    data = test_data,
    id_var = "id",
    wave_var = "wave",
    outcome_var = "life_satisfaction",
    plot_title = "Life satisfaction over time",
    y_label = "Life satisfaction"
  )

  expect_equal(
    result$labels$title,
    "Life satisfaction over time"
  )

  expect_equal(
    result$labels$y,
    "Life satisfaction"
  )

})


test_that("plot_longitudinal rejects non-data-frame input", {

  expect_error(
    plot_longitudinal(
      data = 1:5,
      id_var = "id",
      wave_var = "wave",
      outcome_var = "happiness"
    ),
    "data must be a data frame.",
    fixed = TRUE
  )

})


test_that("plot_longitudinal reports a missing id variable", {

  test_data <- data.frame(
    id = c(1, 1),
    wave = c(1, 2),
    happiness = c(6, 7)
  )

  expect_error(
    plot_longitudinal(
      data = test_data,
      id_var = "missing_id",
      wave_var = "wave",
      outcome_var = "happiness"
    ),
    "id_var must be a variable in data.",
    fixed = TRUE
  )

})


test_that("plot_longitudinal reports a missing wave variable", {

  test_data <- data.frame(
    id = c(1, 1),
    wave = c(1, 2),
    happiness = c(6, 7)
  )

  expect_error(
    plot_longitudinal(
      data = test_data,
      id_var = "id",
      wave_var = "missing_wave",
      outcome_var = "happiness"
    ),
    "wave_var must be a variable in data.",
    fixed = TRUE
  )

})


test_that("plot_longitudinal reports a missing outcome variable", {

  test_data <- data.frame(
    id = c(1, 1),
    wave = c(1, 2),
    happiness = c(6, 7)
  )

  expect_error(
    plot_longitudinal(
      data = test_data,
      id_var = "id",
      wave_var = "wave",
      outcome_var = "missing_outcome"
    ),
    "outcome_var must be a variable in data.",
    fixed = TRUE
  )

})


test_that("plot_longitudinal requires logical show_individuals", {

  test_data <- data.frame(
    id = c(1, 1),
    wave = c(1, 2),
    happiness = c(6, 7)
  )

  expect_error(
    plot_longitudinal(
      data = test_data,
      id_var = "id",
      wave_var = "wave",
      outcome_var = "happiness",
      show_individuals = "yes"
    ),
    "show_individuals must be TRUE or FALSE.",
    fixed = TRUE
  )

})
