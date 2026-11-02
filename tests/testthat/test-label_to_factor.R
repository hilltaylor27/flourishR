test_that("label_to_factor converts labelled variables to factors", {

  test_data <- data.frame(
    id = 1:5,
    gender = haven::labelled(
      c(1, 2, 1, 3, NA),
      labels = c(
        "Woman" = 1,
        "Man" = 2,
        "Other" = 3
      )
    ),
    employment = haven::labelled(
      c(1, 1, 2, 3, NA),
      labels = c(
        "Full-time" = 1,
        "Part-time" = 2,
        "Student" = 3
      )
    )
  )

  result <- label_to_factor(
    data = test_data,
    variables = c("gender", "employment")
  )

  expect_true(is.factor(result$gender))
  expect_true(is.factor(result$employment))
})


test_that("label_to_factor preserves factor labels", {

  test_data <- data.frame(
    gender = haven::labelled(
      c(1, 2, 1, 3, NA),
      labels = c(
        "Woman" = 1,
        "Man" = 2,
        "Other" = 3
      )
    )
  )

  result <- label_to_factor(
    data = test_data,
    variables = "gender"
  )

  expect_equal(
    levels(result$gender),
    c("Woman", "Man", "Other")
  )
})


test_that("label_to_factor preserves missing values", {

  test_data <- data.frame(
    gender = haven::labelled(
      c(1, 2, NA),
      labels = c(
        "Woman" = 1,
        "Man" = 2
      )
    )
  )

  result <- label_to_factor(
    data = test_data,
    variables = "gender"
  )

  expect_true(is.na(result$gender[3]))
})


test_that("label_to_factor leaves unselected variables unchanged", {

  test_data <- data.frame(
    id = 1:3,
    gender = haven::labelled(
      c(1, 2, 1),
      labels = c(
        "Woman" = 1,
        "Man" = 2
      )
    ),
    score = c(5, 7, 9)
  )

  result <- label_to_factor(
    data = test_data,
    variables = "gender"
  )

  expect_equal(result$id, test_data$id)
  expect_equal(result$score, test_data$score)
})


test_that("label_to_factor returns a data frame", {

  test_data <- data.frame(
    gender = haven::labelled(
      c(1, 2),
      labels = c(
        "Woman" = 1,
        "Man" = 2
      )
    )
  )

  result <- label_to_factor(
    data = test_data,
    variables = "gender"
  )

  expect_s3_class(
    result,
    "data.frame"
  )
})


test_that("label_to_factor errors when data is not a data frame", {

  expect_error(
    label_to_factor(
      data = c(1, 2, 3),
      variables = "gender"
    ),
    "data must be a data frame"
  )
})


test_that("label_to_factor errors when variables is not a character vector", {

  test_data <- data.frame(
    id = 1:3
  )

  expect_error(
    label_to_factor(
      data = test_data,
      variables = 1
    ),
    "variables must be a character vector"
  )
})


test_that("label_to_factor errors when a requested variable is missing", {

  test_data <- data.frame(
    id = 1:3
  )

  expect_error(
    label_to_factor(
      data = test_data,
      variables = "gender"
    ),
    "Missing variables: gender"
  )
})


test_that("label_to_factor reports multiple missing variables", {

  test_data <- data.frame(
    id = 1:3
  )

  expect_error(
    label_to_factor(
      data = test_data,
      variables = c(
        "gender",
        "employment"
      )
    ),
    "Missing variables: gender, employment"
  )
})
