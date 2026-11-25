design_test_data <- function() {

	data.frame(
		ID = 1:8,
		COUNTRY = rep(c(1, 9), each = 4),
		STRATA = rep(c(1, 1, 2, 2), 2),
		PSU = rep(1:4, 2),
		LIFE_SAT = rep(c(6, 7, 8, 9), 2),
		ANNUAL_WEIGHT_C1 = rep(1, 8),
		ANNUAL_WEIGHT_C3 = rep(1, 8),
		ANNUAL_WEIGHT_L3 = rep(1, 8)
	)

}

test_that("gfs_design constructs country-specific designs", {

	skip_if_not_installed("survey")

	result <- gfs_design(
		design_test_data(),
		country = "Argentina"
	)

	expect_s3_class(result, "survey.design")
	expect_equal(nrow(result$variables), 4)

	expect_equal(
		as.numeric(
			stats::coef(
				survey::svymean(~LIFE_SAT, result)
			)
		),
		7.5
	)

})

test_that("gfs_design accepts numerical country identifiers", {

	skip_if_not_installed("survey")

	result <- gfs_design(
		design_test_data(),
		country = 9
	)

	expect_equal(nrow(result$variables), 4)
	expect_true(all(result$variables$COUNTRY == 9))

})

test_that("gfs_design excludes missing survey weights", {

	skip_if_not_installed("survey")

	data <- design_test_data()

	data$ANNUAL_WEIGHT_C1[1] <- NA

	result <- gfs_design(data, country = 1)

	expect_equal(nrow(result$variables), 3)

})

test_that("gfs_design selects a single longitudinal record", {

	skip_if_not_installed("survey")

	data <- dplyr::bind_rows(
		transform(design_test_data(), wave = 1),
		transform(design_test_data(), wave = 2),
		transform(design_test_data(), wave = 3)
	)

	result <- gfs_design(
		data,
		country = 1,
		waves = 1:3
	)

	expect_equal(nrow(result$variables), 4)
	expect_true(all(result$variables$wave == 3))

})

test_that("gfs_design validates input", {

	skip_if_not_installed("survey")

	data <- design_test_data()

	expect_error(gfs_design(data, country = 999))

	expect_error(gfs_design(data, country = c(1, 9)))

	expect_error(gfs_design(data, country = 1, waves = 2))

	expect_error(
		gfs_design(rbind(data, data[1, ]), country = 1)
	)

})
