test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

gfs_test_data <- function() {

	data.frame(
		ID = 1:3,
		COUNTRY = c(1, 9, 20),
		GENDER = c(1, 2, 1),
		BORN_COUNTRY_Y1 = c(1, 1, 2),
		AGE_Y1 = c(25, 99, 45),
		AGE_Y2 = c(26, 99, 46),
		AGE_Y3 = c(27, 99, 47),
		LIFE_SAT_Y1 = c(7, 8, 99),
		LIFE_SAT_Y2 = c(8, 98, 6),
		LIFE_SAT_Y3 = c(9, NA, 7),
		LONELY_Y1 = c(5, 6, 7),
		LONELY_Y2 = c(6, 7, 8),
		LONELY_Y3 = c(7, 8, 9),
		WAVE_Y1 = c(1, 1, 1),
		WAVE_Y2 = c(2, 2, 2),
		WAVE_Y3 = c(3, NA, 3),
		STRATA = c(1, 2, 3),
		PSU = c(10, 20, 30),
		ANNUAL_WEIGHT_C1 = c(0.8, 1.1, 1.2),
		ANNUAL_WEIGHT_C3 = c(0.9, NA, 1.3),
		ANNUAL_WEIGHT_L3 = c(0.9, NA, 1.3)
	)

}

test_that("gfs_prepare returns long-format data", {

	result <- gfs_prepare(gfs_test_data())

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 9)
	expect_equal(sort(unique(result$wave)), 1:3)

	expect_true("LIFE_SAT" %in% names(result))
	expect_false("LIFE_SAT_Y1" %in% names(result))

	expect_equal(
		result$LIFE_SAT[result$ID == 1],
		c(7, 8, 9)
	)

})

test_that("gfs_prepare returns wide-format data", {

	result <- gfs_prepare(
		gfs_test_data(),
		format = "wide"
	)

	expect_equal(nrow(result), 3)
	expect_true("LIFE_SAT_Y1" %in% names(result))
	expect_true("LIFE_SAT_Y3" %in% names(result))

})

test_that("gfs_prepare selects requested waves", {

	result <- gfs_prepare(
		gfs_test_data(),
		waves = c(1, 3)
	)

	expect_equal(nrow(result), 6)
	expect_setequal(result$wave, c(1, 3))

})

test_that("gfs_prepare selects requested variables", {

	result <- gfs_prepare(
		gfs_test_data(),
		variables = c("LIFE_SAT", "AGE")
	)

	expect_true("LIFE_SAT" %in% names(result))
	expect_true("AGE" %in% names(result))
	expect_false("LONELY" %in% names(result))

	expect_true("ID" %in% names(result))
	expect_true("COUNTRY" %in% names(result))

})

test_that("gfs_prepare applies variable-specific missing codes", {

	result <- gfs_prepare(gfs_test_data())

	expect_true(is.na(
		result$LIFE_SAT[result$ID == 3 & result$wave == 1]
	))

	expect_true(is.na(
		result$LIFE_SAT[result$ID == 2 & result$wave == 2]
	))

	expect_equal(
		result$AGE[result$ID == 2],
		c(99, 99, 99)
	)

})

test_that("gfs_prepare can retain original missing codes", {

	result <- gfs_prepare(
		gfs_test_data(),
		recode_missing = FALSE,
		format = "wide"
	)

	expect_equal(result$LIFE_SAT_Y1[3], 99)
	expect_equal(result$LIFE_SAT_Y2[2], 98)

})

test_that("gfs_prepare labels countries", {

	result <- gfs_prepare(
		gfs_test_data(),
		format = "wide"
	)

	expect_equal(
		result$COUNTRY,
		c("Argentina", "Japan", "United Kingdom")
	)

	original <- gfs_prepare(
		gfs_test_data(),
		label_countries = FALSE,
		format = "wide"
	)

	expect_equal(original$COUNTRY, c(1, 9, 20))

})

test_that("gfs_prepare preserves baseline characteristics", {

	result <- gfs_prepare(gfs_test_data())

	expect_equal(
		result$GENDER[result$ID == 1],
		rep(1, 3)
	)

	expect_equal(
		result$BORN_COUNTRY_Y1[result$ID == 3],
		rep(2, 3)
	)

})

test_that("gfs_prepare retains survey design and weights", {

	result <- gfs_prepare(gfs_test_data())

	expect_true(all(
		c("STRATA", "PSU", "ANNUAL_WEIGHT_L3") %in%
			names(result)
	))

	expect_equal(
		result$ANNUAL_WEIGHT_L3[result$ID == 1],
		rep(0.9, 3)
	)

})

test_that("gfs_prepare can exclude weights and design variables", {

	result <- gfs_prepare(
		gfs_test_data(),
		keep_weights = FALSE,
		keep_design = FALSE
	)

	expect_false(any(
		grepl("^ANNUAL_WEIGHT", names(result))
	))

	expect_false(any(
		c("STRATA", "PSU") %in% names(result)
	))

})

test_that("gfs_prepare retains incomplete longitudinal records", {

	result <- gfs_prepare(gfs_test_data())

	expect_equal(
		nrow(result[result$ID == 2, ]),
		3
	)

	expect_true(is.na(
		result$LIFE_SAT[result$ID == 2 & result$wave == 3]
	))

})

test_that("gfs_prepare imports CSV files", {

	path <- tempfile(fileext = ".csv")

	utils::write.csv(
		gfs_test_data(),
		path,
		row.names = FALSE
	)

	result <- gfs_prepare(path)

	expect_equal(nrow(result), 9)

	unlink(path)

})

test_that("gfs_prepare validates participant identifiers", {

	data <- gfs_test_data()

	data$ID[2] <- data$ID[1]

	expect_error(
		gfs_prepare(data),
		"Duplicate participant"
	)

	data$ID[2] <- NA

	expect_error(
		gfs_prepare(data),
		"Missing participant"
	)

})

test_that("gfs_prepare validates arguments", {

	expect_error(
		gfs_prepare(gfs_test_data(), waves = 4)
	)

	expect_error(
		gfs_prepare(gfs_test_data(), variables = "UNKNOWN")
	)

	expect_error(
		gfs_prepare(gfs_test_data(), format = "invalid")
	)

	expect_error(
		gfs_prepare(gfs_test_data(), release = 2)
	)

	expect_error(
		gfs_prepare(gfs_test_data(), keep_weights = NA)
	)

})
