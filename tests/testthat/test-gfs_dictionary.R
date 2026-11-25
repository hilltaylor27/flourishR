test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("gfs_dictionary returns variable metadata", {

	result <- gfs_dictionary()

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 172)
	expect_false(anyDuplicated(result$variable) > 0)

})

test_that("gfs_dictionary filters variables correctly", {

	result <- gfs_dictionary(variable = "life_sat")

	expect_equal(nrow(result), 1)
	expect_equal(result$variable, "LIFE_SAT")
	expect_true(result$year1)
	expect_true(result$year2)
	expect_true(result$year3)
	expect_true(result$repeated)

})

test_that("gfs_dictionary filters annual waves correctly", {

	result <- gfs_dictionary(wave = 1:3)

	expect_true(nrow(result) > 0)
	expect_true(all(result$year1))
	expect_true(all(result$year2))
	expect_true(all(result$year3))

	expect_false("GENDER" %in% result$variable)
	expect_true("LIFE_SAT" %in% result$variable)

})

test_that("gfs_dictionary identifies variable-specific missing codes", {

	age <- gfs_dictionary(
		variable = "AGE",
		type = "missing"
	)

	expect_setequal(age$value, c(-998, 998, 999))
	expect_false(99 %in% age$value)

	life_sat <- gfs_dictionary(
		variable = "LIFE_SAT",
		type = "missing"
	)

	expect_setequal(life_sat$value, c(-98, 98, 99))
	expect_false(any(0:10 %in% life_sat$value))

})

test_that("gfs_dictionary returns survey weight metadata", {

	result <- gfs_dictionary(type = "weights")

	expect_equal(nrow(result), 10)
	expect_true(all(result$inference == "within-country"))
	expect_false(anyNA(result$population))

	expect_true(
		"ANNUAL_WEIGHT_L3" %in% result$variable
	)

})

test_that("gfs_dictionary searches variable names", {

	result <- gfs_dictionary(search = "LIFE_SAT")

	expect_true("LIFE_SAT" %in% result$variable)

})

test_that("gfs_dictionary handles invalid arguments", {

	expect_error(gfs_dictionary(wave = 4))
	expect_error(gfs_dictionary(wave = NA))
	expect_error(gfs_dictionary(release = 1))
	expect_error(gfs_dictionary(type = "invalid"))

	expect_error(
		gfs_dictionary(type = "missing", wave = 3)
	)

	expect_equal(
		nrow(gfs_dictionary(variable = "UNKNOWN")),
		0
	)

})
