test_that("gfs_countries returns the country lookup", {

	result <- gfs_countries()

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 23)
	expect_false(anyDuplicated(result$country_code) > 0)
	expect_false(anyNA(result$country_label))

})

test_that("gfs_countries converts numerical identifiers", {

	expect_equal(
		gfs_countries(c(1, 9, 20, 22, 25)),
		c(
			"Argentina",
			"Japan",
			"United Kingdom",
			"United States",
			"China"
		)
	)

})

test_that("gfs_countries accepts character identifiers", {

	expect_equal(
		gfs_countries(c("1", "9", "20")),
		c("Argentina", "Japan", "United Kingdom")
	)

})

test_that("gfs_countries preserves existing country labels", {

	expect_equal(
		gfs_countries(c("Argentina", "Japan")),
		c("Argentina", "Japan")
	)

})

test_that("gfs_countries preserves missing values", {

	expect_equal(
		gfs_countries(c(1, NA, 9)),
		c("Argentina", NA_character_, "Japan")
	)

})

test_that("gfs_countries handles unknown identifiers", {

	expect_equal(
		gfs_countries(999),
		NA_character_
	)

	expect_equal(
		gfs_countries(999, unknown = "keep"),
		"999"
	)

	expect_error(
		gfs_countries(999, unknown = "error")
	)

})

test_that("gfs_countries returns factors when requested", {

	result <- gfs_countries(
		c(1, 9, 20),
		as_factor = TRUE
	)

	expect_true(is.factor(result))
	expect_equal(length(levels(result)), 23)

	expect_equal(
		as.character(result),
		c("Argentina", "Japan", "United Kingdom")
	)

})
