test_that("gfs_weights selects cross-sectional weights", {

	expect_equal(gfs_weights(1), "ANNUAL_WEIGHT_C1")
	expect_equal(gfs_weights(2), "ANNUAL_WEIGHT_C2")
	expect_equal(gfs_weights(3), "ANNUAL_WEIGHT_C3")

})

test_that("gfs_weights selects longitudinal weights", {

	expect_equal(gfs_weights(1:2), "ANNUAL_WEIGHT_L2")
	expect_equal(gfs_weights(1:3), "ANNUAL_WEIGHT_L3")
	expect_equal(gfs_weights(c(3, 1, 2)), "ANNUAL_WEIGHT_L3")

})

test_that("gfs_weights selects all-record weights", {

	expect_equal(
		gfs_weights(2, type = "all records"),
		"ANNUAL_WEIGHT_R2"
	)

	expect_equal(
		gfs_weights(3, type = "all records"),
		"ANNUAL_WEIGHT_R3"
	)

})

test_that("gfs_weights rejects unsupported combinations", {

	expect_error(gfs_weights(c(1, 3)))
	expect_error(gfs_weights(2:3))
	expect_error(gfs_weights(4))
	expect_error(gfs_weights(NA))
	expect_error(gfs_weights(1, type = "all records"))
	expect_error(gfs_weights(1:3, type = "cross-sectional"))

})

test_that("gfs_weights validates available weights", {

	data <- data.frame(
		ANNUAL_WEIGHT_C1 = c(0.5, 1, 1.5, NA)
	)

	expect_equal(
		gfs_weights(1, data = data),
		"ANNUAL_WEIGHT_C1"
	)

	expect_error(gfs_weights(3, data = data))

	data$ANNUAL_WEIGHT_C1[1] <- -1

	expect_error(gfs_weights(1, data = data))

	data$ANNUAL_WEIGHT_C1 <- rep(NA_real_, 4)

	expect_error(gfs_weights(1, data = data))

})
