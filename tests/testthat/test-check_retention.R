retention_test_data <- function() {

	data.frame(
		ID = rep(1:4, each = 3),
		COUNTRY = rep(c("Japan", "Japan", "Canada", "Canada"), each = 3),
		wave = rep(1:3, 4),
		WAVE = c(
			1, 2, 3,
			1, NA, 3,
			1, 2, NA,
			1, NA, NA
		)
	)

}

test_that("check_retention calculates annual retention", {

	result <- check_retention(retention_test_data())

	expect_equal(result$n_baseline, rep(4, 3))
	expect_equal(result$n_participants, c(4, 2, 2))
	expect_equal(result$n_nonparticipants, c(0, 2, 2))
	expect_equal(result$retention_pct, c(100, 50, 50))

})

test_that("check_retention calculates cumulative retention", {

	result <- check_retention(retention_test_data())

	expect_equal(result$n_cumulative, c(4, 2, 1))
	expect_equal(result$cumulative_pct, c(100, 50, 25))

})

test_that("check_retention supports grouped summaries", {

	result <- check_retention(
		retention_test_data(),
		group = "COUNTRY"
	)

	expect_equal(nrow(result), 6)

	expect_equal(
		result$n_participants[result$COUNTRY == "Japan"],
		c(2, 1, 2)
	)

})

test_that("check_retention supports explicit participation values", {

	data <- retention_test_data()

	data$participated <- as.integer(!is.na(data$WAVE))

	result <- check_retention(
		data,
		participation = "participated",
		participated_values = 1
	)

	expect_equal(result$n_participants, c(4, 2, 2))

})

test_that("check_retention supports unbalanced data", {

	data <- retention_test_data()

	data <- data[!is.na(data$WAVE), ]

	result <- check_retention(data)

	expect_equal(result$n_participants, c(4, 2, 2))

})

test_that("check_retention validates input", {

	data <- retention_test_data()

	expect_error(check_retention(data, id = "UNKNOWN"))

	expect_error(
		check_retention(rbind(data, data[1, ]))
	)

	expect_error(check_retention(data, baseline = 4))

})
