# flourishR
# Real-data validation using Global Flourishing Study
# 28 August 2026


# Load package

devtools::load_all()


# Create validation dataset

gfs.validation <- gfs[
  ,
  c(
    "id",
    "country_name",
    "country.t1",
    "gender.t1",
    "tipi1.t1",
    "tipi2.t1",
    "tipi3.t1",
    "tipi4.t1",
    "tipi5.t1",
    "wb.lifesat.t1",
    "wb.lifesat.t2",
    "wb.lifesat.t3",
    "wb.happy.t1",
    "wb.happy.t2",
    "wb.happy.t3"
  )
]


# Basic structure

dim(gfs.validation)

str(gfs.validation)

summary(gfs.validation)


# Check classes

sapply(
  gfs.validation,
  class
)


# Inspect survey missing-value codes

sort(
  unique(gfs.validation$wb.lifesat.t1)
)

sort(
  unique(gfs.validation$wb.happy.t1)
)

sort(
  unique(gfs.validation$tipi1.t1)
)


# Inspect labels

attributes(gfs.validation$gender.t1)

attributes(gfs.validation$country.t1)

# ------------------------------------------------------------
# Validate check_missingness()
# ------------------------------------------------------------

missingness.validation <- check_missingness(
  data = gfs,
  variables = c(
    "wb.lifesat.t1",
    "wb.lifesat.t2",
    "wb.lifesat.t3",
    "wb.happy.t1",
    "wb.happy.t2",
    "wb.happy.t3"
  )
)

missingness.validation


# Check against direct base R calculations

expected.missing <- c(
  sum(is.na(gfs$wb.lifesat.t1)),
  sum(is.na(gfs$wb.lifesat.t2)),
  sum(is.na(gfs$wb.lifesat.t3)),
  sum(is.na(gfs$wb.happy.t1)),
  sum(is.na(gfs$wb.happy.t2)),
  sum(is.na(gfs$wb.happy.t3))
)

stopifnot(
  identical(
    missingness.validation$n_missing,
    expected.missing
  )
)

expected.pct <- round(
  expected.missing / nrow(gfs) * 100,
  2
)

stopifnot(
  identical(
    missingness.validation$pct_missing,
    expected.pct
  )
)

message("check_missingness(): GFS validation passed")


# ------------------------------------------------------------
# Validate score_scale()
# ------------------------------------------------------------

# Use the existing processed TIPI items to recreate
# the GFS extraversion score.

score.validation <- gfs[
  ,
  c(
    "tipi1_pos.t1",
    "tipi6_pos.t1",
    "extra.t1"
  )
]

score.validation <- score_scale(
  data = score.validation,
  items = c(
    "tipi1_pos.t1",
    "tipi6_pos.t1"
  ),
  prefix = "extra.validation",
  min_items = 2,
  valid_range = c(1, 7)
)


# Inspect agreement

summary(
  score.validation$extra.validation.mean
)

summary(
  score.validation$extra.t1
)

cor(
  score.validation$extra.validation.mean,
  score.validation$extra.t1,
  use = "complete.obs"
)


# Exact comparison where both scores are available

score.complete <- !is.na(score.validation$extra.validation.mean) &
  !is.na(score.validation$extra.t1)

max.diff <- max(
  abs(
    score.validation$extra.validation.mean[score.complete] -
      score.validation$extra.t1[score.complete]
  )
)

max.diff

stopifnot(
  max.diff < 1e-12
)

message("score_scale(): GFS validation passed")


# ------------------------------------------------------------
# Validate loop_countries()
# ------------------------------------------------------------

country.missingness <- loop_countries(
  data = gfs,
  country_var = "country_name",
  analysis_function = check_missingness,
  variables = c(
    "wb.lifesat.t1",
    "wb.happy.t1"
  )
)

head(country.missingness)

tail(country.missingness)


# Number of countries represented

n.countries <- length(
  unique(
    gfs$country_name[
      !is.na(gfs$country_name)
    ]
  )
)

stopifnot(
  nrow(country.missingness) ==
    n.countries * 2
)

stopifnot(
  all(
    c(
      "country_name",
      "variable",
      "n_missing",
      "pct_missing"
    ) %in% names(country.missingness)
  )
)


# Check one country manually

uk.manual <- check_missingness(
  data = gfs[
    !is.na(gfs$country_name) &
      gfs$country_name == "United Kingdom",
    ,
    drop = FALSE
  ],
  variables = c(
    "wb.lifesat.t1",
    "wb.happy.t1"
  )
)

uk.loop <- country.missingness[
  country.missingness$country_name == "United Kingdom",
  c(
    "variable",
    "n_missing",
    "pct_missing"
  )
]

stopifnot(
  identical(
    uk.loop$variable,
    uk.manual$variable
  )
)

stopifnot(
  identical(
    uk.loop$n_missing,
    uk.manual$n_missing
  )
)

stopifnot(
  identical(
    uk.loop$pct_missing,
    uk.manual$pct_missing
  )
)

message("loop_countries(): GFS validation passed")


# ------------------------------------------------------------
# Validate plot_cross_national()
# ------------------------------------------------------------

cross.national.plot <- plot_cross_national(
  data = gfs,
  country_var = "country_name",
  outcome_var = "wb.happy.t1",
  plot_title = "Mean happiness by country",
  y_label = "Mean happiness"
)

cross.national.plot


stopifnot(
  inherits(
    cross.national.plot,
    "ggplot"
  )
)


# Compare plotted means against direct country means

plot.data <- cross.national.plot$data

manual.country.means <- dplyr::group_by(
  gfs,
  .data$country_name
)

manual.country.means <- dplyr::summarise(
  manual.country.means,
  n = sum(!is.na(.data$wb.happy.t1)),
  mean_outcome = mean(
    .data$wb.happy.t1,
    na.rm = TRUE
  ),
  .groups = "drop"
)

manual.country.means <- dplyr::filter(
  manual.country.means,
  .data$n > 0
)

plot.means <- sort(
  plot.data$mean_outcome
)

manual.means <- sort(
  manual.country.means$mean_outcome
)

stopifnot(
  isTRUE(
    all.equal(
      plot.means,
      manual.means
    )
  )
)

message("plot_cross_national(): GFS validation passed")


# ------------------------------------------------------------
# Prepare long GFS data
# ------------------------------------------------------------

gfs.long <- data.frame(
  id = rep(
    gfs$id,
    times = 3
  ),
  wave = rep(
    1:3,
    each = nrow(gfs)
  ),
  happiness = c(
    gfs$wb.happy.t1,
    gfs$wb.happy.t2,
    gfs$wb.happy.t3
  )
)


# ------------------------------------------------------------
# Validate plot_longitudinal()
# ------------------------------------------------------------

longitudinal.plot <- plot_longitudinal(
  data = gfs.long,
  id_var = "id",
  wave_var = "wave",
  outcome_var = "happiness",
  plot_title = "Mean happiness over GFS waves",
  y_label = "Mean happiness"
)

longitudinal.plot


stopifnot(
  inherits(
    longitudinal.plot,
    "ggplot"
  )
)


# Extract plotted wave means

point.data <- ggplot2::layer_data(
  longitudinal.plot,
  2
)


# Calculate expected wave means directly

expected.wave.means <- c(
  mean(
    gfs$wb.happy.t1,
    na.rm = TRUE
  ),
  mean(
    gfs$wb.happy.t2,
    na.rm = TRUE
  ),
  mean(
    gfs$wb.happy.t3,
    na.rm = TRUE
  )
)

stopifnot(
  isTRUE(
    all.equal(
      point.data$y,
      expected.wave.means
    )
  )
)


# Expected standard errors

expected.wave.se <- c(
  stats::sd(
    gfs$wb.happy.t1,
    na.rm = TRUE
  ) /
    sqrt(
      sum(
        !is.na(gfs$wb.happy.t1)
      )
    ),

  stats::sd(
    gfs$wb.happy.t2,
    na.rm = TRUE
  ) /
    sqrt(
      sum(
        !is.na(gfs$wb.happy.t2)
      )
    ),

  stats::sd(
    gfs$wb.happy.t3,
    na.rm = TRUE
  ) /
    sqrt(
      sum(
        !is.na(gfs$wb.happy.t3)
      )
    )
)


errorbar.data <- ggplot2::layer_data(
  longitudinal.plot,
  3
)

observed.wave.se <- (
  errorbar.data$ymax -
    errorbar.data$ymin
) / 2

stopifnot(
  isTRUE(
    all.equal(
      observed.wave.se,
      expected.wave.se
    )
  )
)

message("plot_longitudinal(): GFS validation passed")


