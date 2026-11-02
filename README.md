# flourishR

**Reproducible tools for cleaning, scoring, analysing, and visualising wellbeing survey data.**

`flourishR` is an R package for common tasks in the analysis of wellbeing and survey data. It provides lightweight functions for data preparation, scale scoring, missing-data checks, cross-national analysis, and longitudinal visualisation.

The package is being developed using workflows drawn from large-scale longitudinal and cross-national surveys, including Understanding Society (UKHLS) and the Global Flourishing Study (GFS). Development emphasises transparent defaults, explicit handling of missing data, reproducible workflows, and formal testing.

## Current status

`flourishR` is currently under development.

Current development version: `0.0.0.9000`

Eight functions are currently implemented, documented, and covered by automated tests.

## Installation

The package is not currently available from CRAN.

During development, it can be installed locally from the package source:

```r
devtools::install()
```

## Workflow

The current functions support a simple survey-analysis workflow:

```text
Raw survey data
      |
      v
label_to_factor()
recode_missing()
      |
      v
check_missingness()
      |
      v
score_scale()
score_swemwbs()
      |
      v
loop_countries()
      |
      v
plot_cross_national()
plot_longitudinal()
```

Functions can also be used independently.

## Data preparation

### `label_to_factor()`

Converts labelled survey variables to factors while preserving their value labels.

```r
data <- label_to_factor(
  data = data,
  variables = c("sex", "employment_status")
)
```

### `recode_missing()`

Recodes survey-specific missing-value codes to `NA`.

```r
data <- recode_missing(
  data = data,
  variables = c("life_satisfaction", "happiness")
)
```

Custom missing codes can also be supplied.

```r
data <- recode_missing(
  data = data,
  variables = c("life_satisfaction", "happiness"),
  missing_codes = c(-9, -8, -7)
)
```

### `check_missingness()`

Summarises the number and percentage of missing observations for selected variables.

```r
check_missingness(
  data = data,
  variables = c("life_satisfaction", "happiness")
)
```

## Scale scoring

### `score_scale()`

Scores a multi-item scale and returns the number of valid items together with sum, mean, and prorated scores.

```r
data <- score_scale(
  data = data,
  items = c("item1", "item2", "item3", "item4"),
  prefix = "wellbeing",
  min_items = 3,
  valid_range = c(1, 5)
)
```

This creates:

```text
wellbeing.n.valid
wellbeing.sum
wellbeing.mean
wellbeing.prorated
```

### `score_swemwbs()`

Provides a convenient wrapper for scoring the seven SWEMWBS items using the general scale-scoring framework.

```r
data <- score_swemwbs(
  data = data,
  items = c(
    "swemwbs1",
    "swemwbs2",
    "swemwbs3",
    "swemwbs4",
    "swemwbs5",
    "swemwbs6",
    "swemwbs7"
  )
)
```

The function returns:

```text
swemwbs.n.valid
swemwbs.sum
swemwbs.mean
swemwbs.prorated
```

## Cross-national analysis

### `loop_countries()`

Applies an analysis function separately within each country and combines the results.

For example:

```r
country_missingness <- loop_countries(
  data = data,
  country_var = "country",
  analysis_function = check_missingness,
  variables = c("life_satisfaction", "happiness")
)
```

This allows the same analysis to be run consistently across countries without repeatedly writing country-specific code.

### `plot_cross_national()`

Plots mean outcome values across countries.

```r
plot_cross_national(
  data = data,
  country_var = "country",
  outcome_var = "happiness",
  plot_title = "Mean happiness by country",
  y_label = "Mean happiness"
)
```

Countries with no valid observations for the outcome are excluded, and the remaining countries are ordered by their mean outcome.

## Longitudinal analysis

### `plot_longitudinal()`

Plots mean outcome trajectories across waves with standard-error bars.

```r
plot_longitudinal(
  data = data,
  id_var = "participant_id",
  wave_var = "wave",
  outcome_var = "happiness",
  plot_title = "Mean happiness over waves",
  y_label = "Mean happiness"
)
```

Individual participant trajectories can also be displayed:

```r
plot_longitudinal(
  data = data,
  id_var = "participant_id",
  wave_var = "wave",
  outcome_var = "happiness",
  show_individuals = TRUE
)
```

## Example workflow

The functions are designed to work together while remaining usable independently.

A basic analysis might look like:

```r
library(flourishR)

data <- recode_missing(
  data = data,
  variables = c(
    "swemwbs1", "swemwbs2", "swemwbs3",
    "swemwbs4", "swemwbs5", "swemwbs6",
    "swemwbs7"
  )
)

check_missingness(
  data = data,
  variables = c(
    "swemwbs1", "swemwbs2", "swemwbs3",
    "swemwbs4", "swemwbs5", "swemwbs6",
    "swemwbs7"
  )
)

data <- score_swemwbs(
  data = data,
  items = c(
    "swemwbs1", "swemwbs2", "swemwbs3",
    "swemwbs4", "swemwbs5", "swemwbs6",
    "swemwbs7"
  )
)

plot_cross_national(
  data = data,
  country_var = "country",
  outcome_var = "swemwbs.mean"
)

plot_longitudinal(
  data = data,
  id_var = "participant_id",
  wave_var = "wave",
  outcome_var = "swemwbs.mean"
)
```

## Development

The package follows several development principles:

- reproducible workflows
- transparent defaults
- explicit handling of missing data
- clear documentation
- automated testing
- development against realistic survey-data structures
- incremental development with version tracking

The current package structure is:

```text
flourishR/
├── R/                  # package functions
├── man/                # generated documentation
├── tests/testthat/     # automated tests
├── dev/                # development tools and project inventory
├── DESCRIPTION         # package metadata and dependencies
├── NAMESPACE           # generated namespace
├── .Rbuildignore       # files excluded from package builds
└── CHANGELOG.md        # development history
```

`R/`, `man/`, and `tests/testthat/` contain the distributable package implementation. Development materials in `dev/` are excluded from package builds.

## Testing

All currently implemented functions have formal `testthat` coverage.

Package-level validation includes documentation examples, package installation and loading, dependency checks, and `R CMD check`.

## Author

Taylor Hill
