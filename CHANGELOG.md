# flourishR Changelog

Development history for the flourishR package.

Earlier development history is retained in the original Word CHANGELOG.
Package-hardening updates are recorded here.

## 2026-08-28

### label_to_factor

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Added input validation for variables argument
- Generated package documentation and export
- Documentation example passes
- 11 test expectations pass

Notes / limitations:
- Real-data validation using UKHLS and/or GFS still required

### recode_missing

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Added variables argument validation
- Tested default and custom missing codes
- Tested preservation of existing NA values
- Tested that unselected variables remain unchanged
- Documentation examples passed
- 13 automated test expectations passed

Notes / limitations:
- Real-data validation using UKHLS and/or GFS still required

### check_missingness

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Tested missingness counts and percentages
- Tested variable selection and missing-variable errors
- Documentation examples passed
- 17 automated test expectations passed

### score_scale

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Tested item validation, missing-data thresholds, valid ranges, and output naming
- Tested sum, mean, prorated, and valid-item outputs
- Documentation examples passed
- 35 automated test expectations passed

### score_swemwbs

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Integrated scoring through score_scale()
- Enforced seven-item SWEMWBS structure
- Tested valid range and missing-item handling
- Documentation examples passed
- 19 automated test expectations passed

### loop_countries

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Fixed exclusion of missing country values during country-specific subsetting
- Tested forwarding of additional arguments
- Tested multi-row outputs and country ordering
- Documentation examples passed
- 21 automated test expectations passed

### plot_cross_national

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Added package-safe tidy evaluation using rlang::.data
- Tested country means, ordering, missing outcomes, labels, and plot output
- Documentation examples passed

### plot_longitudinal

Status: Package-ready — automated tests passed

Changes:
- Added full roxygen2 documentation
- Added formal testthat test suite
- Added package-safe tidy evaluation using rlang::.data
- Tested wave means, standard errors, missing outcomes, labels, and optional individual trajectories
- Documentation examples passed


## 2026-08-28

### Real-data validation: Global Flourishing Study

Validated package functions against the cleaned three-wave Global Flourishing Study dataset
(n = 207,919).

Validated successfully:
- check_missingness(): matched direct missing-data counts and percentages
- score_scale(): exactly reproduced the existing GFS extraversion score where both scores were observed
- loop_countries(): matched manual country-specific calculations
- plot_cross_national(): plotted country means matched direct calculations
- plot_longitudinal(): plotted wave means and standard errors matched direct calculations

Not applicable to this cleaned GFS dataset:
- recode_missing(): survey-specific missing codes had already been recoded
- label_to_factor(): labelled attributes were not retained in the cleaned dataset
- score_swemwbs(): SWEMWBS items were not present
