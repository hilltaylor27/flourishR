# flourishR

<!-- badges: start -->
[![R-CMD-check](https://github.com/hilltaylor27/flourishR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hilltaylor27/flourishR/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/flourishR)](https://CRAN.R-project.org/package=flourishR)
<!-- badges: end -->

*flourishR* supports reproducible cross-national and longitudinal survey research, with dedicated tools for the Global Flourishing Study (GFS).

## Installation

```r
install.packages("flourishR")
# latest GitHub version
remotes::install_github("hilltaylor27/flourishR")
```

The workflow described here requires version 0.2.0. Until this version is available on CRAN, install it from GitHub. Optional packages `survey`, `metafor` and `lme4` enable the corresponding statistical methods.

## GFS workflow

```r
library(flourishR)
gfs <- gfs_prepare("path/to/combined_gfs.csv", waves = 1:3)
gfs <- score_tipi(gfs)
retention <- check_retention(gfs, group = "COUNTRY")
plot_retention(retention, group = "COUNTRY")
```

### Data preparation

`label_to_factor()`, `recode_missing()`, `check_missingness()`, `gfs_dictionary()`, `gfs_countries()`, `gfs_prepare()`, `join_country_data()`.

### Survey design and retention

`gfs_weights()`, `gfs_design()`, `check_retention()`.

### Cross-national analysis

`describe_countries()`, `loop_countries()`, `country_moderation()`, `meta_countries()`.

### Longitudinal analysis

`create_lags()`, `change_scores()`, `within_between()`, `loop_longitudinal()`.

### Visualisation

`plot_cross_national()`, `plot_longitudinal()`, `plot_country_effects()`, `plot_retention()`, `plot_missingness()`.

### Personality scoring

`score_tipi()`.

See `vignette("gfs-workflow", package = "flourishR")` for a complete runnable synthetic-data example. GFS participant data are not distributed with this package. See https://www.cos.io/gfs-resources for study resources and access information.

Survey weights support within-country inference. Mixed-effects models are unweighted; country meta-analysis is not a population-weighted global estimate. Inspect model status, warnings and singularity before interpreting estimates.


## Citation

To cite flourishR in publications:

```r
citation("flourishR")
```

Hill, T. G. (2026). *flourishR: Tools for Reproducible
Wellbeing and Survey Data Analysis*.
https://doi.org/10.32614/CRAN.package.flourishR

## License and support

flourishR is distributed under the MIT license.

Report bugs or suggest improvements through
[GitHub Issues](https://github.com/hilltaylor27/flourishR/issues).
