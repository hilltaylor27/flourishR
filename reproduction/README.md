# Reproducing the manuscript analyses

This directory contains the R script used to reproduce the empirical application presented in *flourishR: An R Package for Cross-National and Longitudinal Wellbeing Research*.

## Data

The analyses use the March 2026 release of the combined Wave 1–3 dataset from the Global Flourishing Study (GFS).

Data and accompanying documentation are available through the Global Flourishing Study:

https://www.cos.io/gfs-resources

https://doi.org/10.17605/OSF.IO/3JTZ8

Data from Waves 1 and 2 are publicly available. At the time of manuscript submission, access to Wave 3 requires preregistration through the Global Flourishing Study.

Participant-level data are not included in this repository. Users must obtain access to the dataset separately.

## Software

The analyses were conducted using R version 4.5.2 and the following package versions:

| Package | Version |
|---|---|
| flourishR | 0.2.0 |
| dplyr | 1.2.1 |
| ggplot2 | 4.0.4 |
| survey | 4.4.8 |
| lme4 | 1.1.37 |
| metafor | 5.2.1 |
| openxlsx | 4.2.8.1 |
| patchwork | 1.3.2 |

Version 0.2.0 of flourishR is available at:

https://github.com/hilltaylor27/flourishR/releases/tag/v0.2.0

The version used in the manuscript can be installed directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("hilltaylor27/flourishR@v0.2.0")
```

The remaining packages can be installed from CRAN:

```r
install.packages(c(
  "dplyr", "ggplot2", "survey", "lme4",
  "metafor", "openxlsx", "patchwork"
))
```

The package versions listed above correspond to those used in the manuscript analyses. The installation commands for the additional packages install the currently available CRAN versions.

## Reproducing the analyses

Set the path to the combined GFS dataset and run the reproduction script from the repository root.

```r
Sys.setenv(GFS_DATA_PATH = "/path/to/gfs_all_countries_Y1_Y2_Y3.csv")

source("reproduction/reproduce.R")
```

The script performs data preparation, retention and missingness analyses, survey-weighted descriptive analyses, cross-sectional and prospective regression, within- and between-person modelling, meta-analysis, and meta-regression.

The raw dataset can be stored separately from the repository. The script reads the data from the specified location without modifying the original dataset.

## Computational requirements

The complete reproduction script was run successfully on 26 November 2026 using the following computing environment:

- R version 4.5.2
- macOS (Darwin 22.6.0, x86_64)
- Intel Core i5-7360U processor (2.30 GHz)
- 8 GB RAM

The total elapsed runtime was 95.48 seconds (1.59 minutes), including data preparation, statistical analyses, workbook export, and figure generation. Runtime may vary across computing environments.

All 23 countries were retained in each analysis, and the reproduced numerical results matched those reported in the manuscript at the reported precision.

## Outputs

All outputs are saved to the `manuscript outputs` directory, including:

- `flourishR.manuscript.results.xlsx`
- `figures/retention.png`
- `figures/country_means.png`
- `figures/trajectories.png`
- `figures/cross_sectional.png`
- `figures/within_between.png`

The results workbook includes country-specific estimates, pooled estimates, model diagnostics, analytical specifications, and R session information.
