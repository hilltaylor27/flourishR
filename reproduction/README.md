# Reproducing the manuscript analyses

This directory contains the R script used to reproduce the empirical application presented in *flourishR: An R Package for Cross-National and Longitudinal Wellbeing Research*.

## Data

The analyses use the combined Wave 1–3 dataset from the Global Flourishing Study (GFS).

Data and accompanying documentation are available through the Global Flourishing Study:

https://www.cos.io/gfs-resources

Participant-level data are not included in this repository. Users must obtain access to the dataset separately.

## Software

The analyses were conducted using flourishR version 0.2.0 and the following R packages:

- dplyr
- ggplot2
- survey
- lme4
- metafor
- openxlsx
- patchwork

## Reproducing the analyses

Set the path to the combined GFS dataset and run the reproduction script from the repository root.

```r
Sys.setenv(GFS_DATA_PATH = '/path/to/gfs_all_countries_Y1_Y2_Y3.csv')

source('reproduction/reproduce.R')
```

The script performs data preparation, retention and missingness analyses, survey-weighted descriptive analyses, cross-sectional and prospective regression, within- and between-person modelling, meta-analysis and meta-regression.

## Outputs

All outputs are saved to the `manuscript outputs` directory, including:

- `flourishR.manuscript.results.xlsx`
- `figures/retention.png`
- `figures/country_means.png`
- `figures/trajectories.png`
- `figures/cross_sectional.png`
- `figures/within_between.png`

The results workbook includes country-specific estimates, pooled estimates, model diagnostics, analytical specifications and R session information.
