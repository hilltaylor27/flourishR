# packages

# use flourishR 0.2.0 and the March 2026 GFS combined dataset

library(flourishR)
library(dplyr)
library(ggplot2)
library(survey)
library(lme4)
library(metafor)
library(openxlsx)
library(patchwork)

stopifnot(packageVersion('flourishR') == '0.2.0')

# paths

gfs.path <- Sys.getenv('GFS_DATA_PATH', unset = 'gfs_all_countries_Y1_Y2_Y3.csv')
output.path <- 'manuscript outputs'
figures.path <- file.path(output.path, 'figures')
if (!file.exists(gfs.path)) stop('Set GFS_DATA_PATH to the combined Wave 1–3 CSV file.')
dir.create(figures.path, recursive = TRUE, showWarnings = FALSE)

# data preparation

gfs <- gfs_prepare(gfs.path, waves = 1:3,
                   variables = c('AGE', 'GENDER', 'LIFE_SAT', 'LONELY'),
                   format = 'long')
stopifnot(!anyDuplicated(gfs[c('ID', 'wave')]),
          n_distinct(gfs$COUNTRY) == 23,
          all(gfs$LONELY[!is.na(gfs$LONELY)] %in% 0:10),
          all(gfs$LIFE_SAT[!is.na(gfs$LIFE_SAT)] %in% 0:10))
gfs <- mutate(gfs, LONELINESS = 10 - LONELY)

coverage <- gfs %>% group_by(wave) %>% summarise(
  n_records = n(), n_participants = sum(!is.na(WAVE)),
  n_life_sat = sum(!is.na(WAVE) & !is.na(LIFE_SAT)),
  n_loneliness = sum(!is.na(WAVE) & !is.na(LONELINESS)), .groups = 'drop')
missingness <- check_missingness(filter(gfs, !is.na(WAVE)),
  variables = c('LIFE_SAT', 'LONELINESS', 'AGE', 'GENDER'),
  by = c('COUNTRY', 'wave'))

# retention

retention <- check_retention(gfs)
retention.country <- check_retention(gfs, group = 'COUNTRY')

# descriptive statistics

trajectories <- bind_rows(lapply(1:3, function(w) {
  describe_countries(gfs, variables = c('LIFE_SAT', 'LONELINESS'),
                     wave = w, lonely_psu = 'adjust')
}))
descriptives <- filter(trajectories, wave == 3)

# cross-national associations

results.cross <- loop_countries(gfs, country_var = 'COUNTRY',
  formula = LIFE_SAT ~ LONELINESS + AGE + factor(GENDER),
  wave = 3, weighted = TRUE, lonely_psu = 'adjust')

# prospective associations

# C3 covers Wave 1 and Wave 3 participation; Wave 2 is not required.
gfs.long <- create_lags(gfs, variables = c('LIFE_SAT', 'LONELINESS', 'AGE'), lag = 2)
results.prospective <- loop_countries(gfs.long, country_var = 'COUNTRY',
  formula = LIFE_SAT ~ LONELINESS_lag2 + LIFE_SAT_lag2 + AGE_lag2 + factor(GENDER),
  wave = 3, weighted = TRUE, lonely_psu = 'adjust')

# within- and between-person associations

# Calculate person means over observed loneliness responses at participating waves.
gfs.mixed <- within_between(filter(gfs, !is.na(WAVE)), variables = 'LONELINESS')
results.mixed <- loop_longitudinal(gfs.mixed,
  formula = LIFE_SAT ~ LONELINESS_within + LONELINESS_between +
    factor(wave) + AGE + factor(GENDER),
  waves = 1:3, model = 'mixed')

# heterogeneity and pooled associations

models <- list(cross_sectional = results.cross, prospective = results.prospective,
               within_person = results.mixed, between_person = results.mixed)
terms <- c(cross_sectional = 'LONELINESS', prospective = 'LONELINESS_lag2',
           within_person = 'LONELINESS_within', between_person = 'LONELINESS_between')

heterogeneity <- bind_rows(lapply(names(models), function(m) {
  mutate(country_moderation(models[[m]], term = terms[[m]]), analysis = m)
}))
pooled <- bind_rows(lapply(names(models), function(m) {
  mutate(meta_countries(models[[m]], term = terms[[m]]), analysis = m)
}))

# country-level moderation

context <- trajectories %>% filter(wave == 1, variable == 'LIFE_SAT', status == 'ok') %>%
  transmute(COUNTRY, national.life.sat = mean)
results.context <- join_country_data(results.mixed, context)
meta.context <- meta_countries(results.context, term = 'LONELINESS_within',
                              moderators = ~ national.life.sat)

# model audit

audit <- bind_rows(lapply(names(models), function(m) {
  d <- models[[m]]
  target <- filter(d, term == terms[[m]], status == 'ok',
                   is.finite(estimate), is.finite(std.error), std.error > 0)
  data.frame(analysis = m, n_countries = n_distinct(target$COUNTRY),
             excluded = paste(setdiff(unique(gfs$COUNTRY), target$COUNTRY), collapse = '; '),
             n_model_records = sum(target$n),
             n_people = if ('n_people' %in% names(target)) sum(target$n_people) else sum(target$n),
             singular_countries = if ('singular' %in% names(target)) sum(target$singular, na.rm = TRUE) else NA_integer_)
}))

stopifnot(all(audit$n_countries == 23),
          all(trajectories$status == 'ok'),
          all(audit$singular_countries[!is.na(audit$singular_countries)] == 0))

# results workbook

metadata <- data.frame(item = c('package version', 'R version', 'data', 'loneliness coding',
  'cross-sectional weight', 'prospective weight', 'prospective covariates', 'mixed models',
  'person means', 'survey variance', 'meta-analysis'),
  value = c(as.character(packageVersion('flourishR')), R.version.string, basename(gfs.path),
  '10 - LONELY; higher scores indicate greater loneliness',
  gfs_weights(3), gfs_weights(3), 'Wave 1 age and life satisfaction; baseline gender',
  'Unweighted random intercept; all available complete model records',
  'All observed loneliness responses at participating waves before model-specific deletion',
  'adjust for single-PSU strata', 'REML with Knapp-Hartung inference'))

outputs <- list(metadata = metadata, coverage = coverage, missingness = missingness,
  retention = retention, retention_country = retention.country, descriptives = trajectories,
  cross_sectional = results.cross, prospective = results.prospective, mixed = results.mixed,
  model_audit = audit, heterogeneity = heterogeneity, pooled = pooled,
  country_context = context, meta_regression = meta.context,
  session = data.frame(info = capture.output(sessionInfo())))
openxlsx::write.xlsx(outputs, file.path(output.path, 'flourishR.manuscript.results.xlsx'), overwrite = TRUE)

# figures

p1 <- plot_retention(retention.country, group = 'COUNTRY', cumulative = TRUE) +
  scale_x_continuous(breaks = 1:3) + labs(y = 'Cumulative retention (%)')
p2 <- plot_cross_national(descriptives, country_var = 'COUNTRY', outcome_var = 'LIFE_SAT',
                         estimates = TRUE, y_label = 'Mean life satisfaction')
p3 <- plot_longitudinal(trajectories, id_var = 'ID', wave_var = 'wave',
  outcome_var = 'LIFE_SAT', country_var = 'COUNTRY', estimates = TRUE,
  y_label = 'Mean life satisfaction') + scale_x_continuous(breaks = 1:3)
p4 <- plot_country_effects(results.cross, term = 'LONELINESS',
  pooled = filter(pooled, analysis == 'cross_sectional'), x_label = 'Cross-sectional association')
p5a <- plot_country_effects(results.mixed, term = 'LONELINESS_within',
  pooled = filter(pooled, analysis == 'within_person'), x_label = 'Within-person association')
p5b <- plot_country_effects(results.mixed, term = 'LONELINESS_between',
  pooled = filter(pooled, analysis == 'between_person'), x_label = 'Between-person association')
p5 <- p5a + p5b + plot_layout(ncol = 2)

plots <- list(retention = p1, country_means = p2, trajectories = p3,
              cross_sectional = p4, within_between = p5)
widths <- c(12, 8, 12, 8, 14)
heights <- c(9, 9, 9, 9, 9)
for (i in seq_along(plots)) {
  ggsave(file.path(figures.path, paste0(names(plots)[i], '.png')),
         plots[[i]], width = widths[i], height = heights[i], dpi = 300, bg = 'white')
}

# results

print(coverage)
print(retention)
print(audit)
print(heterogeneity)
print(pooled)
print(meta.context)
print(table(trajectories$status, useNA = 'ifany'))
cat('\nResults saved to:', output.path, '\n')
