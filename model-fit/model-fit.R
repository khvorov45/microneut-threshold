# Fitting the model to the data

library(tidyverse)
library(here)
library(furrr)

plan(multiprocess)

# Directories used
data_dir <- here("data")
model_fit_dir <- here("model-fit")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

fit_linear <- function(data, outname = "logtitre1") {
  fit <- lm(as.formula(glue::glue("{outname} ~ inf")), data)
  pred_data <- tibble(inf = c(0L, 1L))
  preds <- predict(fit, pred_data, se.fit = TRUE)
  mutate(
    pred_data,
    fit_val = preds$fit,
    fit_se = preds$se.fit,
    fit_low = fit_val - qnorm(0.975) * fit_se,
    fit_high = fit_val + qnorm(0.975) * fit_se,
    res_sd = sqrt(mean(fit$residuals^2))
  )
}

predthresh_linear <- function(logthreshold, fit_lin) {
  calc_testchar <- function(logthreshold, inf, dist_mean, res_sd) {
    ifelse(
      inf,
      pnorm(logthreshold, dist_mean, res_sd, lower.tail = FALSE),
      pnorm(logthreshold, dist_mean, res_sd, lower.tail = TRUE)
    )
  }
  fit_lin %>%
    mutate(
      test_char = calc_testchar(logthreshold, inf, fit_val, res_sd),
      test_char_low = ifelse(
        inf,
        calc_testchar(logthreshold, inf, fit_low, res_sd),
        calc_testchar(logthreshold, inf, fit_high, res_sd)
      ),
      test_char_high = ifelse(
        inf,
        calc_testchar(logthreshold, inf, fit_high, res_sd),
        calc_testchar(logthreshold, inf, fit_low, res_sd)
      ),
      char = ifelse(inf, "Sensitivity", "Specificity"),
      logthreshold = local(logthreshold)
    )
}

predthresh_linear_many <- function(logthresholds, fit_lin) {
  map_dfr(logthresholds, predthresh_linear, fit_lin)
}

save_summ <- function(data, name) {
  write_csv(data, file.path(model_fit_dir, glue::glue("{name}-lin.csv")))
}

# Script ======================================================================

sim_data <- read_data("sim")
suellen_data <- read_data("suellen")

fits <- list(
  sim = fit_linear(sim_data),
  suellen = fit_linear(suellen_data, "logtitre")
)

preds <- map(fits, ~ predthresh_linear_many(
  seq(log(20), log(80), length.out = 25), .x
))

iwalk(preds, save_summ)
