# Fitting the model to the data

library(tidyverse)
library(here)
library(furrr)

plan(multiprocess)

# Directories used
data_dir <- "data"
model_fit_dir <- "model-fit"

# Functions ===================================================================

read_data <- function(name) {
  read_csv(
    file.path(data_dir, glue::glue("{name}.csv")),
    col_types = cols(inf = col_integer())
  )
}

fit_linear <- function(data) {
  fit <- lm(logtitre ~ inf, data)
  pred_data <- tibble(inf = c(0L, 1L))
  preds <- predict(fit, tibble(inf = c(0L, 1L)), se.fit = TRUE)
  mutate(
    pred_data,
    fit_val = preds$fit,
    fit_se = preds$se.fit,
    res_sd = sd(fit$residuals)
  )
}

predthresh_linear <- function(threshold, fit_lin) {
  fit_lin %>%
    mutate(
      dist_mean = rnorm(n(), fit_val, fit_se),
      test_char = ifelse(
        inf,
        pnorm(threshold, dist_mean, res_sd, lower.tail = FALSE),
        pnorm(threshold, dist_mean, res_sd, lower.tail = TRUE)
      ),
      char = ifelse(inf, "Sensitivity", "Specificity"),
      threshold = local(threshold)
    )
}

predthresh_linear_many <- function(thresholds, fit_lin) {
  map_dfr(thresholds, predthresh_linear, fit_lin)
}

predict_thresholds <- function(n_samples, thresholds, fit_lin) {
  future_map_dfr(
    1:n_samples, ~ predthresh_linear_many(thresholds, fit_lin) %>%
      mutate(ind = .x)
  )
}

summ_preds <- function(preds) {
  preds %>%
    group_by(char, threshold) %>%
    summarise(test_char = list(quantile(test_char, c(0.025, 0.5, 0.975)))) %>%
    ungroup() %>%
    unnest_wider(test_char) %>%
    rename(low = `2.5%`, med = `50%`, high = `97.5%`)
}

save_summ <- function(data, name) {
  write_csv(data, file.path(model_fit_dir, glue::glue("{name}-lin.csv")))
}

# Script ======================================================================

data <- map(c("sim" = "sim", "suellen" = "suellen"), read_data)

fit_lin <- map(data, fit_linear)

preds <- map(fit_lin, ~ predict_thresholds(
  1e3, seq(log(20), log(40), length.out = 101), .x
))

summs <- map(preds, summ_preds)

iwalk(summs, save_summ)
