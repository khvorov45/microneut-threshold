# Resample data and simulate the testing process

library(tidyverse)
library(rsample)
library(furrr)

plan(multiprocess)

data_dir <- here::here("data")
resample_dir <- here::here("resample")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

test_micro_only <- function(data, index, logthreshold,
                            titrename = "logtitre1") {
  logtitre <- rlang::sym(titrename)
  data %>%
    filter(!is.na(inf), !is.na(!!logtitre)) %>%
    mutate(
      micro_only = if_else(!!logtitre < logthreshold, 0L, 1L)
    ) %>%
    group_by(inf) %>%
    summarise(
      prop_pos = sum(micro_only) / n(), index = index, type = "micro_only",
      logthreshold = logthreshold
    )
}

calc_sens_spec <- function(results) {
  results %>%
    mutate(
      prop_pos = if_else(inf == 1L, prop_pos, 1 - prop_pos),
      inf = if_else(inf == 1L, "Sensitivity", "Specificity"),
    ) %>%
    rename(test_char = prop_pos, char = inf)
}

resample_and_test <- function(data, n_resamples, micro_thresholds,
                              titrename = "logtitre1") {
  boots <- bootstraps(data)$splits
  res <- future_map_dfr(
    micro_thresholds,
    function(threshold) {
      imap_dfr(boots, ~ test_micro_only(analysis(.x), .y, threshold, titrename))
    }
  )
  calc_sens_spec(res)
}

save_resample <- function(data, name) {
  write_csv(
    data, file.path(resample_dir, glue::glue("resample-{name}.csv"))
  )
}

# Script ======================================================================

sim_data <- read_data("sim")
suellen_data <- read_data("suellen")

res <- list(
  sim = resample_and_test(
    sim_data, 250, seq(log(20), log(80), length.out = 100)
  ),
  suellen = resample_and_test(
    suellen_data, 250, seq(log(20), log(80), length.out = 100), "logtitre"
  )
)

iwalk(res, save_resample)
