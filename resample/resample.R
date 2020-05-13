# Resample data and simulate the testing process

library(tidyverse)
library(rsample)
library(furrr)

plan(multiprocess)

data_dir <- here::here("data")
resample_dir <- here::here("resample")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

test_micro_only <- function(data, index, logthreshold) {
  data %>%
    filter(!is.na(inf), !is.na(logtitre)) %>%
    mutate(
      micro_only = if_else(logtitre < logthreshold, 0L, 1L)
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
      inf = if_else(inf == 1L, "sens", "spec"),
      prop_pos = if_else(inf == 1L, prop_pos, 1 - prop_pos)
    ) %>%
    rename(test_char = prop_pos, char = inf)
}

resample_and_test <- function(data, n_resamples, micro_thresholds) {
  boots <- bootstraps(data)$splits
  res <- map_dfr(
    micro_thresholds,
    function(threshold) {
      imap_dfr(boots, ~ test_micro_only(analysis(.x), .y, threshold))
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

data <- map(c("sim" = "sim", "suellen" = "suellen"), read_data)

res <- map(data, resample_and_test, 100, seq(log(20), log(80), length.out = 25))

iwalk(res, save_resample)
