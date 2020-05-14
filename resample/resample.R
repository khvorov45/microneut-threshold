# Resample data and simulate the testing process

library(tidyverse)
library(rsample)
library(furrr)

plan(multiprocess)

data_dir <- here::here("data")
resample_dir <- here::here("resample")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

test_elisa_micro <- function(data, index, threshold_elisa,
                             logthreshold_titre,
                             elisaname = "logtitre2",
                             titrename = "logtitre") {
  logtitre <- rlang::sym(titrename)
  logtitre2 <- rlang::sym(elisaname)
  data %>%
    filter(!is.na(inf), !is.na(!!logtitre), !is.na(!!logtitre2)) %>%
    mutate(
      micro_only = if_else(!!logtitre < logthreshold_titre, 0L, 1L),
      elisa_micro = case_when(
        !!logtitre2 > threshold_elisa ~ 1L,
        !!logtitre > logthreshold_titre ~ 1L,
        TRUE ~ 0L
      ),
    ) %>%
    group_by(inf) %>%
    summarise(
      prop_pos1 = sum(micro_only) / n(),
      prop_pos2 = sum(elisa_micro) / n(), index = index,
      logthreshold_titre = logthreshold_titre, threshold_elisa = threshold_elisa
    )
}

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
      prop_pos = sum(micro_only) / n(), index = index,
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

calc_sens_spec2 <- function(results) {
  results %>%
    mutate(
      prop_pos1 = if_else(inf == 1L, prop_pos1, 1 - prop_pos1),
      prop_pos2 = if_else(inf == 1L, prop_pos2, 1 - prop_pos2),
      inf = if_else(inf == 1L, "Sensitivity", "Specificity"),
    ) %>%
    rename(test_char1 = prop_pos1, test_char2 = prop_pos2, char = inf)
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

resample_and_test_2 <- function(data, n_resamples, micro_thresholds,
                                elisa_thresholds,
                                titrename = "logtitre1",
                                elisaname = "logtitre2") {
  boots <- bootstraps(data)$splits
  res_elisa_micro <- future_map_dfr(
    elisa_thresholds,
    function(elisa_threshold) {
      map_dfr(
        micro_thresholds,
        function(micro_threshold) {
          imap_dfr(
            boots, ~ test_elisa_micro(
              analysis(.x), .y, elisa_threshold, micro_threshold, elisaname,
              titrename
            )
          )
        }
      )
    }
  )
  calc_sens_spec2(res_elisa_micro)
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

res2 <- list(
  sim2 = resample_and_test_2(
    sim_data, 25,
    seq(log(20), log(80), length.out = 10),
    seq(log(20), log(80), length.out = 10)
  ),
  "suellen-iga" = resample_and_test_2(
    suellen_data, 25,
    seq(log(20), log(80), length.out = 10),
    seq(2.5, 5, length.out = 5),
    "logtitre", "iga"
  ),
  "suellen-igg" = resample_and_test_2(
    suellen_data, 25,
    seq(log(20), log(80), length.out = 10),
    seq(0.5, 2.5, length.out = 5),
    "logtitre", "igg"
  )
)

all_res <- c(res, res2)

iwalk(all_res, save_resample)
