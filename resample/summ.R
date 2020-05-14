# Summarise the resamples

library(tidyverse)

resample_dir <- here::here("resample")

# Functions ===================================================================

read_resample <- function(name) {
  read_csv(
    file.path(resample_dir, glue::glue("resample-{name}.csv")),
    col_types = cols()
  )
}

summ <- function(result) {
  result %>%
    group_by(char, logthreshold) %>%
    summarise(
      test_char_low = quantile(test_char, 0.025),
      test_char_high = quantile(test_char, 0.975),
      test_char = mean(test_char),
    ) %>%
    ungroup()
}

summ2 <- function(result) {
  result %>%
    mutate(diff = test_char1 - test_char2) %>%
    group_by(char, logthreshold_titre, threshold_elisa) %>%
    summarise(
      test_char1_low = quantile(test_char1, 0.025),
      test_char1_high = quantile(test_char1, 0.975),
      test_char1 = mean(test_char1),
      test_char2_low = quantile(test_char2, 0.025),
      test_char2_high = quantile(test_char2, 0.975),
      test_char2 = mean(test_char2),
      diff_low = quantile(diff, 0.025),
      diff_high = quantile(diff, 0.975),
      diff = mean(diff)
    ) %>%
    ungroup()
}

save_summ <- function(summ, name) {
  write_csv(
    summ, file.path(resample_dir, glue::glue("summ-{name}.csv"))
  )
}

# Script ======================================================================

resmpls <- map(c("sim" = "sim", "suellen" = "suellen"), read_resample)
resmpls2 <- map(
  c(
    "sim2" = "sim2",
    "suellen-iga" = "suellen-iga",
    "suellen-igg" = "suellen-igg"
  ),
  read_resample
)

summs <- map(resmpls, summ)
summs2 <- map(resmpls2, summ2)

all_summ <- c(summs, summs2)

iwalk(all_summ, save_summ)
