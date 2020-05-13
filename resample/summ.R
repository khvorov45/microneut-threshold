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
    group_by(char, type, logthreshold) %>%
    summarise(
      test_char_low = quantile(test_char, 0.025),
      test_char_high = quantile(test_char, 0.975),
      test_char = mean(test_char),
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
summs <- map(resmpls, summ)
iwalk(summs, save_summ)
