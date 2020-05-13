# Extract Suellen data

library(tidyverse)
library(readxl)
library(here)

# Directories used
data_raw_dir <- here("data-raw")
data_dir <- here("data")

# Functions ===================================================================

censor_titres <- function(data) {
  data %>%
    mutate(
      # Nobody went above 2560, no unbounded upper bound problem
      logtitre_point = if_else(logtitre < log(20), NA_real_, logtitre),
      logtitre_low = if_else(logtitre < log(20), -1e6, logtitre - 0.01),
      logtitre_high = logtitre + 0.01
    )
}

# Script ======================================================================

suellen_data <- read_excel(file.path(data_raw_dir, "suellen.xlsx")) %>%
  rename(id = Sample, titre = `Final titer`, covid = `COVID-19 status`) %>%
  filter(str_detect(id, "VIDRL-")) %>%
  mutate(
    inf = if_else(covid == "Pos", 1L, 0L),
    logtitre = log(titre),
  ) %>%
  censor_titres()

write_csv(suellen_data, file.path(data_dir, "suellen.csv"))
