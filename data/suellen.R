# Extract Suellen data

library(tidyverse)
library(readxl)
library(here)

# Directories used
data_raw_dir <- here("data-raw")
data_dir <- here("data")

# Functions ===================================================================

read_eia_sheet <- function(sheetname) {
  read_excel(
    file.path(data_raw_dir, "eia.xlsx"),
    skip = 7,
    col_names = c(
      # index (igg/iga) = od / co
      "od_igg", "co_igg", "igg", "interpret_igg", "well",
      "od_iga", "co_iga", "iga", "interpret_iga",
      "id", "titre"
    ),
    sheet = sheetname
  ) %>%
    select(id, igg, iga) %>%
    mutate(
      id = paste0("VIDRL-310320-", str_pad(id, 2, pad = "0"))
    )
}

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

eia_data <- map_dfr(c("Sheet1", "Sheet2"), read_eia_sheet)

all_data <- full_join(suellen_data, eia_data, by = "id")

write_csv(all_data, file.path(data_dir, "suellen.csv"))
