# Extract Suellen data

library(tidyverse)
library(readxl)
library(here)

# Directories used
data_raw_dir <- here("data-raw")
data_dir <- here("data")

# Functions ===================================================================

# Script ======================================================================

suellen_data <- read_excel(file.path(data_raw_dir, "suellen.xlsx")) %>%
  rename(id = Sample, titre = `Final titer`, covid = `COVID-19 status`) %>%
  mutate(
    covid = if_else(covid == "Pos", 1L, 0L),
    logtitre = log(titre)
  )

write_csv(suellen_data, file.path(data_dir, "suellen.csv"))
