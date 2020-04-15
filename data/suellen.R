# Extract Suellen data

library(tidyverse)
library(readxl)
library(here)

# Directories used
data_raw_dir <- here("data-raw")
data_dir <- here("data")

# Functions ===================================================================

# Script ======================================================================

suellen <- read_excel(file.path(data_raw_dir, "suellen.xlsx")) %>%
  rename(id = Sample, titre = `Final titer`, covid = `COVID-19 status`) %>%
  mutate(covid = if_else(covid == "Pos", 1L, 0L))

write_csv(suellen, file.path(data_dir, "suellen.csv"))
