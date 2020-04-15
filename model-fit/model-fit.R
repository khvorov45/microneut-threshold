# Fitting the model to the data

library(tidyverse)
library(here)

# Directories used
data_dir <- "data"
model_fit_dir <- "model-fit"

# Functions ===================================================================

read_sim <- function() {
  read_csv(
    file.path(data_dir, "sim.csv"),
    col_types = cols(inf = col_integer())
  )
}

read_suellen <- function() {
  read_csv(
    file.path(data_dir, "suellen.csv"),
    col_types = cols(covid = col_integer())
  )
}

# Script ======================================================================

sim_data <- read_sim()
suellen_data <- read_suellen()
