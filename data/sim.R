# Simulate data
#
# We know the sample's true infection status and take a measurement.
# We are looking for a threshold that can be used for infection
# status determination

library(tidyverse)
library(extraDistr)
library(here)

# Directories used
data_dir <- here("data")

# Functions ===================================================================

sim <- function(n = 200, inf_prop = 0.5,
                mu0 = 0, sd0 = 2,
                mu1 = 3, sd1 = 2) {
  tibble(
    inf = as.integer(rbern(n, inf_prop)),
    logtitre = rnorm(n, ifelse(inf, mu1, mu0), ifelse(inf, sd1, sd0))
  )
}

# Script ======================================================================

sim_data <- sim(n = 1e5)

write_csv(sim_data, file.path(data_dir, "sim.csv"))
