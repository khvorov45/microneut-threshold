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

censor_titres <- function(data) {
  data %>%
    mutate(
      logtitre_point = if_else(
        logtitre < log(20) | logtitre > log(2560), NA_real_, logtitre
      ),
      logtitre_low = if_else(logtitre < log(20), -1e6, logtitre - 0.01),
      logtitre_high = if_else(logtitre > log(2560), 1e6, logtitre + 0.01)
    )
}

# Script ======================================================================

sim_data <- sim(n = 5e3, mu0 = 1, mu1 = 4, sd0 = 1, sd1 = 1) %>%
  censor_titres()

write_csv(sim_data, file.path(data_dir, "sim.csv"))
