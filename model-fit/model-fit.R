# Fitting the model to the data

library(tidyverse)
library(here)
library(rjags)

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

make_model <- function(data, status_name) {
  data <- rename(data, "status" = local(status_name))
  list(
    filepath = file.path(model_fit_dir, "model.jags"),
    data = data,
    pars = c("beta_0", "beta_1", "sigma"),
    inits = list(
      list(sigma = 1, beta_0 = 1, beta_1 = 3),
      list(sigma = 2, beta_0 = 0, beta_1 = 0),
      list(sigma = 3, beta_0 = 2, beta_1 = 6)
    )
  )
}

fit_jags <- function(data) {

}

# Script ======================================================================

sim_data <- read_sim()
suellen_data <- read_suellen()

sim_model <- make_model(sim_data, "inf")
