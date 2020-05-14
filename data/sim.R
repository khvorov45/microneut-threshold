# Simulate data
#
# We know the sample's true infection status and take a measurement.
# We are looking for a threshold that can be used for infection
# status determination

library(tidyverse)

# Directories used
data_dir <- here::here("data")

# Functions ===================================================================

sim <- function(n = 200, inf_prop = 0.5,
                mu0 = 0, sd = 2, beta_inf = 3) {
  if (length(mu0) > 1L) {
    logtitres_inf <- mvtnorm::rmvnorm(n * inf_prop, mu0 + beta_inf, sd)
    logtitres_uninf <- mvtnorm::rmvnorm(n * (1 - inf_prop), mu0, sd)
  } else {
    logtitres_inf <- rnorm(n * inf_prop, mu0 + beta_inf, sd) %>%
      as.matrix(ncol = 1)
    logtitres_uninf <- rnorm(n * (1 - inf_prop), mu0, sd) %>%
      as.matrix(ncol = 1)
  }
  colnames(logtitres_inf) <- paste0("logtitre", 1:ncol(logtitres_inf))
  colnames(logtitres_uninf) <- paste0("logtitre", 1:ncol(logtitres_uninf))
  logtitres_inf <- as_tibble(logtitres_inf) %>% mutate(inf = 1L)
  logtitres_uninf <- as_tibble(logtitres_uninf) %>% mutate(inf = 0L)
  bind_rows(logtitres_inf, logtitres_uninf)
}

# Script ======================================================================

sim_data <- sim(
  n = 1e4,
  mu0 = c(2, 2),
  sd = matrix(c(1, 0.2, 0.2, 1), ncol = 2),
  beta_inf = c(2.5, 2.5)
)

write_csv(sim_data, file.path(data_dir, "sim.csv"))
