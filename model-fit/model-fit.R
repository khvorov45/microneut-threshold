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

fit_jags <- function(model, n_adapt, n_iter) {
  nchains <- length(model$inits)
  cl <- parallel::makeCluster(min(nchains, parallel::detectCores()))
  on.exit(parallel::stopCluster(cl))
  dclone::jags.parfit(
    cl = cl,
    data = c(model$data, n = nrow(model$data)),
    params = model$pars,
    model = model$filepath,
    inits = model$inits,
    n.chains = nchains,
    n.adapt = n_adapt,
    n.update = 0, # Will cut off manually
    n.iter = n_iter
  )
}

# Convert one chain's output to table (JAGS only)
tidy_mcmc_chain <- function(mcmcchain, nchain) {
  as_tibble(as.data.frame(mcmcchain)) %>%
    mutate(nchain = nchain, niter = row_number())
}

# Convert multiple chains output to table (JAGS only)
tidy_mcmc <- function(mcmcout) {
  imap_dfr(mcmcout, tidy_mcmc_chain)
}

save_fit <- function(tidyout, model_name) {
  write_csv(
    tidyout, file.path(model_fit_dir, glue::glue("fit-{model_name}.csv"))
  )
}

# Script ======================================================================

sim_data <- read_sim()
suellen_data <- read_suellen()

sim_model <- make_model(sim_data, "inf")
suellen_model <- make_model(suellen_data, "covid")

sim_fit <- fit_jags(sim_model, 1e3, 1e4)
suellen_fit <- fit_jags(suellen_model, 1e3, 1e4)

sim_fit_tidy <- tidy_mcmc(sim_fit)
suellen_fit_tidy <- tidy_mcmc(suellen_fit)

save_fit(sim_fit_tidy, "sim")
save_fit(suellen_fit_tidy, "suellen")
