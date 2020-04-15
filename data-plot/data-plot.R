# Plots the data

library(tidyverse)
library(here)

# Directories used
data_dir <- here("data")
data_plot_dir <- here("data-plot")

# Functions ===================================================================

read_sim <- function() {
  read_csv(
    file.path(data_dir, "sim.csv"),
    col_types = cols(inf = col_integer())
  )
}

plot_histograms <- function(sim_data) {
  sim_data %>%
    ggplot(aes(logtitre, col = as.factor(inf))) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      legend.position = "bottom",
      legend.box.spacing = unit(0, "null")
    ) +
    scale_color_discrete("Infected", labels = c("1" = "Yes", "0" = "No")) +
    scale_x_continuous("Log titre") +
    scale_y_continuous("Count") +
    stat_bin(
      geom = "step", binwidth = 0.1, position = "identity"
    )
}

save_plot <- function(plot, name) {
  ggdark::ggsave_dark(
    file.path(data_plot_dir, glue::glue("{name}.pdf")),
    plot,
    width = 12, height = 8, units = "cm"
  )
}

# Script ======================================================================

sim_data <- read_sim()

hist_log <- plot_histograms(sim_data)

save_plot(hist_log, "sim-hist-log")
