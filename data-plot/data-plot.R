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

read_suellen <- function() {
  read_csv(
    file.path(data_dir, "suellen.csv"),
    col_types = cols(covid = col_integer())
  )
}

plot_histograms <- function(data, xname = "logtitre",
                            yname = "inf", inf_lab = "Infected") {
  data %>%
    ggplot(
      aes(!!rlang::sym(xname),
        col = as.factor(!!rlang::sym(yname)),
        linetype = as.factor(!!rlang::sym(yname))
      )
    ) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      legend.position = "bottom",
      legend.box.spacing = unit(0, "null"),
      axis.text.x = element_text(angle = 90)
    ) +
    scale_color_discrete(inf_lab, labels = c("1" = "Yes", "0" = "No")) +
    scale_linetype_discrete(inf_lab, labels = c("1" = "Yes", "0" = "No")) +
    scale_x_continuous(
      "Titre",
      breaks = log(10 * 2^(0:8)), labels = 10 * 2^(0:8)
    ) +
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
suellen_data <- read_suellen()

sim_hist_cont <- plot_histograms(sim_data)
suellen_hist_log <- plot_histograms(
  suellen_data,
  yname = "covid", inf_lab = "Covid"
)

save_plot(sim_hist_cont, "sim-hist-cont")
save_plot(suellen_hist_log, "suellen-hist")
