# Plots the data

library(tidyverse)
library(here)

# Directories used
data_dir <- here("data")
data_plot_dir <- here("data-plot")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

plot_histograms <- function(data, xname = "logtitre1",
                            xlab = "Titre",
                            xbreaks = log(10 * 2^(0:8)),
                            xlabs = 10 * 2^(0:8),
                            yname = "inf", inf_lab = "Infected") {
  data %>%
    filter(!is.na(!!rlang::sym(xname)), !is.na(!!rlang::sym(yname))) %>%
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
    scale_x_continuous(xlab, breaks = xbreaks, labels = xlabs) +
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

sim_data <- read_data("sim")
suellen_data <- read_data("suellen")

plots <- list(
  "sim-hist-cont" = plot_histograms(sim_data),
  "suellen-hist" = plot_histograms(
    suellen_data,
    inf_lab = "Covid", xname = "logtitre"
  ),
  "suellen-hist-igg" = plot_histograms(
    suellen_data,
    inf_lab = "Covid", xname = "igg", xlab = "IgG index", xbreaks = waiver(),
    xlabs = waiver()
  ),
  "suellen-hist-iga" = plot_histograms(
    suellen_data,
    inf_lab = "Covid", xname = "iga", xlab = "IgA index", xbreaks = waiver(),
    xlabs = waiver()
  )
)

iwalk(plots, save_plot)
