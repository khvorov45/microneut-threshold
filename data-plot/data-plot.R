# Plots the data

library(tidyverse)
library(here)

# Directories used
data_dir <- here("data")
data_plot_dir <- here("data-plot")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

plot_scatter <- function(data,
                         xname = "logtitre1", xlabel = "Titre 1",
                         xbreaks = log(10 * 2^(0:8)), xlabs = 10 * 2^(0:8),
                         yname = "logtitre2", ylabel = "Titre 2",
                         ybreaks = log(10 * 2^(0:8)), ylabs = 10 * 2^(0:8),
                         inf_lab = "Infected", alpha = 0.1) {
  data %>%
    filter(!is.na(!!rlang::sym(xname)), !is.na(!!rlang::sym(yname))) %>%
    ggplot(aes(
      !!rlang::sym(xname), !!rlang::sym(yname),
      col = as.factor(inf)
    )) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      legend.position = "bottom",
      legend.box.spacing = unit(0, "null"),
      axis.text.x = element_text(angle = 90)
    ) +
    scale_color_discrete(inf_lab, labels = c("1" = "Yes", "0" = "No")) +
    scale_linetype_discrete(inf_lab, labels = c("1" = "Yes", "0" = "No")) +
    scale_x_continuous(xlabel, breaks = xbreaks, labels = xlabs) +
    scale_y_continuous(ylabel, breaks = ybreaks, labels = ylabs) +
    geom_point(alpha = alpha, shape = 18) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
}

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
kanta_data <- read_data("kanta")

plots <- list(
  "sim-hist-cont" = plot_histograms(sim_data),
  "sim-scatter" = plot_scatter(sim_data),
  "suellen-hist" = plot_histograms(
    suellen_data,
    inf_lab = "Covid", xname = "logtitre"
  ),
  "kanta-hist" = plot_histograms(
    kanta_data,
    inf_lab = "Covid", xname = "logtitre"
  ),
  "all-real" = plot_histograms(
    bind_rows(kanta_data, select(suellen_data, id, inf, titre, logtitre)),
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
  ),
  "suellen-scatter-igg" = plot_scatter(
    suellen_data,
    xname = "logtitre", yname = "igg", alpha = 1, ybreaks = waiver(),
    ylabs = waiver(), ylabel = "IgG index", xlabel = "Titre"
  ),
  "suellen-scatter-iga" = plot_scatter(
    suellen_data,
    xname = "logtitre", yname = "iga", alpha = 1, ybreaks = waiver(),
    ylabs = waiver(), ylabel = "IgA index", xlabel = "Titre"
  )
)

iwalk(plots, save_plot)
