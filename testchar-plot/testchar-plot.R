# Plots of resulting threshold test characteristics

library(tidyverse)

# Directories used
model_fit_dir <- here::here("model-fit")
testchar_plot_dir <- here::here("testchar-plot")
resample_dir <- here::here("resample")

# Functions ===================================================================

read_fit <- function(name) {
  read_csv(
    file.path(model_fit_dir, glue::glue("{name}.csv")),
    col_types = cols()
  )
}

read_resample <- function(name) {
  read_csv(
    file.path(resample_dir, glue::glue("summ-{name}.csv")),
    col_types = cols()
  )
}

plot_testchar <- function(fit_res) {
  fit_res %>%
    ggplot(aes(logthreshold, test_char)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "null")
    ) +
    facet_wrap(~char, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_x_continuous(
      "Threshold titre",
      breaks = log(c(seq(20, 45, 5), seq(50, 80, 10))),
      labels = c(seq(20, 45, 5), seq(50, 80, 10))
    ) +
    scale_y_continuous(
      labels = scales::percent_format(1), breaks = seq(0, 1, 0.1)
    ) +
    geom_ribbon(aes(ymin = test_char_low, ymax = test_char_high), alpha = 0.5) +
    geom_line()
}

plot_testchar2 <- function(fit_res) {
  fit_res %>%
    ggplot(aes(logthreshold_titre, test_char2)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "null")
    ) +
    facet_grid(
      char ~ threshold_elisa,
      scales = "free_y"
    ) +
    scale_x_continuous(
      "Threshold titre",
      breaks = log(c(seq(20, 45, 5), seq(50, 80, 10))),
      labels = c(seq(20, 45, 5), seq(50, 80, 10))
    ) +
    scale_y_continuous(
      "Impact",
      labels = scales::percent_format(1), breaks = seq(0, 1, 0.1)
    ) +
    geom_ribbon(
      aes(ymin = test_char2_low, ymax = test_char2_high),
      alpha = 0.5
    ) +
    geom_line()
}

save_plot <- function(pl, name, width = 10, height = 10) {
  ggdark::ggsave_dark(
    file.path(testchar_plot_dir, glue::glue("{name}-testchar.pdf")),
    pl,
    width = width, height = height, units = "cm"
  )
}

# Script ======================================================================

dat <- list(
  "sim" = read_fit("sim-lin"),
  "suellen" = read_fit("suellen-lin"),
  "sim-resample" = read_resample("sim"),
  "suellen-resample" = read_resample("suellen")
)

dat2 <- list(
  "sim2" = read_resample("sim2"),
  "suellen-iga" = read_resample("suellen-iga"),
  "suellen-igg" = read_resample("suellen-igg")
)

pls <- map(dat, plot_testchar)
pls2 <- map(dat2, plot_testchar2)

iwalk(pls, save_plot)
iwalk(pls2, save_plot, 30, 10)
