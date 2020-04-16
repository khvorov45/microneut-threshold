# Plots of resulting threshold test characteristics

library(tidyverse)
library(here)

# Directories used
model_fit_dir <- here("model-fit")
testchar_plot_dir <- here("testchar-plot")

# Functions ===================================================================

read_fit <- function(name) {
  read_csv(
    file.path(model_fit_dir, glue::glue("{name}.csv")),
    col_types = cols()
  )
}

plot_testchar <- function(fit_res) {
  fit_res %>%
    ggplot(aes(threshold, med)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~char, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_x_continuous(
      "Threshold titre",
      breaks = log(c(seq(20, 45, 5), seq(50, 80, 10))),
      labels = c(seq(20, 45, 5), seq(50, 80, 10))
    ) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5) +
    geom_line()
}

save_plot <- function(pl, name) {
  ggdark::ggsave_dark(
    file.path(testchar_plot_dir, glue::glue("{name}-testchar.pdf")),
    pl,
    width = 10, height = 10, units = "cm"
  )
}

# Script ======================================================================

dat <- list(
  "sim" = read_fit("sim-lin"),
  "suellen" = read_fit("suellen-lin")
)

pls <- map(dat, plot_testchar)

iwalk(pls, save_plot)
