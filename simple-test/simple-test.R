cat("Simple threshold testing")

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(here)
})

# Directories used
data_dir <- here::here("data")
simple_test_dir <- here::here("simple-test")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

get_low_bound <- function(x, n) {
  PropCIs::exactci(x, n, 0.95)$conf.int[[1]]
}

get_high_bound <- function(x, n) {
  PropCIs::exactci(x, n, 0.95)$conf.int[[2]]
}

count_with_miss <- function(data) {
  miss <- tribble(
    ~inf, ~pos, ~n,
    0L, 0L, 0L,
    0L, 1L, 0L,
    1L, 0L, 0L,
    1L, 1L, 0L,
  )
  res <- count(data, inf, pos)
  miss %>%
    filter(!paste0(inf, pos) %in% paste0(res$inf, res$pos)) %>%
    bind_rows(res)
}

simple_test <- function(threshold, data) {
  data %>%
    filter(!is.na(inf), !is.na(titre)) %>%
    mutate(pos = as.integer(titre >= threshold)) %>%
    count_with_miss() %>%
    group_by(inf) %>%
    summarise(
      threshold = threshold,
      nsam = sum(n),
      test_char = if_else(first(inf) == 0L, "Specificity", "Sensitivity"),
      test_char_val = if_else(
        first(inf) == 0L, n[pos == 0L] / sum(n), n[pos == 1L] / sum(n)
      ),
      test_char_val_low = if_else(
        first(inf) == 0L, get_low_bound(n[pos == 0L], sum(n)),
        get_low_bound(n[pos == 1L], sum(n))
      ),
      test_char_val_high = if_else(
        first(inf) == 0L, get_high_bound(n[pos == 0L], sum(n)),
        get_high_bound(n[pos == 1L], sum(n))
      ),
      .groups = "drop"
    ) %>%
    select(-inf)
}

save_csv <- function(data, name) {
  write_csv(data, file.path(simple_test_dir, glue::glue("result-{name}.csv")))
}

save_table <- function(table_tex, table_name) {
  write(table_tex, file.path(simple_test_dir, glue::glue("{table_name}.tex")))
}

# Script ======================================================================

datasets <- list(
  suellen = read_data("suellen"),
  kanta = read_data("kanta")
)
datasets$combined <- bind_rows(
  datasets$kanta, select(datasets$suellen, id, inf, titre, logtitre)
)

thresholds <- seq(20L, 50L, 5L)

# Multiple thresholds on all datasets

results <- map(datasets, ~ map_dfr(thresholds, simple_test, .x))

iwalk(results, save_csv)

# Make a table out of it

bind_rows(results, .id = "Dataset") %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  group_by(Dataset) %>%
  mutate(
    Dataset = tools::toTitleCase(Dataset),
    Infected = unique(nsam[test_char == "Sensitivity"]),
    Uninfected = unique(nsam[test_char == "Specificity"]),
    test_char_est = glue::glue(
      "{test_char_val} ({test_char_val_low}, {test_char_val_high})"
    )
  ) %>%
  ungroup() %>%
  select(
    Dataset, test_char, Uninfected, Infected,
    Threshold = threshold,
    test_char_est
  ) %>%
  pivot_wider(names_from = "test_char", values_from = "test_char_est") %>%
  kable(
    format = "latex",
    caption = "Estimates (95\\% CI) of sensitivity and specificity for the
    two datasets separately as well as for the combined one.",
    label = "result-all",
    escape = FALSE,
    booktabs = TRUE,
    align = "lccc"
  ) %>%
  kable_styling(
    latex_options = "striped"
  ) %>%
  collapse_rows(c(1, 2, 3), valign = "top", latex_hline = "major") %>%
  save_table("result-all")

# Multiple thresholds on different symtom onset-based subsets of the Kanta
# dataset

duration_thresholds <- seq(0L, 12L, 2L)

kanta_split <- map_dfr(duration_thresholds, function(duration_threshold) {
  datasets$kanta %>%
    filter(!is.na(titre), !is.na(inf)) %>%
    # We only have symtom duration for the infected
    filter(symptom_duration >= duration_threshold | inf == 0L) %>%
    mutate(duration_threshold = duration_threshold)
})

results_dur <- kanta_split %>%
  group_by(duration_threshold) %>%
  group_modify(~ map_dfr(seq(20L, 35L, 5L), simple_test, .x)) %>%
  ungroup()

results_dur %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  group_by(duration_threshold) %>%
  mutate(
    Infected = unique(nsam[test_char == "Sensitivity"]),
    Uninfected = paste0(
      unique(nsam[test_char == "Specificity"]),
      "EXTRA",
      rnorm(1),
      "EXTRAEND"
    ),
    test_char_est = glue::glue(
      "{test_char_val} ({test_char_val_low}, {test_char_val_high})"
    )
  ) %>%
  ungroup() %>%
  select(
    `Minimum duration` = duration_threshold, Uninfected, Infected, test_char,
    Threshold = threshold, test_char_est
  ) %>%
  pivot_wider(names_from = "test_char", values_from = "test_char_est") %>%
  kable(
    format = "latex",
    caption = "Estimates (95\\% CI) of sensitivity and specificity for the
    new dataset with different symtom duration thresholds. Only sensitivity
    is affected because symptom duration only applies to those infected in
    the given data.  Minimum duration
    of 3 means that subjects whose symptoms lasted for less than 3 days before
    the antibody test were excluded.",
    label = "result-dur",
    escape = FALSE,
    booktabs = TRUE,
    align = "lccc"
  ) %>%
  kable_styling(
    latex_options = "striped"
  ) %>%
  collapse_rows(
    c(1, 2, 3),
    valign = "top", latex_hline = "major"
  ) %>%
  str_replace_all("EXTRA.*EXTRAEND", "") %>%
  save_table("result-dur")
