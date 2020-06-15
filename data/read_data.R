read_data <- function(name) {
  col_types <- list(
    "sim" = cols(inf = col_integer()),
    "suellen" = cols(inf = col_integer(), titre = col_integer()),
    "kanta" = cols(
      inf = col_integer(), titre = col_integer(),
      symptom_duration = col_integer()
    )
  )
  read_csv(
    file.path(here::here("data"), glue::glue("{name}.csv")),
    col_types = col_types[[name]]
  )
}
