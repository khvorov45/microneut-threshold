read_data <- function(name) {
  read_csv(
    file.path(here::here("data"), glue::glue("{name}.csv")),
    col_types = cols(inf = col_integer(), titre = col_integer())
  )
}
