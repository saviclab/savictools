pop_example <- tibble::as_tibble(readr::read_csv("data-raw/pop_example.csv"))
usethis::use_data(pop_example, overwrite = TRUE)

