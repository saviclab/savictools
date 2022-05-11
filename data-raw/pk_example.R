pk_example <- tibble::as_tibble(readr::read_csv("data-raw/pk_example.csv"))
usethis::use_data(pk_example, overwrite = TRUE)
