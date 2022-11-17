#pk_example <- readr::read_csv("data-raw/pk_example.csv")
set.seed(12345)
pk_example <- tibble(
  ID = rep(1:20, each = 27),
  TIME = rep(c(
    0, 24, 48, 72, 96, 120,
    120, 121, 122, 123, 124, 126, 132, 144, # pk day 1
    144,
    2160,
    2160, 2161, 2162, 2163, 2164, 2166, 2172, 2184, # pk day 2
    2184,
    3143, 3863 # sparse samples
    ), 20),
  AMT = rep(c(
    rep(50, 6),
    rep(0, 8), # pk day 1
    50,
    50,
    rep(0, 8), # pk day 2
    50,
    0, 0 # sparse samples
  ), 20),
  DV = rep(c(
    0, 0, 0, 0, 0, 0,
    1, 9, 10, 9, 8, 6, 4, 1, # pk day 1
    0,
    0,
    1, 9, 10, 9, 8, 6, 4, 1, # pk day 2
    0,
    1, 1 # sparse samples
    ), 20),
  EVID = rep(c(
    4, rep(1, 5),
    rep(0, 8), # pk day 1
    1,
    1,
    rep(0, 8), # pk day 2
    1,
    0, 0  # sparse samples
    ), 20),
  ADDL = rep(c(
    rep(0, 6),
    rep(0, 8), # pk day 1
    83,
    0,
    rep(0, 8), # pk day 2
    70,
    0, 0  # sparse samples
    ), 20),
  II = rep(c(
    rep(0, 6),
    rep(0, 8), # pk day 1
    24,
    0,
    rep(0, 8), # pk day 2
    24,
    0, 0  # sparse samples
  ), 20)
) %>%
  dplyr::mutate(
    TIME = if_else(TIME != 0, jitter(TIME, amount = 0.5), TIME),
    DV = log(DV),
    DV = jitter(DV, factor=10),
    DV = exp(DV),
    DV = if_else(EVID == 0, DV, 0),
  )

set.seed(NULL)

usethis::use_data(pk_example, overwrite = TRUE)
