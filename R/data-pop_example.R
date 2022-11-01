#' Population dataset
#'
#' A dataset of demographics, with one row per individual. Used to test
#' savictools functions like `zscores()`, `cohort()`, and `expwt()`.
#'
#' @format A tibble with 1000 rows and 5 variables:
#' \describe{
#' \item{ID}{Patient ID}
#' \item{SEX}{Sex. 1 = male, 2 = female}
#' \item{AGE}{Age in months}
#' \item{WT}{Weight in kg}
#' \item{HT}{Event ID. 0 = observation, 1 = dose, 4 = reset-and-dose}
#' }
#' @examples
#' pop_example
"pop_example"
