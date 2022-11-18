#' PK data set
#'
#' This is a NONMEM-formatted data set intended for testing savictools functions.
#'
#' @format A tibble with 540 rows and 7 variables:
#' \describe{
#' \item{ID}{Patient ID}
#' \item{TIME}{Event time}
#' \item{AMT}{Dose amount}
#' \item{DV}{Dependent value (concentration)}
#' \item{EVID}{Event ID. 0 = observation, 1 = dose, 4 = reset-and-dose}
#' \item{ADDL}{number of additional doses}
#' \item{II}{Inter-dose interval}
#' }
#' @examples
#' pk_example
"pk_example"
