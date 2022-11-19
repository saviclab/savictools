#' @title Calculate expected weight for under-5 children.
#'
#' @description Calculate expected weight in under-5 children. Expected weight
#' is defined as that weight which would result in a weight-for-age z-score of 0
#' based on age and sex.
#'
#' @returns A [tibble::tibble()] with an appended column, EXPWT, containing the expected
#'  weight,
#'
#' @param data A data frame or data frame extension with AGE and SEX columns.
#' @param units Units for age. Default is "months".
#'
#' @author Sandy Floren
#' @export
expwt <- function(data, units = c("months", "years", "weeks")) {
  units <- match.arg(units)
  # convert units to months, if necessary
  if (units == "years") {
    age_vec <- data$AGE * 12
  }
  else if (units == "weeks") {
    age_vec <- data$AGE / 4.345
  } else {
    age_vec <- data$AGE
  }

  calc_expwt <- function(age, sex) {
    if (sex == 1) {expected_weight_table[age + 1, "EXP_WEIGHT_MALE"]}
    else if (sex == 2) {expected_weight_table[age + 1, "EXP_WEIGHT_FEMALE"]}
    else {rlang::warn("SEX must be 1 or 2."); NA}}

  data %>%
    dplyr::mutate(EXPWT = mapply(calc_expwt, age_vec, .data$SEX))

}
