#' @title Calculate expected weight based on age and sex for under-5 children.
#'
#' @description Calculate expected weight based on age and sex for under-5 children.
#'
#' @param data A dataframe with AGE and SEX columns.
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
