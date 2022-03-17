#' @title Calculate expected weight based on age and sex for under-5 children.
#' @author Alexander Floren
#'
#' @description Calculate expected weight based on age and sex for under-5 children.
#'
#' @param age Age.
#' @param sex Sex. 1 = male, 2 = female.
#' @param units Units for age. Default is "months".
#'
#' @export
exp_weight <- function(age, sex, units = c("months", "years", "weeks")) {
  units <- match.arg(units)
  # convert units to months, if necessary
  if (units == "years") {
    data$AGE <- data$AGE * 12
  }
  else if (units == "weeks") {
    data$AGE <- data$AGE / 4.345
  }

  mapply(function(age, sex) {if (sex == 1) {expected_weight_table[age + 1, "EXP_WEIGHT_MALE"]}
    else{expected_weight_table[age + 1, "EXP_WEIGHT_FEMALE"]}}, age, sex)
}
