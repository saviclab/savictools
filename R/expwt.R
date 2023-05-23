#' @title Calculate expected weight for under-5 children.
#'
#' @description Calculate expected weight in under-5 children. Expected weight
#' is defined as the weight which would result in a weight-for-age z-score (or
#' BMI-for-age z-score, if over 10 years of age) of 0 based on age and sex (and
#' height, if over 10 years of age).
#'
#' Weight must be in kg, height in cm. Sex must be coded as: 1 = male, 2 = female.
#'
#' @returns A [tibble::tibble()] with an appended column, EXPWT, containing the
#' expected weight,
#'
#' @param data A data frame or data frame extension.
#' @param age Name of the age column.
#' @param sex Name of the sex column.
#' @param height Name of the height column.
#' @param units Units for age. Default is "months".
#'
#' @examples
#' expwt(pop_example)
#'
#' @author Sandy Floren
#' @export

expwt <- function(data,
                  units = c("months", "years", "weeks", "days"),
                  age = grep("age",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  )[1],
                  sex = grep("sex",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  )[1],
                  weight = grep("weight|wt",
                                colnames(data),
                                ignore.case = T,
                                value = T
                  )[1],
                  height = grep("height|ht",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  )[1]) {
  units <- match.arg(units)
  age <- ifelse(identical(age, character(0)), NA, dplyr::pull(data, {{ age }}))
  sex <- ifelse(identical(sex, character(0)), NA, dplyr::pull(data, {{ sex }}))
  weight <- ifelse(identical(weight, character(0)), NA, dplyr::pull(data, {{ weight }}))
  height <- ifelse(identical(height, character(0)), NA, dplyr::pull(data, {{ height }}))

  # convert age to months
  age_months <- age
  if (units == "years") {
    age_months <- age * MONTHS_PER_YEAR
  } else if (units == "weeks") {
    age_months <- age / WEEKS_PER_MONTH
  } else if (units == "days") {
    age_months <- age / DAYS_PER_YEAR
  }
  age_days <- round(age_months * DAYS_PER_MONTH) # must be integer for EXPWT
  age_years <- age_months * MONTHS_PER_YEAR

  data %>% dplyr::mutate(
    EXPWT = mapply(calc_expwt, age_days, sex, height)#,
    #FFM = mapply(calc_ffm, age_years, sex, weight, height),
    #BSA = mapply(calc_bsa, weight, height)
    )
}

DAYS_PER_YEAR <- 365.25
MONTHS_PER_YEAR <- 12
DAYS_PER_WEEK <- 7
DAYS_PER_MONTH <- DAYS_PER_YEAR / MONTHS_PER_YEAR # 30.4375
WEEKS_PER_YEAR <- DAYS_PER_YEAR / DAYS_PER_WEEK # 52.17857
WEEKS_PER_MONTH <- WEEKS_PER_YEAR / MONTHS_PER_YEAR # 4.348214



calc_expwt <- function(age, sex, height) {

  # assume age in days
  age_months <- round(age / DAYS_PER_MONTH)
  # if age < 10 years
  if (age < 10 * DAYS_PER_YEAR) {
    if (age <= 1856) {
      savictools:::weianthro[which(savictools:::weianthro$sex == sex &
                                     savictools:::weianthro$age == age), "m"]
    } else {
      savictools:::wfawho2007[which(savictools:::wfawho2007$sex == sex &
                                      savictools:::wfawho2007$age == age_months), "m"]
    }
  } else {
    bmi <- savictools:::bfawho2007[which(savictools:::bfawho2007$sex == sex &
                                           savictools:::bfawho2007$age == age_months), ]$m
    bmi * (height / 100)^2
  }
}


calc_ffm <- function(age, sex, weight, height) {

  bmi <- weight / ((height / 100)^2)
  # assume age in years
  ffm <- ifelse(sex == 1,
         (0.88 + ((1 - 0.88) / (1 + (age / 13.4)^-12.7))) * ((9270 * weight) / (6680 + 216 * bmi)),
         (1.11 + ((1 - 1.11) / (1 + (age / 7.1)^-1.1))) * ((9270 * weight) / (8780 + 244 * bmi))
         )

  ifelse(age < 3,
         NA,
         ffm)
}

calc_bsa <- function(weight, height) {
  # Du Bois and Du Bois formula
  0.007184 * (height^0.725) * (weight^0.425)
}
