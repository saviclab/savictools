#' @title Calculate expected weight for under-5 children.
#'
#' @description Calculate expected weight in under-5 children. Expected weight
#' is defined as the weight which would result in a weight-for-age z-score (or
#' BMI-for-age z-score, if over 10 years of age) of 0 based on age and sex (and
#' height, if over 10 years of age).
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
                  units = c("months", "years", "weeks"),
                  age = grep("age",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  ),
                  sex = grep("sex",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  ),
                  height = grep("height|ht",
                    colnames(data),
                    ignore.case = T,
                    value = T
                  )) {
  units <- match.arg(units)
  age <- ifelse(identical(age, character(0)), NA, age)
  sex <- ifelse(identical(sex, character(0)), NA, sex)
  height <- ifelse(identical(height, character(0)), NA, height)


  age_vec <- dplyr::pull(data, {{ age }})

  if (units == "years") {
    age_vec <- age_vec * 12
  } else if (units == "weeks") {
    age_vec <- age_vec / 4.345
  }


  calc_expwt <- function(age, sex, height) {
    age_days <- round(age * 30.4375)
    if (age < 120) {
      if (age_days <= 1856) {
        savictools:::weianthro[which(savictools:::weianthro$sex == sex &
          savictools:::weianthro$age == age_days), "m"]
      } else {
        savictools:::wfawho2007[which(savictools:::wfawho2007$sex == sex &
          savictools:::wfawho2007$age == age), "m"]
      }
    } else {
      bmi <- savictools:::bfawho2007[which(savictools:::bfawho2007$sex == sex &
        savictools:::bfawho2007$age == age), ]$m
      bmi * (height / 100)^2
    }
  }
  data %>% dplyr::mutate(EXPWT = mapply(calc_expwt, !!rlang::sym(age), !!rlang::sym(sex), !!rlang::sym(height)))
}
