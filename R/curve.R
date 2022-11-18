#' @title
#' Detect individual PK curves
#'
#' @author Sandy Floren
#'
#' @description
#' `curve()` assigns a unique number to each PK curve in a data set.
#'
#' @details
#' A PK curve is defined as any sequence of consecutive 0s in the EVID column of
#' length greater than or equal to 1, corresponding to the same ID.
#'
#' `curve()` is primarily used by `pk_plot()` in order to draw separate lines
#' for distinct PK curves belonging to the same individual.
#' @returns
#'  A [tibble::tibble()] with an appended column, CURVE, containing a unique number
#'  for each group of consecutive observations which are not separated by a non-observation
#'  data record.
#'
#' @param data A data frame or data frame extension with ID, TIME, and EVID columns.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export

curve <- function(data) {
  data <- data %>%
    dplyr::arrange(.data$ID, .data$TIME, dplyr::desc(.data$EVID))
  if (!exists("TAD", data)) {
    data <- tad(data)
  }
  l <- nrow(data)
  id <- data$ID
  evid <- data$EVID
  curves <- rep(0, l)
  tad <- data$TAD

  in_curve <- FALSE
  curve_num <- 0
  current_id <- id[1]
  current_tad <- tad[1]
  for (i in 1:l) {
    if (evid[i] != 0 | id[i] != current_id | tad[i] < current_tad) {
      in_curve <- FALSE
      current_id <- id[i]
      current_tad <- tad[i]
      next()
    }
    if (!in_curve) {
      in_curve <- TRUE
      curve_num <- curve_num + 1
    }
    current_tad <- tad[i]
    curves[i] <- curve_num

  }
  data$CURVE <- curves
  data
}
