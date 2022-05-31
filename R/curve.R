#' @title
#' Detect individual PK curves
#'
#' @author Sandy Floren
#'
#' @description
#' `curve()` assigns a unique number to each PK curve in a dataset.
#'
#' @details
#' A PK curve is defined as any sequence of consecutive 0s in the EVID column of
#' length greater than or equal to 1, corresponding to the same ID.
#'
#' `curve()` is primarily used by `pk_plot()` in order to draw separate lines
#' for distinct PK curves belonging to the same individual.
#'
#' @param data A dataframe with ID, TIME, and EVID columns.
#'
#' @export

curve <- function(data) {
  data <- data %>%
    dplyr::arrange(ID, TIME, EVID)
  l <- nrow(data)
  id <- data$ID
  evid <- data$EVID
  curves <- rep(0, l)
  in_curve <- FALSE
  curve_num <- 0
  current_id <- id[1]
  for (i in 1:l) {
    if (evid[i] != 0 | id[i] != current_id) {
      in_curve <- FALSE
      current_id <- id[i]
      next()
    }
    if (!in_curve) {
      in_curve <- TRUE
      curve_num <- curve_num + 1
    }
    curves[i] <- curve_num

  }
  data$CURVE <- curves
  data
}
