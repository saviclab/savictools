#' @title curve
#' @export

curve <- function(data, col_name = "CURVE") {
  l <- nrow(data)
  evid <- data$EVID
  curves <- rep(0, l)
  in_curve <- FALSE
  curve_num <- 0
  for (i in 1:l) {
    if (evid[i] != 0) {
      in_curve <- FALSE
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
