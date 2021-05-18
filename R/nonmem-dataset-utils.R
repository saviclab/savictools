

add_occasion <- function(data, days_between) {

  data$OCC <- 0
  result <- dplyr::group_by(data, ID)
  result <- dplyr::group_modify(result, ~{
    occ <- 1
    time <- 0
    first <- TRUE
    for (row in 1:nrow(.x)) {
      if (.x[row, "TIME"] - time >= days_between * 24) {
        if (.x[row, "EVID"] == 0) {
          if (!first) {
            occ <- occ + 1
          }
          first <- FALSE
          time <- .x[row, "TIME"]
        }
      }
      .x[row, "OCC"] <- occ
    }
    .x
  })
  result
}
