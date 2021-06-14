sparse <- function(data,
                   max_neighbors = 1,
                   max_distance = 24,
                   col_name = "SPARSE") {
  data %>%
    dplyr::group_by(ID) %>%
    dplyr::group_modify(~{

      .x[, col_name] <- 1
      len <- nrow(.x)
      distances <- array(, c(len, len))
      for (i in 1:len) {

        if (.x[i, "EVID"] == 1) {
          next()
        }
        for (j in 1:len) {

          if (.x[j, "EVID"] == 1 | i == j) {
            next()
          }
          if (abs(.x[j, "TIME"] - .x[i, "TIME"]) > max_distance) {
            if (.x[j, "TIME"] - .x[i, "TIME"] > max_distance) {
              break()
            }
            next()
          }
          distances[i, j] <- as.numeric(.x[j, "TIME"] - .x[i, "TIME"])

        }
      }
      clusters <- NULL
      for (row in 1:len) {
        x <- which(!is.na(distances[row, ]))
        if (length(x) <= max_neighbors) {
          next()
        }
        if (is.null(clusters)) {
          clusters <- x
        } else {
          clusters <- union(clusters, x)
        }
      }


      .x[clusters, col_name] <- 0
      .x



    }) %>%
    dplyr::ungroup()
}
