
# TODO: print QC plot by default
#' @title sparse_pk
#' @importFrom rlang .data
#'
#' @export

sparse_pk <- function(data,
                      max_cluster = 2,
                      max_distance = 24,
                      col_name = "SPARSE",
                      plot = TRUE,
                      plot_only = FALSE,
                      nrow = 4,
                      ncol = 4,
                      page = 1) {
  if (!plot_only) {
    data <- data %>%
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
        max_neighbors <- max_cluster - 1
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
  if (plot) {
    p <- data %>%
          dplyr::filter(.data$EVID == 0) %>%
          ggplot2::ggplot(ggplot2::aes(x = TIME, y = DV, group =
                                         .data[[col_name]], color =
                                         factor(.data[[col_name]]))) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::labs(color = col_name) +
          ggforce::facet_wrap_paginate("ID", nrow = nrow, ncol = ncol,
                                       scales = "free_x", page = page)
    print(p)

  }
  invisible(data)
}
