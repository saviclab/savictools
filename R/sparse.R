
# TODO: print QC plot by default
# TODO: docs
#' @title sparse
#'
#' @author Sandy Floren
#' @export

sparse <- function(data,
                      min_cluster = 3,
                      max_distance = 24,
                      plot = FALSE,
                      plot_only = FALSE,
                      nrow = 4,
                      ncol = 4,
                      page = 1) {
  if (!plot_only) {
    data <- data %>%
      dplyr::group_by(ID) %>%
      dplyr::group_modify(~{

        .x$SPARSE <- 1
        len <- nrow(.x)
        #distances <- array(, c(len, len))
        distances <- array(FALSE, c(len, len))
        for (i in 1:len) {

          if (.x[i, "EVID"] == 1) {
            next()
          }
          for (j in 1:len) {

            if (abs(.x[j, "TIME"] - .x[i, "TIME"]) > max_distance) {
              if (.x[j, "TIME"] - .x[i, "TIME"] > max_distance) {
                break()
              }
              next()
            }
            if (.x[j, "EVID"] == 1) {
              next()
            }
            #distances[i, j] <- as.numeric(.x[j, "TIME"] - .x[i, "TIME"])
            distances[i, j] <- TRUE

          }
        }
        clusters <- NULL
        #max_neighbors <- max_cluster - 1
        for (row in 1:len) {
          #x <- which(!is.na(distances[row, ]))
          x <- which(distances[row, ])
          #if (length(x) <= min_cluster) {
          if (length(x) < min_cluster) {
            next()
          }
          if (is.null(clusters)) {
            clusters <- x
          } else {
            clusters <- union(clusters, x)
          }
        }


        .x[clusters, "SPARSE"] <- 0
        .x



      }) %>%
      dplyr::ungroup()
  }
  if (plot) {
    p <- data %>%
          dplyr::filter(EVID == 0) %>%
          ggplot2::ggplot(ggplot2::aes(x = TIME, y = DV, group = ID)) +
          ggplot2::geom_line(color = "black", size = 0.3) +
          ggplot2::geom_point(ggplot2::aes(color = as.factor(SPARSE))) +
          ggplot2::labs(color = "SPARSE") +
      ggplot2::scale_color_manual(values = c("red3", "darkblue"))


    #     ggforce::facet_wrap_paginate("ID", nrow = nrow, ncol = ncol, scales = "free_x", page = page)
    p

  } else {
    data
  }
}
