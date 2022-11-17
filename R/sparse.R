#' @title Detect sparse sampling occasions
#'
#' @description `sparse()` detects differentiates between clusters of consecutive samples
#' and more spread out, or "sparse," samples in a NONMEM-formatted dataset.
#'
#' @details For each ID, `sparse()` iterates through each line of the dataset until
#' it finds an observation. It then checks for "neighbors," meaning consecutive observations without
#' a dose record in between. Any group of `min_cluster` or more observations within at most `max_distance` from
#' one another is considered a cluster. `sparse()` returns the original dataframe with a new column,
#' "SPARSE," which has value 1 for all observation points not contained in a cluster, and is 1 otherwise.
#'
#' @param data A NONMEM-formatted dataframe.
#' @param min_cluster The minimum size of a cluster of consecutive observations to be considered
#' intensive (i.e. not sparse) sampling.
#' @param max_distance The maximum allowed distance between the earliest and latest observation
#' of a single cluster.
#' @param plot Logical. Whether to make a plot of individual PK curves with sparse observations labeled.
#'
#' @author Sandy Floren
#' @export
# TODO: print QC plot by default

sparse <- function(data,
                   min_cluster = 3,
                   max_distance = 26,
                   plot = FALSE) {
  data <- data %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::group_modify( ~ {
      .x$SPARSE <- 1
      len <- nrow(.x)
      #distances <- array(, c(len, len))
      distances <- array(FALSE, c(len, len))
      for (i in 1:len) {
        if (.x[i, "EVID"] != 0) {
          next()
        }

        for (j in 1:len) {
          if (.x[j, "EVID"] != 0) {
            next()
          }

          if (abs(.x[j, "TIME"] - .x[i, "TIME"]) > max_distance) {
            if (.x[j, "TIME"] - .x[i, "TIME"] > max_distance) {
              break()
            } else {
              next()
            }

          } else {
            distances[i, j] <- TRUE
          }

        }
      }

      clusters <- NULL

      for (row in 1:len) {
        x <- which(distances[row,])

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

  if (plot) {
    p <- data %>%
      dplyr::filter(.data$EVID == 0) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$TIME, y = .data$DV, group = .data$ID)) +
      ggplot2::geom_line(color = "black", size = 0.3) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(.data$SPARSE))) +
      ggplot2::labs(color = "SPARSE") +
      ggplot2::scale_color_manual(values = c("red3", "darkblue"))

    p

  } else {
    data
  }
}
