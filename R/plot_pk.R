#' @title plot_pk
#' @export

plot_pk <- function (data, ind = FALSE, nrow = 4, ncol = 4, id = NULL,
                     max_tad = 26)
{
  if (!exists("TAD", data)) {
    data <- tad(data)
  }
  data <- dplyr::filter(curve(data), EVID == 0, TAD <= max_tad)
  if (is.null(id)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = TAD, y = DV,
                                            group = CURVE))
  }
  else {
    data <- dplyr::filter(data, ID %in% id)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = TAD, y = DV,
                                            group = CURVE))
  }
  if (ind) {
    n_ids <- length(unique(data$ID))
    n_plots_remaining <- n_ids
    p <- p + ggplot2::geom_point(color = "purple", cex = 0.5) +
      ggplot2::geom_line(color = "grey")
    for (i in seq(1, ceiling(n_ids/(nrow * ncol)))) {
      if (n_plots_remaining != 1) {
        plot <- p + ggforce::facet_wrap_paginate("ID", nrow = nrow,
                                                 ncol = ncol, page = i)
        n_plots_remaining <- n_plots_remaining - (nrow * ncol)
      } else {
        title <- as.character(tail(data$ID))
        plot <- p + ggplot2::ggtitle(title)
      }
      print(plot)
    }
  }
  else {
    p + ggplot2::geom_point(color = "purple", cex = 0.5) +
      ggplot2::geom_line(color = "grey", alpha = 0.3)
  }
}
