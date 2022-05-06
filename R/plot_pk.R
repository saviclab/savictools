#' @title
#' Plot PK curves
#'
#' @description
#' `plot_pk()` creates spaghetti PK plots with connected data points for each
#' PK curve.
#'
#' @details
#' Using the variance-covariance matrix together with parameter estimates,
#' `cr_plot()` displays the posterior distributions of covariate effects
#' relative to the range of clinical importance.
#'
#' @param runno A run number or model name.
#' @param effect_size The size of a clinically relevant effect. Defaults is 0.2,
#' or 20%.
#' @param width Size of the interval of the posterior
#' distribution of covariate effects. Defaults to 0.95, or 95%.
#'
#' @examples
#' # WT on V, WAZ on F1, FORMULATION on KA
#' cr_plot(27, VWT = 1 + THETA(12), WAZF1 = 1 + THETA(11),
#'         KAFORMULATION = 1 + THETA(9))
#'
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
