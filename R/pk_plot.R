#' @title
#' Plot PK curves
#'
#' @description
#' `pk_plot()` creates spaghetti PK plots with connected data points for each
#' PK curve.
#'
#'
#' @param data A dataframe with ID, EVID, DV, and TIME or TAD columns.
#' @param ind Whether to create separate plots for each ID.
#' @param nrow Number of rows per page of plots when ind = TRUE.
#' @param ncol Number of columns per page of plots when ind = TRUE.
#' @param id Optional. A vector of IDs to plot, instead of plotting every ID.
#' @param max_tad The largest allowed value of TAD to be plotted. Defaults to 26.
#'
#' @author Sandy Floren
#' @export

pk_plot <-
  function(data,
           ind = FALSE,
           nrow = 4,
           ncol = 4,
           id = NULL,
           max_tad = 26)
  {
    if (!exists("TAD", data)) {
      data <- tad(data, expand = T)
    }
    data <- dplyr::filter(curve(data), .data$EVID == 0, .data$TAD <= max_tad)
    if (is.null(id)) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$TAD, y = .data$DV,
                                              group = .data$CURVE))
    }
    else {
      data <- dplyr::filter(data, .data$ID %in% id)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$TAD, y = .data$DV,
                                              group = .data$CURVE))
    }
    if (ind) {
      n_ids <- length(unique(data$ID))
      n_plots_remaining <- n_ids
      p <- p + ggplot2::geom_point(cex = 0.5) +
        ggplot2::geom_line(color = "grey")
      for (i in seq(1, ceiling(n_ids / (nrow * ncol)))) {
        if (n_plots_remaining != 1) {
          plot <- p + ggforce::facet_wrap_paginate("ID",
                                                   nrow = nrow,
                                                   ncol = ncol,
                                                   page = i)
          n_plots_remaining <- n_plots_remaining - (nrow * ncol)
        } else {
          title <- as.character(utils::tail(data$ID))
          plot <- p + ggplot2::ggtitle(title)
        }
        print(plot)
      }
    }
    else {
      p + ggplot2::geom_point(cex = 0.5) +
        ggplot2::geom_line(color = "grey", alpha = 0.3)
    }
  }
