#' @title Plot ETA-covariate correlations
#' @author Kendra Radtke, Dhruv Vaish, Sandy Floren
#'
#' @description For categorical covariates, use `etacorr_cat()`. For continuous
#'  covariates, use `etacorr_cont()`.
#'
#' @param runno Either a xpose compliant run number, nonmem data in the form of a dataframe,
#' or string representing the path to a nonmem tablefile
#' @param eta Which ETAs to plot, e.g. `eta = c(1, 2)`
#' @param ... Covariates to plot against ETAs.
#'
#' @examples
#' \dontrun{
#' # Continuous Covariates:
#'
#' # Plot WT, HT, and AGE against ETA1 and ETA2 from run3
#' etacorr_cont(3, c(1, 2), WT, HT, AGE)
#'
#' # Plot WT, HT, and AGE against ETA1 and ETA2 from an xpose_data object
#' xpdb <- xpose::xpose_data(3)
#' etacorr_cont(xpdb, c(1, 2), WT, HT, AGE)
#'
#' # Categorical Covariates
#'
#' # Plot HIV against ETA1 and ETA2 from run3
#' etacorr_cat("run3", c(1, 2), WT, HT, AGE)
#' }
#' @rdname eta-correlations
#' @export
etacorr_cont <- function(runno, eta, ...) {

  data <- get_data_from_runno(runno)
  etas <- as.list(paste0("ETA", eta))
  ss <- data[!duplicated(data$ID), ] # 1 obs per ind
  vars <- as.list(sapply(substitute(list(...)), deparse)[-1])

  plot_cont <- function(eta, var, data) {
    ggplot2::ggplot(data, ggplot2::aes_string(x = var, y = eta)) +
      ggplot2::geom_point(shape = 21) +
      ggplot2::geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed',
                           color = 'red')
  }

  plot_vars <- function(eta) {lapply(vars, plot_cont, data = ss, eta = eta)}

  plots <- lapply(etas, plot_vars)
  plots <- as.list(purrr::flatten(plots))
  suppressMessages(ggpubr::ggarrange(plotlist = plots))
}

#' @rdname parse-SSE
#' @inheritParams etacorr_cont
#' @export
etacorr_cat <- function(runno, eta, ...) {

  data <- get_data_from_runno(runno)
  etas <- as.list(paste0("ETA", eta))
  ss <- data[!duplicated(data$ID), ] # 1 obs per ind
  vars <- as.list(sapply(substitute(list(...)), deparse)[-1])

  plot_cat <- function(eta, var, data) {
    ggplot2::ggplot(data, ggplot2::aes_string(x = var, y = eta)) +
      ggplot2::geom_boxplot(outlier.colour = NA)
  }

  plot_vars <- function(eta) {lapply(vars, plot_cat, data = ss, eta = eta)}

  plots <- lapply(etas, plot_vars)
  plots <- as.list(purrr::flatten(plots))
  suppressMessages(ggpubr::ggarrange(plotlist = plots))
}

# Helpers --------------------------------------------------------------------
# Provide either an xpdb object, run number, or model name and return the
# dataframe of results from that run.
get_data_from_runno <- function(runno) {
  if (class(runno)[1] == "xpose_data") {
    # runno is an xpdb object
    data <- runno$data$data[[1]]
  } else if (is.numeric(runno)) {
    # runno is a run number
    xpdb <- xpose::xpose_data(runno)
    data <- xpdb$data$data[[1]]
  } else {
    # runno is model name
    xpdb <- xpose::xpose_data(file = model_paste0(runno, ext = ".lst"))
    data <- xpdb$data$data[[1]]
  }
  data
}
