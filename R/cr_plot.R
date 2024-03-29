#' @title
#' Create a clinical relevance plot
#'
#' @author Hwi-yeol (Thomas) Yun, Sandy Floren
#'
#' @description
#' `cr_plot()` visualizes the clinical relevance of covariate effects.
#'
#' @details
#' Using the variance-covariance matrix together with parameter estimates,
#' `cr_plot()` displays the posterior distributions of covariate effects
#' relative to the range of clinical importance.
#'
#' @returns No return value, called for side effects
#'
#' @param runno A run number or model name.
#' @param effect_size The effect size needed for clinical relevance. Default is 0.2.
#' @param width Size of the interval of the posterior
#' distribution of covariate effects. Defaults to 0.95, or 95%.
#' @param type Plotting library to use. Either "lattice" (the default) or "ggplot".
#' @param adjust Passed to `adjust` argument in `ggplot2::geom_density`.
#' @param n Number of samples to draw from the normal distribution for bootstrapping parameter estimates.
#' @param ... Unquoted expressions representing covariate relations; see example.
#'
#' @examples
#' \dontrun{
#' # WT on V, WAZ on F1, FORMULATION on KA
#' cr_plot(27,
#'   VWT = 1 + THETA(12), WAZF1 = 1 + THETA(11),
#'   KAFORMULATION = 1 + THETA(9)
#' )
#' }
#'
#' @export

cr_plot <-
  function(runno,
           effect_size = 0.2,
           width = 0.95,
           type = "lattice",
           adjust = 1,
           n = 10000,
           ...) {
    # for R CMD check
    value <- NULL

    # delay evaluation of `...`, but get variable names
    theta_quos <- dplyr::enquos(...)
    new_varnames <- names(theta_quos)
    new_exprs <- theta_to_Xs(theta_quos)
    theta <- get_thetas_used(new_exprs)

    # get THETA estimates from .lst file
    mu <- get_final_params(model_paste0(runno, ".lst"))
    n_theta <- length(mu)

    # get variance-covariance matrix from .cov file
    variance_covariance_matrix <-
      get_cov(model_paste0(runno, ".cov"))
    theta_cov <- variance_covariance_matrix[1:n_theta, 1:n_theta]

    # sample from multivariate normal distribution
    boot <-
      data.frame(MASS::mvrnorm(n = n, mu = mu, Sigma = theta_cov))
    boot <- boot[, theta, drop = FALSE]

    # calculate covariate relations
    for (name in names(new_exprs)) {
      boot[[name]] <- eval(str2lang(new_exprs[[name]]), envir = boot)
    }

    boot <- dplyr::select(boot, dplyr::all_of(new_varnames))

    # change to long format
    boot <- tidyr::pivot_longer(boot, cols = names(boot))

    # restrict posterior distribution to correct interval width
    boot <- boot[with(
      boot,
      value >= reapply(value, name, quantile, (1 - width) / 2) &
        value <= reapply(value, name, quantile, 1 - ((1 - width) / 2))
    ), ]
    # plot

    if (type == "ggplot") {
      ggplot2::ggplot(boot, ggplot2::aes(x = value)) +
        ggplot2::geom_density(adjust = adjust, fill = "grey") +
        ggplot2::annotate(
          "rect",
          xmin = 1 - effect_size,
          xmax = 1 + effect_size,
          ymin = -Inf,
          ymax = Inf,
          alpha = 0.6,
          fill = "skyblue"
        ) +
        ggplot2::facet_grid(
          as.factor(name) ~ .,
          scales = "free_y",
          switch = "y",
          as.table = FALSE
        ) +
        # ggplot2::geom_vline(data=mu, ggplot2::aes(xintercept=grp.mean,
        # colour = name), linetype ="dashed", alpha = 1) + #Draw median line
        ggplot2::geom_hline(
          yintercept = 0,
          colour = "white",
          size = 1
        ) +
        # Remove baseline of density plot
        # Shade the range 0.8~1.2
        # ggplot2::geom_jitter(data = boot, ggplot2::aes(x=value, y=0),
        # alpha = 0.02, height = 1) +
        # Make dots of value
        ggplot2::xlim(0, 2) +
        ggplot2::xlab("Effect size") +
        ggplot2::theme(
          strip.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
    } else if (type == "lattice") {
      pl1 <- lattice::stripplot(
        name ~ value,
        boot,
        panel = panel.covplot,
        rlim = c(1 - effect_size, 1 + effect_size),
        xlim = c(0, 2),
        main = paste("Clinical Relevance of Covariates:", model_paste0(runno)),
        cuts = c(1 - effect_size, 1, 1 + effect_size),
        xlab = "Effect size",
        shade = "skyblue"
      )

      print(pl1)
    } else {
      stop("`type` must be either \"ggplot\" or \"lattice\"", call. = FALSE)
    }
  }


# Helpers --------------------------------------------------------------------

get_cov <- function(file) {
  utils::read.table(file, skip = 1, header = TRUE)[, -1]
}

get_final_params <- function(file) {
  lst <- readLines(file)
  pos <- grep("FINAL PARAMETER ESTIMATE", lst)

  # find THETA estimates
  pos <- (grep("\\s\\d", lst[pos:length(lst)]) + pos - 1)[2]
  mu <- strsplit(lst[pos], "\\s+")[[1]]
  mu <- as.numeric(mu[2:length(mu)])
  mu
}

theta_to_Xs <- function(theta_quos) {
  exprs <- sapply(theta_quos, rlang::quo_get_expr)
  result <- gsub("THETA", "X", exprs)
  result <- gsub("X\\((\\d+)\\)", "X\\1", result)
  names(result) <- names(theta_quos)
  result
}

get_thetas_used <- function(exprs) {
  words <- strsplit(exprs, "\\W")
  used <- c()
  for (i in 1:length(exprs)) {
    X_pos <- grep("X", words[[i]])
    thetas <- gsub("X", "", words[[i]])[X_pos]
    used <- c(used, thetas)
  }
  as.numeric(used)
}
