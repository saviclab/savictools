#' Clinical relevance plots
#'
#' The `cr_plot()` function creates clinical relevance plots from NONMEM files.
#' Using the variance-covariance matrix together with parameter estimates,
#' `cr_plot()` shows the position of posterior distributions of covariate values
#' relative to the range of clinical importance (usually +/- 20%).
#' @export


# Example:
# run number = 201
# THETA(6) is VWT, and is the only one needed to calculate VWT_5 and VWT_95
# the other expressions are passed directly to dplyr::mutate()
# cr_plot(201,
#         theta = c("VWT" = 6),
#         VWT_5 = (46.92/57.9)^VWT,
#         VWT_95 = (79.015/57.9)^VWT)

cr_plot <- function(runno, theta, effect_size=0.2, lo=0.025, hi=0.975, ...) {

  # delay evaluation of `...`, but get variable names
  exprs <- dplyr::enquos(...)
  new_varnames <- names(exprs)

  # get THETA estimates from .lst file
  mu <- get_theta(paste0("run", runno, ".lst"))
  n_theta <- length(mu)

  # get variance-covariance matrix from .cov file
  variance_covariance_matrix <- get_cov(paste0("run", runno, ".cov"))
  theta_cov <- variance_covariance_matrix[1:n_theta, 1:n_theta]

  # sample from multivariate normal distribution
  boot <- data.frame(MASS::mvrnorm(n = 1000, mu = mu, Sigma = theta_cov))
  boot <- boot[, theta]
  names(boot) <- names(theta)

  # calculate covariate relations
  boot <- dplyr::mutate(boot, ...)
  boot <- dplyr::select(boot, dplyr::all_of(new_varnames))

  # change to long format
  boot <- tidyr::pivot_longer(boot, cols = names(boot))

  # restrict to interval between lo and hi
  boot <- boot[with(boot,
        value >= reapply(value, name, quantile, lo) &
        value <= reapply(value, name, quantile, hi)), ]
  print(boot)
  # plot
  pl1 <- lattice::stripplot(
    name ~ value,
    boot,
    panel = panel.covplot,
    rlim = c(1 - effect_size, 1 + effect_size),
    xlim = c(0, 2),
    main = paste("Run", runno),
    cuts = c(1 - effect_size, 1, 1 + effect_size),
    xlab = 'Change in parameter value relative to reference',
    shade = 'skyblue'
  )

  print(pl1)
}

# Helpers --------------------------------------------------------------------


get_cov <- function(file) {
  read.table(file, skip = 1, header = TRUE)[ , -1]
}

get_theta <- function(file) {
  lst <- readLines(file)
  pos <- grep("FINAL PARAMETER ESTIMATE", lst)

  # find THETA estimates
  pos <- (grep("\\d", lst[pos:length(lst)]) + pos - 1)[2]
  mu <- strsplit(lst[pos], "\\s+")[[1]]
  mu <- as.numeric(mu[2:length(mu)])
  mu
}

