#' Clinical relevance plots
#'
#' The `cr_plot()` function visualizes the clinical relevance of covariate effects.
#'
#' Using the variance-covariance matrix together with parameter estimates,
#' `cr_plot()` displays the posterior distributions of covariate effects
#' relative to the range of clinical importance.
#' @export


# Example:
# #cr_plot(27, VWT = 1 + THETA(12), WAZF1 = 1 + THETA(11), KAFORMULATION = 1 + THETA(9))

cr_plot <- function(runno, effect_size=0.2, lo=0.025, hi=0.975, ...) {

  # delay evaluation of `...`, but get variable names
  theta_quos <- dplyr::enquos(...)
  new_varnames <- names(theta_quos)
  new_exprs <- theta_to_Xs(theta_quos)
  theta <- get_thetas_used(new_exprs)

  # get THETA estimates from .lst file
  mu <- get_final_params(paste0("run", runno, ".lst"))
  n_theta <- length(mu)

  # get variance-covariance matrix from .cov file
  variance_covariance_matrix <- get_cov(paste0("run", runno, ".cov"))
  theta_cov <- variance_covariance_matrix[1:n_theta, 1:n_theta]

  # sample from multivariate normal distribution
  boot <- data.frame(MASS::mvrnorm(n = 1000, mu = mu, Sigma = theta_cov))
  boot <- boot[, theta, drop = FALSE]


  # calculate covariate relations
  for (name in names(new_exprs)) {
    boot[[name]] <- eval(str2lang(new_exprs[[name]]), envir = boot)
  }

  boot <- dplyr::select(boot, dplyr::all_of(new_varnames))

  #print(boot)
  # change to long format
  boot <- tidyr::pivot_longer(boot, cols = names(boot))

  # restrict to interval between lo and hi
  boot <- boot[with(boot,
        value >= reapply(value, name, quantile, lo) &
        value <= reapply(value, name, quantile, hi)), ]
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

get_final_params <- function(file) {
  lst <- readLines(file)
  pos <- grep("FINAL PARAMETER ESTIMATE", lst)

  # find THETA estimates
  pos <- (grep("\\d", lst[pos:length(lst)]) + pos - 1)[2]
  mu <- strsplit(lst[pos], "\\s+")[[1]]
  mu <- as.numeric(mu[2:length(mu)])
  mu
}

theta_to_Xs <- function(theta_quos) {
  exprs <- sapply(theta_quos, rlang::quo_get_expr)
  result <- (gsub("[\\(\\)]", "", gsub("THETA\\(", "X", exprs)))
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
