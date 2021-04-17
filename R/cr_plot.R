#' Clinical relevance plots
#'
#' The `cr_plot()` function creates clinical relevance plots from NONMEM files.
#' Using the variance-covariance matrix together with parameter estimates,
#' `cr_plot()`


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
  mu <- mu[theta]

  # get variance-covariance matrix from .cov file
  variance_covariance_matrix <- get_cov(paste0("run", runno, ".cov"))
  theta_cov <- variance_covariance_matrix[theta, theta]

  # sample from multivariate normal distribution
  boot <- data.frame(MASS::mvrnorm(n = 1000, mu = mu, Sigma = theta_cov))
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

  # plot
  pl1 <- lattice::stripplot(
    name ~ value,
    boot,
    panel = metrumrg::panel.covplot,
    rlim = c(1 - effect_size, 1 + effect_size),
    xlim = c(0, 2),
    cuts = c(1 - effect_size, 1, 1 + effect_size),
    xlab = 'Change in parameter value relative to reference',
    shade = 'skyblue'
  )

  print(pl1)
}

# Helpers --------------------------------------------------------------------

# The following function is copied from MIFuns, which was removed from CRAN..
# Author: Tim Bergsma, Metrum Institute
reapply <- function(x, INDEX, FUN, ...)
{
  if(!is.list(INDEX)) INDEX <- list(INDEX)
  INDEX <- lapply(INDEX,function(x)as.integer(factor(x)))
  INDEX <- as.integer(do.call(interaction,c(INDEX,drop=TRUE)))
  form <- tapply(x, INDEX)
  calc <- tapply(x, INDEX, FUN, ...,simplify=FALSE)
  need <- table(form)
  calc <- lapply(
    seq_along(calc),
    function(cell)rep(
      calc[[cell]],
      length.out=need[[
        as.character(cell)
        ]]
    )
  )
  calc <- c(calc,list(rep(NA,sum(is.na(form)))))
  form[is.na(form)] <- length(calc)
  grps <- split(form,form)
  grps <- lapply(
    grps,
    seq_along
  )
  elem <- unsplit(grps,form)
  sapply(
    seq_along(form),
    function(i)calc[[
      form[[i]]
      ]][[
        elem[[i]]
        ]]
  )
}


get_cov <- function(file) {
  read.table(file, skip = 1, header = TRUE)[ , -1]
}

get_theta <- function(file) {
  lst <- readLines(file)
  pos <- grep("FINAL PARAMETER ESTIMATE", lst)

  if (!grepl("TH", lst[pos + 9])) {
    stop("Problem with .lst format")
  }

  mu <- strsplit(lst[pos + 11], "\\s+")[[1]]
  mu <- as.numeric(mu[2:length(mu)])
  mu
}

