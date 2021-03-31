#' @title popPKSim2
#' @author Alexander Floren
#'
#' @description
#'
#' @param model
#' @param dose_fun
#' @param data
#' @param n
#' @param start
#' @param end
#' @param sample_freq
#' @param dose_freq
#' @param tail
#' @param target
#' @param output
#' @param output
#' @param params
#' @param seed
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

# library(mlxR)
# library(ggplot2, warn.conflicts = FALSE)
# library(dplyr, warn.conflicts = FALSE)
# library(ellipsis)


# TODO auto-calculate time span
# TODO add flexibility to names of outputs (?)
# TODO implement command line interface
# TODO check that params are present in model for run_sim
# TODO add different modes (i.e. oral, infusion, etc.), specified in modfile,
#      that change how treatment dataframe is constructed in run_sim
# TODO structure code into a package



# Run simulations
run_sim <- function(model, dose_fun, data, n = NULL, start = 0, end = 36,
                    sample_freq = 1, dose_freq = NULL, tail = 24, target = "Ad",
                    output = c("Cc", "AUC"), params = NULL, seed = NULL) {
  if (is.data.frame(data)) {
    df <- data
  }
  else {
    if (file.exists(data)) {
      df <- read.csv(data)
    }
    else {
      stop("data must be a file or dataframe.")
    }
  }
  # select rows of input file
  if (is.numeric(n)) {
    df <- df[1:n,]
  }

  # check that sample_freq divides 12 hours evenly
  if (!(12 / sample_freq) %% 1 == 0) {
    stop("sample_freq must evenly divide 12.")
  }


  # check that dose_freq divides 24 hours evenly
  if (is.null(dose_freq)) {
    stop("dose_freq is a required argument.")
  }
  if (!(24 / dose_freq) %% 1 == 0) {
    stop("dose_freq must evenly divide 24.")
  }

  times <- seq(from = start, to = end, by = dose_freq)
  dosesPerDay <- 24 / dose_freq

  N <- nrow(df)
  numDoses <- length(times)

  # Total dataframe
  total <- df %>% dplyr::slice(rep(1:N, each = numDoses))

  # Structure "total" columns into a named list to be used as the arguments
  # to dose_fun
  arglist <- list()
  for (name in colnames(total)) {
    arglist[[sym(name)]] <-  total[[sym(name)]]
  }
  arglist$doseOccasion <- rep(1:dosesPerDay, (N * numDoses / dosesPerDay))

  # Occasions
  occ <- list(name = "occ", time = times)

  # Treatment
  trt <- data.frame(id = rep(df$ID, each = numDoses),
                    time = rep(times, N),
                    target = target,
                    amount = do.call(dose_fun, arglist))

  # Covariates
  cov <- df %>% rename(id = ID)

  # Define Output Format
  if (!is.null(params)) {
    out <- list(list(name = output,
                     time = seq(start, end + tail, by = sample_freq)),
                list(name = params))
  } else {
    out <- list(name = output,
                time = seq(start, end + tail, by = sample_freq))
  }

  # Simulate
  res <- simulx(model     = model$model,
                parameter = list(model$pk_par, cov),
                treatment = trt,
                output    = out,
                varlevel  = occ,
                settings  = list(seed = seed))
  res$data <- df

  return(res)
}

# Compute PK targets from simulation results
compute_targets <- function(res) {

  # Important assumptions:
  # 1. Steady-state has been reached at least 24 hours before last dose
  # 2. Dosing events are at regular intervals, starting at time 0
  targets <- data.frame(id = as.numeric(res$originalId$oriId))
  lastDoseTime <- max(res$occasion$time)

  doseFreq <- res$occasion$time[2] - res$occasion$time[1]

  if (exists("AUC", where = res)) {
    lastObsTime <- max(res$AUC$time)
    if (lastObsTime >= 12) {
      targets$AUC12 <- filter(res$AUC, time == 12)$AUC
    }
    if (lastObsTime >= 24) {
      targets$AUC24 <- filter(res$AUC, time == 24)$AUC
      AUC24_SS <- filter(res$AUC, time == lastObsTime)$AUC -
        filter(res$AUC, time == lastObsTime - 24)$AUC
      targets$AUC24_SS <- AUC24_SS
    }
  }

  if (exists("Cc", where = res)) {
    lastObsTime <- max(res$Cc$time)
    initial_dose <- filter(res$Cc, time <= doseFreq)
    targets$CMAX <- summarize(group_by(initial_dose, id),
                              CMAX = max(Cc), .groups = "keep")$CMAX
    if (lastObsTime >= 24) {
      steady_state_24h <- res$Cc %>%
        group_by(id) %>%
        filter(time >= lastDoseTime - 24 & time <= lastDoseTime) %>%
        summarize(CMAX_SS = max(Cc),
                  CMIN_SS = min(Cc),
                  .groups = "keep")
      targets$CMAX_SS <- steady_state_24h$CMAX_SS
      targets$CMIN_SS <- steady_state_24h$CMIN_SS
    }
    # targets$CMAX_SS <- steady_state_24h$CMAX_SS
    # targets$CMIN_SS <- steady_state_24h$CMIN_SS

    targets$C_trough_SS <- filter(res$Cc, time == lastDoseTime)$Cc
  }

  targets <- inner_join(res$data, targets, by = c("ID" = "id"))

  return(targets)
}


# Write simulation results and PK targets
write_res <- function(res, result_folder = "out", plot = FALSE,
                      color = "darkred") {
  oriId <- res$originalId$oriId
  for (name in names(res)) {
    if (name == "originalId") {
      next()
    }
    res[[sym(name)]] <- mutate(res[[sym(name)]], originalId = oriId[id])
  }
  targets <- compute_targets(res)
  writeDatamlx(res, result.folder = result_folder,
               result.file = paste(result_folder, "total.csv", sep = "/"))
  write.csv(targets, paste(result_folder, "targets.csv", sep = "/"))
  # Create and save AUC and Cc plots
  if (plot) {
    dir.create(paste0(result_folder, "/plots"))
    auc_plot <- prctilemlx(res$AUC, color = color) +
      ggtitle(paste("AUC:", tail(strsplit(result_folder, "/")[[1]], 1)))
    cc_plot <- prctilemlx(res$Cc, color = color) +
      ggtitle(paste("Cc", tail(strsplit(result_folder, "/")[[1]], 1)))
    suppressMessages(ggsave(filename = paste0("AUC", ".png"),
                            plot = auc_plot,
                            path = paste0(result_folder, "/plots"),
                            device = "png"))
    suppressMessages(ggsave(filename = paste0("Cc", ".png"),
                            plot = cc_plot,
                            path = paste0(result_folder, "/plots"),
                            device = "png"))

  }
}


# Helper function: Compare all models with a single dosing regimen
compareAll_models0 <- function(modfile, regimen, data, folder, plot = FALSE,
                               ...) {

  source(modfile)

  if (!exists("models")) {
    stop("Make sure to list the names of the models to compare in a variable
         called \"models\".")
  }
  if (!exists("regimens")) {
    stop("Make sure to list the names of the dosing regimens in a variable
         called \"regimens\".")
  }
  if (!regimen %in% regimens) {
    stop("regimen not found.")
  }
  reg <- get(regimen)
  for (mod in models) {
    model <- get(mod)
    res <- run_sim(model, reg$dose_fun, data, dose_freq = reg$freq, ...)
    write_res(res, result_folder = paste(folder, "/", mod, "-", r, sep = ""),
              plot)
  }
}


# Helper function: Compare all dosing regimens with a single model
compareAll_regimens0 <- function(modfile, mod, data, folder, plot = FALSE,
                                 ...) {
  source(modfile)

  if (!exists("models")) {
    stop("Make sure to list the names of the models to compare in a variable
         called \"models\".")
  }
  if (!exists("regimens")) {
    stop("Make sure to list the names of the dosing regimens in a variable
         called \"regimens\".")
  }
  if (!mod %in% models) {
    stop("Model not found.")
  }

  model <- get(mod)
  for (r in regimens) {
    reg <- get(r)
    res <- run_sim(model, reg$dose_fun, data, dose_freq = reg$freq, ...)
    write_res(res, result_folder = paste(folder, "/", mod, "-", r, sep = ""),
              plot)
  }
}


# Compare all combinations of models and dosing regimens
compareAll0 <- function(modfile, data, folder = "compareAll", plot = FALSE,
                        ...) {

  source(modfile)

  if (dir.exists(folder)) {
    warning(paste("Folder", folder, "already exists in current working
                  directory. Overwriting."), call. = FALSE)
    unlink(folder, recursive = TRUE)
  }
  dir.create(folder)

  if (!exists("models")) {
    stop("Make sure to list the names of the models to compare in a variable
         called \"models\".")
  }
  if (!exists("regimens")) {
    stop("Make sure to list the names of the dosing functions to compare in a
         variable called \"regimens\".")
  }

  for (mod in models) {
    compareAll_regimens0(modfile, mod, data, folder, plot, ...)
  }

  files <- list.files(folder)
  dat <- read.csv(data)
  if ("n" %in% names(list(...))) {
    dat <- dat[1:list(...)$n, ]
  }
  f <- read.csv(paste(folder, files[1], "targets.csv", sep = "/"))
  dat <- inner_join(dat, select(f, -id, -X), by = c("ID" = "originalId"))
  if (length(files) > 1) {
    for (i in seq(2, length(files))) {
      f <- read.csv(paste(folder, files[i], "targets.csv", sep = "/"))
      targets <- select(f, -id, -X)
      sfx <- c(paste0("-", files[i - 1]), paste0("-",files[i]))
      dat <- inner_join(dat, targets, by = c("ID" = "originalId"), suffix = sfx)
    }
  }
  write.csv(dat, paste(folder, "all_targets.csv", sep = "/"))
}



# smodel constructor
new_smodel <- function(mod) {
  if (!is.character(mod$model)) {
    stop(
      "model must be a .txt file or a character string.",
      .call = FALSE
    )
  }
  else {
    if (grepl("\\.txt", mod$model) & !file.exists(mod$model)) {
      stop(
        paste0(mod$model, " des not exist in the current working directory."),
        .call = FALSE
      )
    }
    mod$model <- inlineModel(mod$model)
  }


  structure(mod, class = "smodel")
}

# smodel validator
validate_smodel <- function(mod) {
  model <- mod$model
  pk_par <- mod$pk_par
  if (is.null(model)) {
    stop(
      "No model was specified.",
      .call = FALSE
    )
  }

  if (is.null(pk_par)) {
    stop(
      "No pk_par was specified.",
      .call = FALSE
    )
  }
  mod
}

# User-friendly constructor
smodel <- function(mod) {
  validate_smodel(new_smodel(mod))
}


sim <- function(model, regimen, data, ...) {
  ellipsis::check_dots_used()
  UseMethod("sim")
}

sim.smodel <- function(model, regimen, data, ...) {
  run_sim(model, regimen$dose_fun, data, dose_freq = regimen$freq, ...)
}



# Helper function: Compare all dosing regimens with a single model
compareAll_regimens <- function(mod, regimens, mod_name, reg_names, data, folder, plot = FALSE,
                                ...) {
  i <- 1
  for (reg in regimens) {
    reg_name <- reg_names[[i]]
    res <- run_sim(mod, reg$dose_fun, data, dose_freq = reg$freq, ...)
    write_res(res, result_folder = paste(folder, "/", mod_name, "-",
                                         reg_name, sep = ""),
              plot)
    i <- i + 1
  }
}


# Compare all combinations of models and dosing regimens
compareAll <- function(models, regimens, data, dir = "compareAll", plot = FALSE,
                       ...) {

  if (dir.exists(dir)) {
    warning(paste("Folder", dir, "already exists in current working
                  directory. Overwriting."), call. = FALSE)
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)

  if (is.data.frame(data)) {
    dat <- data
  }
  else {
    if (file.exists(data)) {
      dat <- read.csv(data)
    }
    else {
      stop("data must be a file or dataframe.")
    }
  }

  reg_names <- sapply(substitute(regimens), deparse)[-1]
  i <- 2
  for (mod in models) {
    mod_name <- deparse(substitute(models)[[i]])
    compareAll_regimens(mod, regimens, mod_name, reg_names, data, dir, plot, ...)
    i <- i + 1
  }

  files <- list.files(dir)

  if ("n" %in% names(list(...))) {
    dat <- dat[1:list(...)$n, ]
  }
  f <- read.csv(paste(dir, files[1], "targets.csv", sep = "/"))
  dat <- inner_join(dat, select(f, -id, -X), by = c("ID" = "originalId"))
  if (length(files) > 1) {
    for (i in seq(2, length(files))) {
      f <- read.csv(paste(dir, files[i], "targets.csv", sep = "/"))
      targets <- select(f, -id, -X)
      sfx <- c(paste0("-", files[i - 1]), paste0("-",files[i]))
      dat <- inner_join(dat, targets, by = c("ID" = "originalId"), suffix = sfx)
    }
  }

  write.csv(dat, paste(dir, "all_targets.csv", sep = "/"))
}

#source("DTGModels2.R")

# system.time(compareAll("ABCModels2.R", "India_1.csv", plot = TRUE, n = 200, params = c("CL")))
#result <- run.sim(viiv, who_dose$dose_fun, "India_1.csv",
#                 end = 24, sample.freq = 1, n = 20, dose.freq = who_dose$freq)
#result
#write.res(result, plot = TRUE)

#compareAll("DTGModels2.R", "India_1.csv", plot= TRUE, end = 24, sample.freq = 1, n = 20)
