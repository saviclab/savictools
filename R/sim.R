#' @title sim
#' @author Alexander Floren
#'
#' @description
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

sim <- function() {
  library("argparse")
  library(dplyr, warn.conflicts = FALSE)
  source("popPKSim.R")

  parser <- argparse::ArgumentParser(description = 'Simulate population PK trials.')
  parser$add_argument('modfile', nargs = 1, help = 'R script containing model specifications')
  parser$add_argument('--model', dest = 'model', nargs = '+', help = 'Names of models in the same order as infiles')
  parser$add_argument('--infile', dest = 'infile', nargs = '+', help = 'Csv file (or files) with population data')
  parser$add_argument('-R', '--recursive', dest = 'recursive', action = 'store_true', help = 'Should the model be applied to all .csv files in infile? If -R is specified, only one model can be used.')
  parser$add_argument('-B', '--bind', dest = 'bind', action = 'store_true', help = 'Should sim.R bind together all the results into one .csv? If -B is specified, you must also specify the name of the outfile (-o flag). If there is only one infile, -B has no effect.')

  parser$add_argument('-d', '--dosefun', dest = 'dosefun', nargs = '?', default = 'who_dose', help = 'Dosing function. Default: who_dose')
  parser$add_argument('-n', '--number', dest = 'number', nargs = '?', default = 0, type = "integer", help = 'Maximum ID to simulate up to')
  parser$add_argument('-s', '--start', dest = 'start', nargs = '?', default = 0, type = "integer", help = 'First administration time. Default: 0')
  parser$add_argument('-e', '--end', dest = 'end', nargs = '?', default = 36, type = "integer", help = 'Last administration time. Default: 36')
  parser$add_argument('-b', '--by', dest = 'by', nargs = '?', default = 12, type = "integer", help = 'Dose frequency. Must be a divisor of 24, e.g. 4, 6, 8, 24. Default: 12')
  parser$add_argument('-q', '--freq', dest = 'freq', nargs = '?', default = 12, type = "integer", help = 'Simulation sample frequency. Default: 12')
  parser$add_argument('-t', '--tail', dest = 'tail', nargs = '?', default = 24, type = "integer", help = 'How long to simulate after last dose. Default: 24')
  parser$add_argument('-u', '--units', dest = 'units', nargs = '?', default = 'hours', help = 'Time units for the above arguments. Default: "hours"')
  parser$add_argument('-c', '--compartment', dest = 'compartment', nargs = '?', default = 'Ad', help = 'Target compartment. Default: \"Ad\"')
  parser$add_argument('-a', '--age', dest = 'age', nargs = '?', default = 0, type = "integer", help = 'Age cutoff (months). Default: 0')
  parser$add_argument('-m', '--metrics', dest = 'metrics', nargs = '*', default = c("CP", "AUC"), help = 'Output metrics. Default: CP, AUC')
  parser$add_argument('-v', '--covariates', dest = 'covariates', nargs = '*', default = c("WT", "AGE"), help = 'Covariates. Default: WT, AGE')
  parser$add_argument('-A', '--doseargs', dest = 'doseargs', nargs = '*', default = NULL, help = 'Arguments to dosing function, except dosing occasion, which is calculated from frequency. Defaults to use same names as covariates')
  parser$add_argument('-o', '--outfile', dest = 'outfile', nargs = '?', default = NULL, help = 'Names of output files. Default is \'MODEL_INFILE.csv\'')
  parser$add_argument('-p', '--params', dest = 'params', nargs = '*', default = c("CL"), help = 'Output parameters. Default: CL')

  #test_args <- list("DTGModels.R", "--model", "viiv", "--infile",
  #                "top-30-batches", "-n=10", "-o=test", "-b=12", "-q=24", "-e=48", "-R")
  args      <- parser$parse_args()
  mod       <- args$model
  infile    <- args$infile
  recursive <- args$recursive
  bind      <- args$bind
  outfile   <- args$outfile

  run_sim_args <- list(modfile     = args$modfile,
                       dose_fun    = args$dosefun,
                       n           = args$number,
                       start       = args$start,
                       end         = args$end,
                       by          = args$by,
                       freq        = args$freq,
                       tail        = args$tail,
                       time_units  = args$units,
                       target      = args$compartment,
                       min_age     = args$age,
                       params      = args$params,
                       metrics     = unlist(args$metrics),
                       covariates  = as.list(args$covariates),
                       dose_args   = args$doseargs)

  if (recursive) {
    if (length(mod) > 1) {
      stop("-R option cannot be used with multiple models.", call. = FALSE)
    }
    if (length(infile) > 1) {
      stop("-R option cannot be used with multiple infiles. Supply a single directory path name.", call. = FALSE)
    }
    if (!dir.exists(infile)) {
      stop(paste(infile, "is not a directory. Remove the -R flag.", sep = " "))
    }

    infile <- list.files(path = infile, pattern = ".csv", recursive = TRUE, full.names = TRUE)
  }

  # make sure mod and infile have same length
  if (length(mod) > 1) {
    if (length(mod) != length(infile)) {
      stop("If using multiple models, number of models and infiles must match.", call. = FALSE)
    }
  } else {
    mod <- rep(mod, length(infile))
  }

  if (is.null(outfile)) {
    if (bind) {
      stop("If -B is specified, you must also specify the name of the outfile (-o flag).")
    }

    # create vector of outfile names using defaults
    outfile <- c()
    if (recursive) {
      for (i in seq(length(infile))) {
        # remove leading directory names from infile path
        csv <- tail(strsplit(infile[i], "/")[[1]], 1)
        outfile[i] <- paste(toupper(mod[i]), "_", toupper(strsplit(csv, ".csv")[[1]]), ".csv", sep = "")
      }
    } else {
      for (i in seq(length(infile))) {
        outfile[i] <- paste(toupper(mod[i]), "_", toupper(strsplit(infile[i], ".csv")[[1]]), ".csv", sep = "")
      }
    }
  } else {
    if (length(outfile) > 1) {
      if (bind) {
        stop("If -B is specified, you must only provide one outfile name (-o flag).")
      }
      if (length(outfile) != length(infile)) {
        stop("If using multiple outfiles, the number of outfiles and infiles must match.")
      }
    } else {
      if (!bind) {
        if (dir.exists(outfile)) {
          parent <- outfile
          # create vector of outfile names using defaults
          outfile <- c()
          if (recursive) {
            for (i in seq(length(infile))) {
              # remove leading directory names from infile path
              csv <- tail(strsplit(infile[i], "/")[[1]], 1)
              outfile[i] <- paste(parent, "/", toupper(mod[i]), "_", toupper(strsplit(csv, ".csv")[[1]]), ".csv", sep = "")
            }
          } else {
            for (i in seq(length(infile))) {
              outfile[i] <- paste(parent, "/", toupper(mod[i]), "_", toupper(strsplit(infile[i], ".csv")[[1]]), ".csv", sep = "")
            }
          }
        } else {
          stop("If bind (-B flag) is not specified, outfile (-o) must be either a directory or left blank")
        }
      }
    }
  }

  if (args$n == 0) {
    run_sim_args$n <- NULL
  }

  df_list <- list(NULL)






  if (bind) {
    for (i in seq(length(infile))) {
      cat("Running model", mod[i], "on", infile[i], "\n")
      df_list[[i]] <- do.call(run_sim, c(mod[i], infile[i], run_sim_args))
    }
    complete_df <- dplyr::bind_rows(df_list) %>% dplyr::arrange(ID)
    write.csv(complete_df, outfile)
  } else {
    for (i in seq(length(infile))) {
      cat("Running model", mod[i], "on", infile[i], "\n")
      write.csv(do.call(run_sim, c(mod[i], infile[i], run_sim_args)), outfile[i])
    }
  }
}
