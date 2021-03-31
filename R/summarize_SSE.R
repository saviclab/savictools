#' @title parse_sse
#' @author Alexander Floren
#'
#' @description
#' `parse_sse()` parses a single .csv file of SSE summary statistics.
#'
#' @param path Path to the SSE results file.
#' @param param_rows Row numbers of the desired parameter statistics in the
#'   SSE results.
#' @param n_theta Number of THETAs.
#' @param n_omega Number of OMEGAs.
#' @param n_sigma Number of SIGMAs.
#' @param paramStats Parameter statistics to summarize. Must be present in
#'   param_rows of the SSE results.
#' @param type Type of error for ofv statistics. Either "I" or "II".
#' @param ofv_cols Column numbers to include from the ofv statistics table.
#' @param ofv_colNames Ofv statistics column names. Must be the same length as
#'   ofv_cols.
#' @param simNames Descriptive names for each alternative model, in the same
#' order as in the SSE results.
#'
#'   For example, if there are 3 alternative models called "alt_efv.mod",
#'   "alt_wt.mod", and "alt_age.mod", in that order, then you could use
#'   `simNames = c("efv", "wt, "age")` or something similar.
#' @param summary If TRUE, parse_sse creates a folder called "summary" within
#' the current directory and writes two .csv files to it summarizing the
#' results.
#' @usage
#' `parse_sse(param_colNames, simNames)`
#' @returns
#'   A named list of dataframes.
#'   "params" is a dataframe of parameter statistics.
#'   "error" is a summary of ofv statistics (either type I or II).
#' @examples
#'
#' @export


parse_sse <- function(path = "sse_results.csv",
                      param_rows = 19:20,
                      n_theta,
                      n_omega,
                      n_sigma,
                      paramStats = c("relative_rmse", "bias"),
                      type = "II",
                      ofv_cols = c(1, 4, 5, 6),
                      ofv_colNames = c("effect", "pow.05", "pow.01", "pow.001"),
                      simNames,
                      summary = FALSE) {


  param_cols <- c("metric", "temp", paste0("TH_", 1:n_theta),
                  paste0("OM_", 1:n_omega), paste0("SI_", 1:n_sigma))

  # read file
  df <- suppressWarnings(
    suppressMessages(
      readr::read_csv(path, col_names = param_cols, skip_empty_rows = FALSE)))

  # check format
  if (any(df[param_rows, param_cols[1]] != paramStats, na.rm = TRUE)) {
    stop("Incorrect format. Check param_rows and param_stats.")
  }

  # find ofv stats
  ofv_start <- which(df[, param_cols[1]] == "ofv Statistics")
  ofv_df <- df[ofv_start:nrow(df), ]
  t1_start <- which(ofv_df[, param_cols[1]] ==
                      "Type I error rate")
  t2_start <- which(ofv_df[, param_cols[1]] ==
                      "1 - type II error rate (power)")

  # format
  t1_df <- ofv_df[(t1_start + 2):(t2_start - 2), ofv_cols]
  t2_df <- ofv_df[(t2_start + 2):nrow(ofv_df), ofv_cols]

  colnames(t1_df) <- ofv_colNames
  colnames(t2_df) <- ofv_colNames

  t1_df <- t1_df[2:nrow(t1_df),]
  t2_df <- t2_df[2:nrow(t2_df),]

  param_df <- df[param_rows, c(1, 3:length(param_cols))]

  # output
  if (summary) {
    dir.create("summary")
    readr::write_csv(param_df, "summary/param_stats.csv", append = TRUE)


    if (type == "I") {
      readr::write_csv(t1_df, "summary/type_I_error_ofv.csv", append = TRUE)
    }
    else if (type == "II") {
      readr::write_csv(t2_df, "summary/type_II_error_ofv.csv", append = TRUE)
    }
  }
  error_df <- t2_df
  if (type == "I") {
    error_df <- t1_df
  }
  error_df[dplyr::sym(ofv_colNames[1])] <- simNames

  list("params" = param_df, "error" = error_df)
}

#' @title get_sse
#' @author Alexander Floren
#'
#' @description
#' Recursively search a directory for SSE results, and combine into dataframes
#' for parameter metrics and error rates.
#'
#' @usage
#' `get_sse()`
#'
#' @returns a list of dataframes
#'
#' @examples
#'
#' @export

get_sse <- function(path, exclude = NULL, ...) {

  # convert to global path
  path <- Sys.glob(path)

  # remove trailing "/" character from path if present
  if (substr(path, nchar(path), nchar(path)) == "/") {
    path <- substr(path, 1, nchar(path) - 1)
  }

  path_names <- list.files(path, pattern = "sse_results.csv", recursive = TRUE,
                           full.names = TRUE)

  # exclude certain files
  if (!is.null(exclude)) {
    path_names <- grep(exclude, path_names, invert = TRUE, value = TRUE)
  }

  # determine directory depth
  relative_paths <- stringr::str_remove(path_names, path)

  # subtract 1 so as not to include "sse_results.csv" as a column
  max_depth <- max(sapply(strsplit(relative_paths, "/"), length)) - 1

  # parse all sse_results.csv files
  all_results <- lapply(path_names, parse_sse, ...)

  # initialize empty lists
  param_dfs <- list()
  error_dfs <- list()

  for (i in seq_along(all_results)) {
    params <- all_results[[i]]$params
    error  <- all_results[[i]]$error

    params_ncol <- ncol(params)
    error_ncol <- ncol(error)

    # split path name on slashes
    substrings <- strsplit(relative_paths[i], "/")[[1]]

    # assign new column values
    for (j in 1:max_depth) {
      params[ , params_ncol + j] <- substrings[j]
      error[ , error_ncol + j] <- substrings[j]
    }

    # ss  <- stringr::str_extract(substrings[1], "\\d+")
    # des <- substrings[2]
    # iiv <- stringr::str_extract(substrings[3], "\\d+")

    param_dfs[[i]] <- as.data.frame(params)
    error_dfs[[i]] <- as.data.frame(error)
  }

  # combine dataframes
  p <- dplyr::bind_rows(param_dfs)
  e <- dplyr::bind_rows(error_dfs)

  list("params" = p, "error" = e)
}
