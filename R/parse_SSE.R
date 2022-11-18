#' @title Parse SSE results
#' @author Alexander Floren
#'
#' @description
#' `parse_sse()` parses a single .csv file of SSE summary statistics.
#'
#' @param file Name of the SSE results file. Default is "sse_results.csv".
#' @param param_stats Parameter statistics to summarize. Must be present in
#'   param_rows of the SSE results. Default is c("relative_rmse", "bias").
#' @param ofv_cols Column numbers to include from the ofv statistics table.
#' @param ofv_colNames OFV statistics column names. Must be the same length as
#'   ofv_cols.
#' @param sim_names Optionally, descriptive names for each alternative model,
#' in the same order as listed in the SSE results.
#'
#'   For example, if there are 3 alternative models called "alt_efv.mod",
#'   "alt_wt.mod", and "alt_age.mod", in that order, then you could use
#'   `simNames = c("efv", "wt, "age")` or something similar.
#' @param summary If TRUE, parse_sse creates a folder called "summary" within
#' the current directory and writes two .csv files to it summarizing the
#' results.
#'
#' @returns
#'   A named list of data frames.
#'   "params" is a data frame of parameter statistics.
#'   "type_I_error" and "type_II_error" are summaries of OFV statistics.
#'
#' @rdname parse-SSE
#' @examples
#'
#' @export

parse_sse <- function(file = "sse_results.csv",
                      param_stats = c("relative_rmse", "bias"),
                      ofv_cols = c(1, 4, 5, 6),
                      ofv_colNames = c("effect", "pow.05", "pow.01", "pow.001"),
                      sim_names = NULL,
                      summary = FALSE) {

  # read file
  df <- suppressWarnings(
    suppressMessages(
      readr::read_csv(file, col_names = as.character(1:100), skip_empty_rows = FALSE)))

  # compute number of THETAs, OMEGAs, and SIGMAs
  n_theta <- sum(grepl("THETA", df[5, ]))
  n_omega <- sum(grepl("OMEGA", df[5, ]))
  n_sigma <- sum(grepl("SIGMA", df[5, ]))

  # compute param_rows
  param_rows <- lapply(paste0("\\b", param_stats, "\\b"), grep, x = dplyr::pull(df, 1))
  param_rows <- unlist(lapply(param_rows, `[[`, 1))
  param_cols <- c("metric", "temp", paste0("TH_", 1:n_theta),
                  paste0("OM_", 1:n_omega), paste0("SI_", 1:n_sigma))
  colnames(df) <- param_cols
  df[ , length(param_cols) + 1:100] <- NULL

  # check format
  if (any(df[param_rows, param_cols[1]] != param_stats, na.rm = TRUE)) {
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
    readr::write_csv(param_df, file.path("summary", "param_stats.csv"),
                     append = TRUE)
    readr::write_csv(t1_df, file.path("summary", "type_I_error_ofv.csv"),
                     append = TRUE)
    readr::write_csv(t2_df, file.path("summary", "type_II_error_ofv.csv"),
                     append = TRUE)

  }

  if (!is.null(sim_names)) {
    t1_df[ , dplyr::sym(ofv_colNames[1])] <- sim_names
    t2_df[ , dplyr::sym(ofv_colNames[1])] <- sim_names
  }

  list("params" = param_df, "type_I_error" = t1_df, "type_II_error" = t2_df)
}

#' @title Summarize all SSE results in a directory
#'
#' @author Sandy Floren
#'
#' @description
#' `parse_all_sse()` recursively searches for SSE results csv files, parses them, and
#' combines them into data frames summarizing parameter statistics and error
#' rates.
#'
#' @param path Path to the parent directory containing the SSE results. Defaults
#' to the current working  directory.
#' @param exclude Optionally, a Perl-style regular expression matching file
#' paths to exclude from the results.
#'
#' @returns A named list of data frames.
#' "params" is a data frame of parameter statistics.
#' "type_I_error" and "type_II_error" are summaries of OFV statistics.
#'
#' @rdname parse-SSE
#' @examples
#' \dontrun{
#' setwd(~/Path/To/SSE/Results)
#'
#' # Parse all "sse_results.csv" files in a directory
#' parse_all_sse()
#'
#' # Parse all "sse_results.csv" files in a directory, except those with the
#' # pattern "induc" in their sub-directory
#' parse_all_sse(exclude = "induc")
#'
#' # Generate summary .csv files
#' parse_all_sse(summary = TRUE)
#' }
#' @export

parse_all_sse <- function(path = getwd(), exclude = NULL, file = "sse_results.csv",
                          param_stats = c("relative_rmse", "bias"),
                          ofv_cols = c(1, 4, 5, 6),
                          ofv_colNames = c("effect", "pow.05", "pow.01", "pow.001"),
                          sim_names = NULL,
                          summary = FALSE) {

  # convert to absolute path platform-independently
  path <- Sys.glob(file.path(path))
  path_names <- list.files(path, pattern = "sse_results.csv", recursive = TRUE,
                           full.names = TRUE)

  # exclude certain files
  if (!is.null(exclude)) {
    path_names <- grep(exclude, path_names, invert = TRUE, value = TRUE)
  }

  # determine directory depth
  relative_paths <- stringr::str_remove(path_names, path)

  # subtract 1 so as not to include "sse_results.csv" as a column
  max_depth <- max(sapply(
    strsplit(relative_paths, .Platform$file.sep), length)) - 1

  # parse all sse_results.csv files

  all_results <- lapply(path_names,
                        parse_sse,
                        param_stats = param_stats,
                        ofv_cols = ofv_cols,
                        ofv_colNames = ofv_colNames,
                        sim_names = sim_names,
                        summary = summary)

  # initialize empty lists
  param_dfs <- list()
  t1_dfs <- list()
  t2_dfs <- list()

  for (i in seq_along(all_results)) {
    params <- all_results[[i]]$params
    t1  <- all_results[[i]]$type_I_error
    t2  <- all_results[[i]]$type_II_error

    params_ncol <- ncol(params)
    t1_ncol <- ncol(t1)
    t2_ncol <- ncol(t2)

    # split path name
    substrings <- strsplit(relative_paths[i], .Platform$file.sep)[[1]]
    substrings <- substrings[2:(length(substrings) - 1)]

    # assign new column values
    for (j in 1:(max_depth - 1)) {
      params[ , as.character(paste0("X", j))] <- substrings[j]
      t1[ , as.character(paste0("X", j))] <- substrings[j]
      t2[ , as.character(paste0("X", j))] <- substrings[j]
    }

    param_dfs[[i]] <- as.data.frame(params)
    t1_dfs[[i]] <- as.data.frame(t1)
    t2_dfs[[i]] <- as.data.frame(t2)
  }

  # combine dataframes
  params_final <- dplyr::bind_rows(param_dfs)
  t1_final <- dplyr::bind_rows(t1_dfs)
  t2_final <- dplyr::bind_rows(t2_dfs)

  list("params" = params_final, "type_I_error" = t1_final,
       "type_II_error" = t2_final)
}
