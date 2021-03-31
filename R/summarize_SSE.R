#' @title parse_sse
#' @author Alexander Floren
#'
#' @description
#' Parse a csv file of SSE summary statistics.
#'
#' To change the paramStats field, make sure you also change the corresponding
#' param_rows field so that they match the row numbers in your sse_results.csv.
#'
#' Choose type I or type II error by changing ofv to either "I" or "II".
#'
#' @param x
#' @param param_rows
#' @param param_colNames
#' @param paramStats
#' @param ofv
#' @param ofv_cols
#' @param ofv_colNames
#' @param simNames
#' @param summary
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

# library(dplyr)
# library(readr)
# library(stringr)

parse_sse <- function(x = "sse_results.csv",
                      param_rows = 19:20,
                      param_colNames = c("metric", "temp", "TH_1", "TH_2",
                                         "TH_3", "TH_4", "TH_5", "TH_6", "TH_7",
                                         "TH_8", "TH_9", "OM_1", "OM_2", "OM_3",
                                         "OM_4", "SI_1"),
                      paramStats = c("relative_rmse", "bias"),
                      ofv = "II",
                      ofv_cols = c(1, 4, 5, 6),
                      ofv_colNames = c("effect", "pow.05", "pow.01", "pow.001"),
                      simNames = c("efv", "wt", "age"),
                      summary = FALSE) {


  # read file
  df <- readr::read_csv(x, col_names = param_colNames, skip_empty_rows = FALSE)

  # check format
  if (any(df[param_rows, param_colNames[1]] != paramStats)) {
    stop("Incorrect format. Check param_rows and param_stats.")
  }

  # find ofv stats
  ofv_start <- which(df[, param_colNames[1]] == "ofv Statistics")
  ofv_df <- df[ofv_start:nrow(df), ]
  t1_start <- which(ofv_df[, param_colNames[1]] ==
                      "Type I error rate")
  t2_start <- which(ofv_df[, param_colNames[1]] ==
                      "1 - type II error rate (power)")

  # format
  t1_df <- ofv_df[(t1_start + 2):(t2_start - 2), ofv_cols]
  t2_df <- ofv_df[(t2_start + 2):nrow(ofv_df), ofv_cols]

  colnames(t1_df) <- ofv_colNames
  colnames(t2_df) <- ofv_colNames

  t1_df <- t1_df[2:nrow(t1_df),]
  t2_df <- t2_df[2:nrow(t2_df),]

  param_df <- df[param_rows, c(1, 3:length(param_colNames))]

  # output
  if (summary) {
    dir.create("summary")
    readr::write_csv(param_df, "summary/param_stats.csv", append = TRUE)


    if (ofv == "I") {
      readr::write_csv(t1_df, "summary/type_I_error_ofv.csv", append = TRUE)
    }
    else if (ofv == "II") {
      readr::write_csv(t2_df, "summary/type_II_error_ofv.csv", append = TRUE)
    }
  }
  error_df <- t2_df
  if (ofv == "I") error_df <- t1_df
  error_df[sym(ofv_colNames[1])] <- simNames

  list("params" = param_df, "error" = error_df)
}

#' @title get_sse
#' @author Alexander Floren
#'
#' @description
#' Recursively search a directory for SSE results, and combine into dataframes
#' for parameter metrics and error.
#'
#' @usage
#'
#' @returns a list of dataframes
#'
#' @examples
#'
#' @export

get_sse <- function(parameter, induc = FALSE, ...) {

  path_names <- list.files(pattern = "sse_results.csv", recursive = TRUE)

  # remove induc results
  if (!induc) {
    path_names <- grep("induc", path_names, invert = TRUE, value = TRUE)
  }

  # parse all sse_results.csv files
  all_results <- lapply(path_names, parse_sse, ...)

  # initialize empty lists
  param_dfs <- list()
  error_dfs <- list()

  for (i in seq_along(all_results)) {
    params <- dplyr::select(all_results[[i]]$params, metric, all_of(parameter))
    error  <- all_results[[i]]$error

    # split path name on slashes
    substrings <- strsplit(path_names[i], "/")[[1]]
    ss  <- stringr::str_extract(substrings[1], "\\d+")
    des <- substrings[2]
    iiv <- stringr::str_extract(substrings[3], "\\d+")


    params <- dplyr::mutate(params, ss = ss, des = des, iiv = iiv)
    error  <- dplyr::mutate(error, ss = ss, des = des, iiv = iiv)

    param_dfs[[i]] <- as.data.frame(params)
    error_dfs[[i]] <- as.data.frame(error)
  }

  # combine dataframes
  p <- dplyr::bind_rows(param_dfs)
  e <- dplyr::bind_rows(error_dfs)

  list("params" = p, "error" = e)
}
