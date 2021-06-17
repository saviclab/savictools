#' @title Create parameter tables
#' @author Alexander Floren
#'
#' @description
#'
#' @param ...
#' @param write
#' @param max_omega
#' @param max_sigma
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

param_table <- function(..., write = FALSE, max_omega = 30, max_sigma = 5,
                        filename = NULL) {
  runnos <- list(...)
  if (length(runnos) == 1) {
    return(nmsum(..., write))
  }

  result <- NULL
  ofvs <- c()
  i <- 1
  for (runno in runnos) {
    params <- nmsum(runno)
    colnames(params)[3:4] <- c(paste0("Value (run", runno, ")"), paste0("RSE (run", runno, ")"))
    ofvs <- c(ofvs, as.numeric(params[params$Parameter == "OFV", 3]))
    if (is.null(result)) {
      result <- params
      i <- i + 1
      next()
    }
    based_on_index <- which(dplyr::pull(params, 3)[2] == paste0("run", runnos))

    if (length(based_on_index) == 1) {
      params[params$Parameter == "dOFV", 3] <- as.character(round(ofvs[i] - ofvs[based_on_index], digits = 4))
    }
    result <- dplyr::full_join(result, params, by = c("Parameter", "Description"))
    i <- i + 1
  }

  omega <- c()
  sigma <- c()
  for (i in 1:max_omega) {
    omega <- c(omega, paste0("OMEGA(", i, ",", 1:max_omega, ")"))
  }
  for (i in 1:max_sigma) {
    sigma <- c(sigma, paste0("SIGMA(", i, ",", 1:max_sigma, ")"))
  }
  x <- c("Model:", "Based on:", "Description:", paste0("THETA", 1:100), omega, sigma, "OFV", "dOFV")

  result <- dplyr::slice(result, match(x, result$Parameter))

  if (write) {
    if (is.null(filename)) {
      readr::write_csv(result, paste("runs", ..., "params.csv", sep = "_"))
    } else {
      readr::write_csv(result, filename)
    }
  }
  else {
    result
  }
}

nmsum <- function(runno, write = FALSE) {

  xpdb <- xpose::xpose_data(runno, quiet = TRUE)
  summary <- xpose::get_summary(xpdb)
  ofv <- dplyr::filter(summary, label == "ofv")$value
  ref <- paste0("run", dplyr::filter(summary, label == "ref")$value)
  descr <- dplyr::filter(summary, label == "descr")$value

  param_tab <- xpose::get_prm(xpdb) %>%
    dplyr::select(name, label, value, rse, fixed) %>%
    dplyr::mutate(rse = ifelse(fixed == TRUE, "FIX", rse),
                  value = as.character(value)) %>%
    dplyr::select(-fixed)

  xpsum <- dplyr::bind_rows(list(name = "Model: ", label = "", value = paste0("run", runno), rse = ""),
                            list(name = "Based on:", label = "", value = ref, rse = ""),
                            list(name = "Description:", label = "", value=descr, rse = ""),
                            param_tab,
                            list(name = "", label = "", value = "", rse = ""),
                            list(name = "OFV", label = "Objective function value",
                                 value = ofv, rse = ""),
                            list(name = "dOFV", label = "Change in OFV", value = "", rse = ""))
  xpsum <-  dplyr::rename(xpsum, Parameter = name, Description = label, Value = value, RSE = rse)

  if (write) {
    write.csv(xpsum, paste0("run", n, "_params.csv"))
  } else {
    xpsum
  }
}

