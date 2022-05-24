#' @title Create parameter tables
#' @author Sandy Floren
#'
#' @description
#'
#' @param ... Run number(s) or model names(s).
#' @param nice Whether to use Value \[%RSE\] formatting.
#' @param transform Whether to transform diagonal elements of OMEGA and SIGMA to standard deviations, and off-diagonal elements to correlations.
#' @param write Whether to write to a csv file.
#' @param filename Filename.
#' @param value_digits How many decimal places to display for parameter estimates when using "nice" formatting.
#' @param rse_digits How many decimal places to display for %RSE estimates when using "nice" formatting.
#' @param max_omega Maximum number of OMEGAs in model.
#' @param max_sigma Maximum number of SIGMAs in model.
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

param_table <-
  function(...,
           nice = TRUE,
           transform = TRUE,
           write = FALSE,
           filename = NULL,
           value_digits = 2,
           rse_digits = 1,
           max_omega = 30,
           max_sigma = 5) {
    runnos <- list(...)
    if (length(runnos) == 1) {
      return(nmsum(..., nice = nice, transform = transform, write = write, filename = filename,
                   value_digits = value_digits, rse_digits = rse_digits))
    }

    result <- NULL
    ofvs <- c()
    i <- 1
    for (runno in runnos) {
      params <- nmsum(runno, nice = nice, transform = transform, write = write, filename = filename,
                      value_digits = value_digits, rse_digits = rse_digits)
      max_col <- length(colnames(params))
      for (col in 3:max_col) {
        colnames(params)[col] <- paste0(colnames(params)[col], " (", model_paste0(runno), ")")
      }
      ofvs <-
        c(ofvs, as.numeric(params[params$Parameter == "OFV", 3]))
      if (is.null(result)) {
        result <- params
        i <- i + 1
        next()
      }
      based_on_index <-
        which(dplyr::pull(params, 3)[2] == model_paste0(runnos))

      if (length(based_on_index) == 1) {
        params[params$Parameter == "dOFV", 3] <-
          as.character(round(ofvs[i] - ofvs[based_on_index], digits = 4))
      }
      result <-
        dplyr::full_join(result, params, by = c("Parameter", "Description"))
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
    x <-
      c(
        "Model: ",
        "Based on:",
        "Description:",
        paste0("THETA", 1:100),
        omega,
        sigma,
        "OFV",
        "dOFV"
      )

    result <- dplyr::slice(result, match(x, result$Parameter))




    if (write) {
      if (is.null(filename)) {
        readr::write_csv(result, paste("runs", ..., "params.csv", sep = "_"), na = "",)
      } else {
        readr::write_csv(result, filename, na = "")
      }
    }
    else {
      result
    }
  }

nmsum <- function(runno,
                  nice = TRUE,
                  transform = TRUE,
                  write = FALSE,
                  filename = NULL,
                  value_digits = 2,
                  rse_digits = 1) {

  runno <- model_paste0(runno)
  xpdb <- xpose::xpose_data(runno, prefix = "", ext = ".lst", quiet = TRUE)
  summary <- xpose::get_summary(xpdb)
  ofv <- dplyr::filter(summary, label == "ofv")$value
  ref <- paste0("run", dplyr::filter(summary, label == "ref")$value)
  descr <- dplyr::filter(summary, label == "descr")$value

  param_tab <- xpose::get_prm(xpdb, transform = transform) %>%
    dplyr::select(name, label, value, rse, fixed) %>%
    dplyr::mutate(rse = ifelse(fixed == TRUE, "FIX", rse),
                  value = as.character(value)) %>%
    dplyr::select(-fixed)

  xpsum <-
    dplyr::bind_rows(
      list(
        name = "Model: ",
        label = "",
        value = paste0("run", runno),
        rse = ""
      ),
      list(
        name = "Based on:",
        label = "",
        value = ref,
        rse = ""
      ),
      list(
        name = "Description:",
        label = "",
        value = descr,
        rse = ""
      ),
      param_tab,
      list(
        name = "",
        label = "",
        value = "",
        rse = ""
      ),
      list(
        name = "OFV",
        label = "Objective function value",
        value = ofv,
        rse = ""
      ),
      list(
        name = "dOFV",
        label = "Change in OFV",
        value = "",
        rse = ""
      )
    )
  xpsum <-
    dplyr::rename(
      xpsum,
      Parameter = name,
      Description = label,
      Value = value,
      RSE = rse
    )


  if (nice) {
    xpsum <- xpsum %>%

      dplyr::mutate(Value_pct_RSE = paste0(
        round_format(Value, value_digits),
        " [",
        round_format(as.numeric(RSE) * 100, rse_digits),
        "]"
      )) %>%
      dplyr::select(-Value,-RSE)
    xpsum[1:3, "Value_pct_RSE"] <- ""
  }


  if (write) {
    readr::write_csv(xpsum, paste0("run", n, "_params.csv"))
  } else {
    xpsum
  }
}

#' @export
round_format <- function(x, digits) {
  round_lt_0 <- FALSE
  if (digits < 0) {
    fmt <- "%.0f"
    round_lt_0 <- TRUE
  } else {
    fmt <- paste0("%.", digits, "f")
  }
  if (round_lt_0) {
    suppressWarnings(sprintf(fmt, as.numeric(round(x, digits))))
  } else {
    suppressWarnings(sprintf(fmt, as.numeric(x)))
  }
}
