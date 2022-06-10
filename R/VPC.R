  #' @title Visual predictive checks
  #'
  #' @author Sandy Floren
  #'
  #' @description
  #' `VPC()` allows you to run PsN's `vpc` command and generate a visual
  #' predictive check with `xpose4::xpose.VPC()` in one line of code in the R
  #' console.
  #'
  #' @details
  #' Let the name of your model for VPC be called "runXX.mod". runXX.mod must
  #' exist in your current working directory. If a vpc_runXX directory already
  #' exists from a previous VPC, *and* force = FALSE, psn_args can be left
  #' blank, and you need only supply the run number/model name.
  #'
  #' @param runno A run number or model name.
  #' @param psn_args Optional. A character string containing arguments to PsN's
  #' `vpc`.
  #' @param overwrite Optional. Logical. Should `VPC()` run a new VPC, deleting old
  #' VPC directories?
  #' @param ... Optional. Additional arguments to `xpose4::xpose.VPC()`.
  #'
  #' @examples
  #' \dontrun{
  #' # run VPC for run46.mod using runno = 46
  #' VPC(46, "-samples=500 -bin_array=-0.5,0.5,1.5,2.5,3.5,5,7,11,13,24.5
  #' -idv=TAD -bin_by_count=0", subset = "TAD <= 24")
  #'
  #' # run VPC for run46.mod using runno = "run46"
  #' VPC("run46", "-samples=500 -bin_array=-0.5,0.5,1.5,2.5,3.5,5,7,11,13,24.5
  #' -idv=TAD -bin_by_count=0", subset = "TAD <= 24")
  #'
  #' }
  #' @export


  # TODO: handle case of model names other than "runXX.mod"


  VPC <- function(runno,
                  psn_args = NULL,
                  overwrite = FALSE,
                  ...) {
    # ensure runno is a number
    runno <- as.numeric(stringr::str_extract(runno, "\\d+"))

    if (is.null(psn_args)) {
      psn_args <- ""
    }

    psn_args <- unlist(strsplit(psn_args, " "))
    psn_args <- c(model_paste0(runno, ".mod"), psn_args)
    xpose_args <- list(...)

    dir <- paste0("vpc_", model_paste0(runno))

    # ensure directory argument is included
    if (any(grepl("-directory=", psn_args))) {
      dir <- strsplit(strsplit(grep("-directory=", psn_args, value = TRUE),
                               "=")[[1]][2], " ")[[1]][1]
    } else {
      psn_args <- c(psn_args, paste0("-directory=", dir))
    }

    # overwrite old vpc directory, if necessary
    if (file.exists(dir)) {
      message(paste0(dir), " already exists in working directory.")
      if (overwrite) {
        # delete vpc directory and run new vpc
        message(paste0("Deleting ", dir, " and running new VPC."))
        unlink(dir, recursive = TRUE)
      }
    }

    system2("vpc", args = psn_args, wait = TRUE)

    # set vpc.info argument for xpose.VPC
    if (is.null(xpose_args$vpc.info)) {
      xpose_args$vpc.info <- paste0(dir, "/vpc_results.csv")
    }

    # set vpctab argument for xpose.VPC
    if (is.null(xpose_args$vpctab)) {
      xpose_args$vpctab <- paste0(dir, "/vpctab", runno)
    }

    # plot VPC
    do.call(xpose4::xpose.VPC, xpose_args)
  }
