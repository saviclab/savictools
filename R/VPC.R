#' @title Visual predictive checks
#'
#' @author Alexander Floren
#'
#' @description `VPC()` allows you to run PsN's `vpc` command and generate a
#' visual predictive check with `xpose::xpose.VPC()` in one line of code in the
#' R console.
#'
#' @examples
#'   VPC(19, "-samples=500 -idv=TAD -bin_array=-0.5,0.5,1.3,2.3,3.3,5,7,10,17,28
#'   -bin_by_count=0", subset = "TAD < 40")
#'
#'
#'
#' If a vpc_runXX directory already exists and force = FALSE, psn_args can be
#' left blank, and you only need to give the run number.
#'
#'
#'
#'
#'
#' @export

# TODO: need to wait until psn vpc is done before calling xpose.VPC()

VPC <- function(runno = NULL,
                psn_args = NULL,
                force = FALSE,
                ...) {

  if (is.null(psn_args)) {
    psn_args <- trimws(runno)
  }
  psn_args <- unlist(strsplit(psn_args, " "))
  xpose_args <- list(...)

  # make sure model argument is included
  if (!grepl("\\.mod", psn_args[1])) {
    if (is.null(runno)) {
      stop("must specify runno or include your model name in psn_args.",
           call. = FALSE)
    }
    psn_args <- c(model_paste0(runno, ".mod"), psn_args)
    dir <- paste0("vpc_", model_paste0(runno))
  } else {
    dir <- paste0("vpc_", model_paste0(psn_args[1]))
  }

  # make sure directory argument is included
  if (!any(grepl("-directory=", psn_args))) {
    psn_args <- c(psn_args, paste0("-directory=", dir))
  } else {
    dir <- strsplit(grep("-directory=", psn_args, value = TRUE), "=")[[1]][2]
  }

  if (file.exists(dir) & !force) {
    message(paste0(dir), " already exists in working directory.")
    message("To run a new VPC anyway, use option force = TRUE")
  } else {
    system2("vpc", args = psn_args, wait = FALSE)
  }

  if (is.null(xpose_args$vpctab)) {
    xpose_args$vpctab <- list.files(path = paste0("./", dir),
                                    pattern = "^vpctab",
                                    full.names = TRUE)[1]
  }

  if (is.null(xpose_args$vpc.info)) {
    xpose_args$vpc.info <- paste0("./", dir, "/vpc_results.csv")
  }

  do.call(xpose4::xpose.VPC, xpose_args)
}
