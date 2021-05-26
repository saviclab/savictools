#' @title Visual predictive checks
#'
#' @description `VPC()` calls the PsN command `vpc`
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export

VPC <- function(runno = NULL,
                psn_args = NULL,
                force = FALSE,
                ...) {

  if (is.null(psn_args)) {
    psn_args <- runno
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
    print(psn_args)
    system2("vpc", args = psn_args)
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
