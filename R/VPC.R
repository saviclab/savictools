#' @title Visual predictive checks
#'
#' @description
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
                samples = 500,
                idv = "TAD",
                force = FALSE,
                psn_args = list(),
                xpose_args = list()) {


  if (is.character(psn_args)) {
    psn_args <- as.list(unlist(strsplit(psn_args, " ")))
  }

  if (!any(grepl("\\.mod", psn_args))) {
    if (is.null(runno)) {
      stop("must specify runno or include your model name in psn_args.",
           call. = FALSE)
    }
    model <- model_paste0(runno, ".mod")
    psn_args$model_arg <- model
    dir <- paste0("vpc_", model_paste0(runno))
  } else {
    #dir <- sub("(\\w*)\\.mod", "vpc_\\1", unlist(psn_args)[grepl("\\.mod", psn_args)])
    model <- unlist(psn_args)[grepl("\\.mod", psn_args)]
    dir <- paste0("vpc_", model_paste0(model))
  }


  if (!any(grepl("directory", psn_args))) {
    psn_args$directory_arg <- paste0("-directory=", dir)
  }
  if (!any(grepl("samples", psn_args))) {
    psn_args$samples_arg <- paste0("-samples=", samples)
  }

  if (!any(grepl("\\idv", psn_args))) {
    psn_args$idv_arg <- paste0("-idv=", idv)
  }

  psn_args <- unlist(psn_args)

  if (file.exists(dir) & !force) {
    message(paste0(dir), " already exists in working directory.")
    message("To run a new VPC anyway, use option force = TRUE")
  } else {
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
