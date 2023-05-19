#' @title Flexibly process NONMEM file names
#'
#' @author Sandy Floren
#'
#' @description
#' `model_paste0()` can take either an integer, the name of a model as a string,
#' or a complete file name and returns a model name with the desired file
#' extension.
#'
#' @returns A string corresponding to the name of a NONMEM file.
#'
#' @param runno Either a NONMEM model name (with or without file extension), or
#' the number of a NONMEM run as an integer.
#' @param ext Optional. A file extension (e.g. ".lst") to append to the model
#'  name.
#'
#' @examples
#' # Stripping a NONMEM file name to just the model name
#' model_paste0("run46.mod")
#'
#' # Constructing a run name from a run number
#' model_paste0(46)
#'
#' # Changing the file extension
#' model_paste0("run46.mod", ext = ".lst")
#'
#' @export
model_paste0 <- function(runno, ext = "") {
  # If runno is a number, return "run[runno].ext"
  ifelse(suppressWarnings(!is.na(as.numeric(
    as.character(runno)
  ))),
  paste0("run", runno, ext),
  # If runno is a file name, strip any file extension off and return "runno.ext"
  paste0(gsub("\\.\\w*", "", runno), ext)
  )
}
