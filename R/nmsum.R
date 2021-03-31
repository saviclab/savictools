#!/Library/Frameworks/R.Framework/Versions/Current/Resources/bin/Rscript --vanilla
#' @title nmsum
#' @author Alexander Floren
#'
#' @description
#'
#' @param n
#' @param write
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

# library(xpose, quietly = TRUE, warn.conflicts = FALSE)
# library(dplyr,  quietly = TRUE, warn.conflicts = FALSE)
# library(readr, quietly = TRUE, warn.conflicts = FALSE)

nmsum <- function(n, write=TRUE) {
  xpdb <- xpose::xpose_data(n)

  ofv <- dplyr::filter(xpose::get_summary(xpdb), label == "ofv")$value

  param_tab <- xpose::get_prm(xpdb) %>%
    dplyr::select(name,label,value,rse,fixed) %>%
    dplyr::mutate(rse = ifelse(fixed == TRUE, "FIX", rse),
                  value = as.character(value)) %>%
    dplyr::select(-fixed)

  xpsum <- dplyr::bind_rows(list(name="OFV", label = "Objective function value",
                                 value=ofv, rse=""),
                            list(name="dOFV", label="Change in OFV", value="", rse=""),
                            list(name="", label="", value="", rse=""),
                            param_tab) %>%
    dplyr::rename(Parameter=name, Description=label, Value=value, RSE=rse)

  if (write) write_csv(xpsum, paste0("run", n, "_params.csv"))
  xpsum
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  invisible(nmsum(args))
}
