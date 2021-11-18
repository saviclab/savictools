#' Calculate time after dose (TAD)
#'
#' `tad()` computes the time after dose, adding a new column to a dataframe.
#'
#' `tad()` assumes that the identifier column is called ID, the event id column
#' is called EVID, and the time column is called TIME. Data is grouped by ID
#' internally before TAD is calculated.
#'
#' @param data A NONMEM-formatted dataframe
#' @param ... A condition that specifies for which rows to calculate TAD.
#'
#' @details Expressions in `...` are used to determine under what conditions a
#' row of `data` should count as a "dose" for  calculating time after dose. This
#' is especially useful if there is more than one type of dose event, and TAD
#' should only apply to one of them. For example, suppose the DV column of
#' `data` contains concentrations of a drug *X*. If a flag colum "Y_FLAG"
#' exists to indicate that a drug *Y* is given, as opposed to *X*, passing
#' `Y_FLAG = 0` to `...` will ensure that TAD is only calculated with respect to
#' drug *X* dosing events.
#'
#' @examples
#' # 1. Basic TAD calculation
#' tad(data)
#'
#' # 2. Calculating TAD only when RIF_FLAG is 0
#' tad(data, RIF_FLAG == 0)
#'
#'
#' @return A NONMEM-formatted dataframe with a TAD column
#'
#' @importFrom magrittr %>%
#' @author Alexander Floren
#' @export


tad <- function(data, ...) {
  addl_present <- FALSE
  if ("ADDL" %in% colnames(data)) {
    if ("II" %in% colnames(data)) {
      addl_present <- TRUE
    }
    else
      (stop("To use the ADDL data record, you must also specify II."))
  }

data %>%
    dplyr::group_by(ID) %>%
    dplyr::arrange(TIME) %>%
    dplyr::group_modify( ~ {

      indices <- which(mutate(.x, TEMP = !!quo(...))$TEMP)
      evid <- pull(.x, EVID)
      copy <- .x
      copy$TAD <- 0
      last_dose <- as.double(dplyr::pull(.x, TIME)[2])
      for (j in seq(nrow(.x))) {
        this_evid <- evid[j]
        # only count rows where dots are TRUE for last_dose calculation
        if (j %in% indices & (this_evid == 1 | this_evid == 4)) {
          # deal with case of ADDL
          if (addl_present) {
            last_dose <- as.double(dplyr::pull(.x, TIME)[j]) +
              (as.double(dplyr::pull(.x, ADDL)[j]) *
                 as.double(dplyr::pull(.x, II)[j]))
          } else {
            last_dose <- as.double(dplyr::pull(.x, TIME)[j])
          }
        }
        else {
          copy[j, "TAD"] <- as.double(dplyr::pull(.x, TIME)[j]) - last_dose
        }
      }
      copy
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID, TIME, dplyr::desc(EVID))
}
