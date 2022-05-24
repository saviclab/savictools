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
#' # Basic TAD calculation
#' tad(pk_example)
#'
#' @return A NONMEM-formatted dataframe with a TAD column
#'
#' @importFrom magrittr %>%
#' @author Sandy Floren
#' @export


tad <- function(data, ...) {
  cond <- dplyr::quo(...)
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
    dplyr::arrange(TIME, .by_group = TRUE) %>%
    dplyr::group_modify( ~ {
      copy <- .x %>%
        dplyr::mutate(rownum = 1:dplyr::n()) %>%
        dplyr::filter(!!cond)
      indices <- dplyr::pull(copy, rownum)
      evid <- dplyr::pull(.x, EVID)
      copy <- .x
      copy$TAD <- 0
      last_dose <- as.double(dplyr::pull(.x, TIME)[2])
      ii_prev <- 0
      for (i in seq(nrow(.x))) {
        this_evid <- evid[i]
        this_time <- as.double(dplyr::pull(.x, TIME)[i])
        # only count rows where dots are TRUE for last_dose calculation
        if (i %in% indices & (this_evid == 1 | this_evid == 4)) {
          # deal with case of ADDL
          if (addl_present) {
            ii_prev <- as.integer(dplyr::pull(.x, II)[i])
            # note that this is the only case where last_dose > this_time,
            # hence the modulo operation below is always appropriate.
            last_dose <- this_time +
              (as.integer(dplyr::pull(.x, ADDL)[i]) *
                 ii_prev)
          } else {
            last_dose <- this_time
          }
        }
        else {
          if (last_dose > this_time) {
            # calculate TAD as time difference from latest dose modulo II
            copy[i, "TAD"] <- (this_time - last_dose) %% ii_prev
          } else {
            copy[i, "TAD"] <- this_time - last_dose
          }
        }
      }
      copy
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID, TIME, dplyr::desc(EVID))
}
