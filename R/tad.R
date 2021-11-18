#' Calculate time after dose (TAD)
#'
#' `tad()` computes the time after dose, adding a new column to a dataframe.
#'
#' `tad()` assumes that the identifier column is called ID, the event id column
#' is called EVID, and the time column is called TIME. Data is grouped by ID
#' internally before TAD is calculated.
#'
#' @param data A NONMEM-formatted dataframe
#'
#' @return A NONMEM-formatted dataframe with a TAD column
#'
#' @importFrom magrittr %>%
#' @author Alexander Floren
#' @export


tad <- function(data) {
  data %>%
    dplyr::group_by(ID) %>%
    dplyr::arrange(TIME) %>%
    dplyr::group_modify( ~ {
      addl_present <- FALSE
      if ("ADDL" %in% colnames(.x)) {
        if ("II" %in% colnames(.x)) {
          addl_present <- TRUE
        }
        else
          (stop("To use the ADDL data record, you must also specify II."))
      }
      evid <- pull(.x, EVID)
      copy <- .x
      copy$TAD <- 0
      last_dose <- as.double(dplyr::pull(.x, TIME)[2])
      for (j in seq(nrow(.x))) {
        this_evid <- evid[j]
        if (this_evid == 1 | this_evid == 4) {
          # deal with case of ADDL
          if (addl_present) {
            last_dose <- as.double(dplyr::pull(.x, TIME)[j]) +
              (as.double(dplyr::pull(.x, ADDL)[j]) *
                 as.double(dplyr::pull(.x, II)[j]))
          } else {
            last_dose <- as.double(dplyr::pull(.x, TIME)[j])
          }
        }
        if (this_evid == 0 | this_evid == 3) {
          copy[j, "TAD"] <- as.double(dplyr::pull(.x, TIME)[j]) - last_dose
        }
      }
      copy
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID, TIME, dplyr::desc(EVID))
}
