#' @title Calculate time after dose (TAD)
#'
#' @description `tad()` computes the time after dose, adding a new column to a dataframe.
#'
#' @details `tad()` assumes NONMEM-formatted data. It will expand the ADDL column internally,
#' but will return the data in its non-expanded form unless `expand` is set to TRUE.
#'
#' @param data A NONMEM-formatted dataframe.
#' @param cond A filtering condition that specifies for which rows to calculate TAD, as a string (optional).
#' @param expand Whether to expand the ADDL column in the result.
#'
#' @details Expressions in `...` are used to determine under what conditions a
#' row of `data` should count as a "dose" for calculating time after dose. This
#' is especially useful if there is more than one type of dose event, and TAD
#' should only apply to one of them. For example, suppose the DV column of
#' `data` contains concentrations of a drug *X*. If a flag column "Y_FLAG"
#' exists to indicate that a drug *Y* is given, as opposed to *X*, passing
#' `Y_FLAG = 0` to `...` will ensure that TAD is only calculated with respect to
#' drug *X* dosing events.
#'
#' @examples
#' # Basic TAD calculation
#' tad(pk_example)
#'
#' # compute TAD only for even ID numbers, and return in expanded form
#' tad(pk_example, "ID %% 2 == 0", expand = TRUE)
#'
#' @return A NONMEM-formatted dataframe with a TAD column.
#'
#' @importFrom magrittr %>%
#' @author Sandy Floren
#' @export

tad <- function(data, cond = "", expand = FALSE) {
  # format check
  nmcheck(data)

  cond <- dplyr::enexpr(cond)
  expanded_addl <- expand_addl(data, check = FALSE)

  res <- expanded_addl %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::arrange(.data$TIME, .by_group = TRUE) %>%
    dplyr::group_modify( ~ {
      evid <- as.integer(.x$EVID)
      time <- .x$TIME

      if (cond == "") {
        calc_tad <- rep(1, nrow(expanded_addl))
      } else {
        calc_tad <- .x %>%
          dplyr::mutate(calc_tad = as.numeric(!!cond)) %>%
          dplyr::pull("calc_tad")
      }

      # handle case of no dosing records or no observations
      if (!any(c(1, 4) %in% unique(evid)) |
          !(0 %in% unique(evid))) {
        return(.x)
      }

      .x$TAD <- .tad(evid, time, calc_tad)
      .x
    }) %>%
    dplyr::ungroup()

  if (expand | !("ADDL" %in% colnames(data))) {
    res
  } else {
    suppressMessages(dplyr::left_join(data, res) %>%
                       tidyr::replace_na(list(TAD = 0)))
  }
}


# Deprecated
tad_old <- function(data, ...) {
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
    dplyr::group_by(.data$ID) %>%
    dplyr::arrange(.data$TIME, .by_group = TRUE) %>%
    dplyr::group_modify( ~ {
      evid <- as.integer(dplyr::pull(.x, .data$EVID))

      copy <- .x %>%
        dplyr::mutate(rownum = 1:dplyr::n()) %>%
        dplyr::filter(!!cond)
      indices <- dplyr::pull(copy, rownum)
      copy <- .x
      copy$TAD <- dplyr::if_else(copy$EVID == 0, NA_real_, 0)

      # handle case of no dosing records or no observations
      if (!any(c(1, 4) %in% unique(evid)) |
          !(0 %in% unique(evid))) {
        return(copy)
      }

      dose_found <- FALSE
      last_dose <- as.double(dplyr::pull(.x, TIME)[2])

      ii_prev <- NA_real_

      if (addl_present) {
        ii_prev <-
          as.numeric(dplyr::pull(dplyr::filter(copy, II != 0), II)[1])
      }

      for (i in seq(nrow(.x))) {
        this_evid <- evid[i]
        this_time <- as.double(dplyr::pull(.x, TIME)[i])
        # only count rows where dots are TRUE for last_dose calculation

        # if this is a dose record
        if (i %in% indices & (this_evid == 1 | this_evid == 4)) {
          dose_found <- TRUE
          # ADDL
          if (addl_present) {
            ii_prev <- as.numeric(dplyr::pull(.x, II)[i])
            # note that this is the only case where last_dose > this_time,
            # hence the modulo operation below is always appropriate.
            last_dose <- this_time +
              (as.integer(dplyr::pull(.x, ADDL)[i]) *
                 ii_prev)
          }

          # no ADDL
          else {
            last_dose <- this_time
          }
        }

        # if this is an observation record
        else {
          # prior dosing record
          if (dose_found) {
            # ADDL
            if (last_dose > this_time) {
              # calculate TAD as time difference from latest dose modulo II
              copy[i, "TAD"] <- (this_time - last_dose) %% ii_prev
            }
            # no ADDL
            else {
              copy[i, "TAD"] <- this_time - last_dose
            }
          }

          # if no prior dosing record, leave TAD as NA.

        }
      }
      copy
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$ID, .data$TIME, dplyr::desc(.data$EVID))
}
