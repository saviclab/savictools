#' @title ttcc
#' @author Sandy Floren
#'
#' @description
#' Find consecutive negatives past some threshold occuring within an interval.
#'
#' ttcc will assume that data is grouped by individual, so as to not compare
#' timestamps from two different patients. ttcc returns a vector `consec`` which
#' has value 1 at index i if `ttp[i]` at `time[i]``is both greater than or equal
#' to (default), or less than or equal to `threshold``, and is the ith consecutive
#' such observation spaced at least `interval`` apart.
#'
#' @param time a vector of numeric data representing the time of each observation
#' @param ttp a vector of numeric data representing Time to Positivity (TTP)
#' @param threshold a numeric bound for the value of `ttp``
#' @param interval a numeric upper bound for the length between two observations
#' @param compare a string to specify whether threshold is a lower (default) or
#' upper bound. "gt" (greater than) is the default, and makes `threshold``
#' a lower bound. "lt" (less than) makes `threshold`` an upper bound.
#'
#' @usage ttcc(time, ttp, threshold = , interval = , compare = "gt")
#'
#' @return a vector, `consec`` of 0s and 1s with the same length as `time``
#'
#' @examples
#' \dontrun{
#' data %>%
#'     group_by(ID) %>%
#'    summarize(
#'        time = TIME,
#'        ttp = DVTTP,
#'        TTCC = ttcc(TIME, DVTTP, threshold = 30, interval = 25)
#'    )
#'  }
#'
#' @export


ttcc <- function(time, ttp, threshold, interval, compare = "gt") {
  length <- length(time)

  # determine comparison type
  comp <- `>=`
  if (identical(compare, "lt")) {
    comp <- `<=`
  }
  else if (!identical(compare, "gt")) {
    warning("Invalid argument to compare. Defaulting to \"gt\"")
  }


  # initialize consec to all 0s
  consec <- rep(0, length)


  # look for consecutive negatives
  first_found <- FALSE
  first_time <- Inf
  for (i in seq(length)) {
    # test for positivity
    if (!comp(ttp[i], threshold)) {
      first_found <- FALSE
      first_time <- Inf
      next
    }

    if (!first_found) {
      first_found <- TRUE
      first_time <- time[i]
      next
    }

    # check if enough time has passed since last negative
    if (time[i] - first_time < interval) {
      next
    }

    consec[i] <- 1
  }

  consec
}
