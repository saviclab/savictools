#' @title NONMEM dataset utilites
#' @author Sandy Floren
#'

#' @description
#' Check a NONMEM dataset for formatting errors
#'
#' @param data A dataframe

#' @details
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @author Sandy Floren
#' @rdname nonmem-data
#' @export

nmcheck <- function(data) {
  check_evid(data)
  check_ss(data)
  check_addl_ii(data)
}


#' @description
#' Expand doses coded with ADDL to distinct rows for each dose.
#'
#' @param data a NONMEM-formatted dataset.
#' @param check whether to perform nmcheck.
#'
#' @rdname nonmem-data
#' @export

expand_addl <- function(data, check = T) {
  if (check)
    nmcheck(data)
  if (!("ADDL" %in% colnames(data))) {
    return(data)
  }
  dummy_id <- FALSE
  if (!("ID" %in% colnames(data))) {
    dummy_id <- TRUE
    data$ID <- 1
  }

  data <- data %>%
    dplyr::arrange(ID, TIME, desc(EVID), .by_group = TRUE)

  if (all(data$ADDL == 0)) {
    return(data)
  }

  doses <-
    data %>% dplyr::filter((EVID == 1 | EVID == 4) & ADDL > 0)
  time <- doses$TIME
  addl <- doses$ADDL
  ii <- doses$II

  tmp <- lapply(seq_len(length(addl)), function(i) {
    time_i <- time[i]
    addl_i <- addl[i]
    ii_i <- ii[i]
    new_rows <- dplyr::bind_rows(rep(list(doses[i, ]), addl_i))
    new_rows$TIME <-
      seq(time_i + ii_i, time_i + ii_i * addl_i, by = ii_i)
    new_rows$EVID <- 1
    new_rows
  })
  res <- dplyr::bind_rows(list(data, tmp)) %>%
    dplyr::mutate(ADDL = 0) %>%
    dplyr::arrange(ID, TIME, desc(EVID), by_group = TRUE)

  if (dummy_id) {
    res %>% dplyr::select(-ID)
  } else {
    res
  }

}


# EVID checker
check_evid <- function(data) {
  evid <- data$EVID

  data <- data %>%
    dplyr::mutate(
      AMT = if ("AMT" %in% colnames(.data))
        .data$AMT
      else
        NA,
      RATE = if ("RATE" %in% colnames(.data))
        .data$RATE
      else
        NA,
      II = if ("II" %in% colnames(.data))
        .data$II
      else
        NA,
      ADDL = if ("ADDL" %in% colnames(.data))
        .data$ADDL
      else
        NA,
      SS = if ("SS" %in% colnames(.data))
        .data$SS
      else
        NA
    )

  if (any(evid == 0 &
          (
            data$AMT != 0 |
            data$RATE != 0 |
            data$II != 0 | data$ADDL != 0 | data$SS != 0
          ),
          na.rm = TRUE)) {
    stop("When EVID is 0, dose-related data items (AMT, RATE, II, ADDL, SS) must be zero.")
  }

  if (any(evid == 1 &
          data$AMT == 0 &
          data$RATE == 0 &
          data$II == 0 & data$ADDL == 0 & data$SS == 0,
          na.rm = TRUE)) {
    stop(
      "When EVID is 1, one or more of AMT, RATE, II, ADDL, SS data items must be non-zero to define the dose."
    )
  }

  if (any(evid == 2 &
          (
            data$AMT != 0 |
            data$RATE != 0 |
            data$II != 0 | data$ADDL != 0 | data$SS != 0
          ),
          na.rm = TRUE)) {
    stop("When EVID is 2, dose-related data items (AMT, RATE, II, ADDL, SS) must be zero.")
  }

  if (any(evid == 3 &
          (
            data$AMT != 0 |
            data$RATE != 0 |
            data$II != 0 | data$ADDL != 0 | data$SS != 0
          ),
          na.rm = TRUE)) {
    stop("When EVID is 3, dose-related data items (AMT, RATE, II, ADDL, SS) must be zero.")
  }

  if (any(evid == 4 &
          data$AMT == 0 &
          data$RATE == 0 &
          data$II == 0 & data$ADDL == 0 & data$SS == 0,
          na.rm = TRUE)) {
    stop(
      "When EVID is 4, one or more of AMT, RATE, II, ADDL, SS data items must be non-zero to define the dose."
    )
  }

}


# PREDPP  recognizes two varieties of doses, transient and steady-state.
# Steady-state  doses  are  described  separately   (See SS_dose_event).
# Transient doses are described here.


# II/ADDL checker

check_addl_ii <- function(data) {
  # ADDL without II
  if (("ADDL" %in% colnames(data) & (!"II" %in% colnames(data)))) {
    stop("To use the ADDL data record, you must also specify II.")
  }

  # no II or ADDL
  if (!("II" %in% colnames(data))) {
    return()
  }

  # II present
  data <- data %>%
    dplyr::mutate(
      SS = if ("SS" %in% colnames(.data))
        .data$SS
      else
        NA,
      RATE = if ("RATE" %in% colnames(.data))
        .data$RATE
      else
        NA,
      ADDL = if ("ADDL" %in% colnames(.data))
        .data$ADDL
      else
        NA
    )


  if (any(data$SS > 0 &
          data$AMT == 0 & data$RATE > 0 & data$II != 0, na.rm = TRUE)) {
    stop("For a steady-state infusion (AMT=0; RATE>0), II should be 0.")
  }

  if (any(data$SS > 0 & data$II == 0, na.rm = TRUE)) {
    stop("For non-infusion steady-state doses, II should be a positive number.")
  }

  if (any(data$EVID == 1 &
          data$II > 0 & data$ADDL <= 0, na.rm = TRUE)) {
    stop(
      "For non-steady-state doses, II should be a positive number if and only
            if the ADDL data item is a positive number."
    )
  }

}





check_ss <- function(data) {
  if (!("SS" %in% colnames(data))) {
    return()
  }

  if (!("II" %in% colnames(data))) {
    stop("To use the SS data record, you must also specify II.")
  }

}
