#' Create NONMEM ready datasets for simulated clinical PK trials
#'
#' The `cohort()` function is used to generate NONMEM-ready datasets for
#' clinical PK trials, either by sampling from real datasets or
#' generating synthetic data.
#'
#' @section Real Data:
#' To sample from an existing dataset, pass a dataframe or file name to
#' `data`, and leave `param` unspecified.
#'
#' Data will be filtered by the criteria specified in `include`, which must
#' use only variable names present in the data. `n` individuals will then be
#' sampled randomly.
#'
#' @section Synthetic Data:
#' To generate synthetic data, leave `data` unspecified and pass the details of
#' the distributions from which you wish to sample to `param`. Currently, all
#' variables are assumed to be independent.
#'
#' @section Dosing and Observation Events:
#' After sampling is finsihed, `cohort()` will create duplicate rows for each
#' individual corresponding to the timepoints specified in
#' `dose_times` and `obs_times`, and create a column "EVID" distinguishing
#' between dosing and observation events.
#'
#' Finally, the function (or fixed amount) passed to `amt` is used to calculate
#' the dose for each individual at each dosing event. If using a function, it
#' must be Vectorized, defined in the calling environment, and its arguments
#' must match the names of variables used in your data. See Example 6 for
#' details.
#'
#' @return
#' A dplyr dataframe in NONMEM format.
#'
#' @param data A dataframe, or the name of a file in the current directory.
#' @param include A character string in the form of a logical R statement, to
#' specify the inclusion criteria for this cohort. For example,
#' if "WT" and "HT" are variables corresponding to weight in kg
#' and height in cm, respectively, you can sample only
#' individuals below 50kg and below 150cm by writing:
#' include = "WT < 50 & HT < 150"
#' @param n An integer. The number of patients to enroll.
#' @param obs_times A numeric vector of observation times.
#' @param dose_times  A numeric vector of dosing times.
#' @param amt Either a numeric fixed dose, or a function that computes a
#' dose based on variables in your data (or which were specified
#' in param). If using a function, it must be Vectorized, and the
#' names of the variables in the function must match the names of
#' the variables in data (or param). This function should be
#' defined in the environment in which cohort is called.
#' @param param If creating synthetic data, this is where you specify the
#' distribution and parameters to use for random sampling. Supply
#' a list with named fields, each of which corresponds to a
#' variable. The value of each field should be another list,
#' containing (in order):
#'      -the name of an R stats function for random sampling, e.g.
#'         "rnorm", "runif", "rlnorm", etc.
#'       -the arguments to the above function (except "n", which you
#'         will have already specified).
#' For example, to create a normally-distributed random variable
#' called "WT" with mean 16.3 and standard deviation 2.5, and a
#' binomially distributed random variable called "HIV" with p = 0.34, write:
#' param = list("WT" = list("rnorm", 16.3, 2.5), "HIV" = list("rbinom", 1,
#'  0.34))
#' @param original_id Optional. Use the same ids as the input data? (TRUE or
#' FALSE).
#' @param group Optional. An integer indicating a group number. If specified,
#' output will include an additional column "GR" containing the
#' group number.
#' @param read_fun Optional. If using data in a format other than
#' comma-separated valued (csv), supply a function which reads the given format.
#' For example, if using tab-delimited data, write: read_fun = read.delim
#' @param pop_size Optional. If using synthetic data, cohort will generate a
#' population of size pop_size * n, and then randomly sample
#' from it. The default value is 10.
#' @param replace Optional. Whether to sample with replacement. Default: FALSE.
#'
#' @examples
#' # 1. Reading from a .csv and sampling 20 individuals,above 10kg and below
#' 120cm,
#' # with a fixed dose of 200mg, observing every 4 hours for one day and dosing
#' # at times 0, 5, and 12. Note that the data has columns called "WT" and "HT".
#'
#' inc <- "WT > 10 & HT < 120"
#' ot <- seq(0, 24, by = 4)
#' dt <- c(0, 5, 12)
#'
#' df1 <- cohort("my_data.csv", include = inc, n = 20, obs_times = ot,
#'               dose_times = dt, amt = 200)
#'
#'
#' # 2. As in (1), except using tab-delimited data.
#'
#' df2 <- cohort("my_data.txt", include = inc, n = 20, obs_times = ot,
#'               dose_times = dt, amt = 200, read_fun = read.table)
#'
#'
#' # 3, As in (1), except setting a seed and specifying a group number.
#'
#' df3 <- cohort("my_data.csv", include = inc, n = 20, obs_times = ot,
#'                dose_times = dt, amt = 200, seed = 123, group = 4)
#'
#'
#' # 4. As in (1), except we keep the ids from our original dataset.
#'
#' df4 <- cohort("my_data.csv", include = inc, n = 20, obs_times = ot,
#'               dose_times = dt, amt = 200, original_id = TRUE)
#'
#'
#' # 5. Simulating data. We assume WT and HT are normally distributed random
#' # variables, with means and standard deviations of 16 and 3.4 for WT and 132
#' # and 13.6 for HT.
#'
#' p1 <- list("WT" = list("rnorm", 16, 3.4), "HT" = list("rnorm", 132, 13.6))
#'
#' df5 <- cohort(param = p1, include = inc, n = 20,
#'               obs_times = ot, dose_times = dt, amt = 200)
#'
#'
#' # 6. As in (5), except we now define a dosing function.
#'
#' dose_fun <- Vectorize(function(WT) {
#'   if(WT < 16) {
#'     return(150)
#'   }  else if(WT < 20) {
#'     return(200)
#'   } else {
#'     return(250)
#'   }})
#'
#' df6 <- cohort(param = p1, include = inc, n = 20,
#'               obs_times = ot, dose_times = dt, amt = dose_fun)
#'
#' @author Alexander Floren
#' @export

cohort <- function(data=NULL, include=NULL, n=NULL, obs_times=NULL,
                   dose_times=NULL, amt=NULL, param=NULL, original_id=FALSE,
                   group=NULL, read_fun=NULL, pop_size=NULL,
                   replace=FALSE) {

  # check inputs
  if (is.null(data) & is.null(param)) {
    stop("Error: `data` and `param` cannot both be NULL.")
  }
  if (!is.null(data) & !is.null(param)) {
    stop("Error: specify one of `data` or `param`, but not both.")
  }
  if (!is.null(param) & (!is.list(param) | is.null(names(param)))) {
    stop("Error: `param` requires a list with named fields.")
  }
  if (!is.numeric(n) | n %% 1 != 0) {
    stop("Error: `n` must be of type integer")
  }
  if (is.null(amt) | (!is.function(amt) & !is.numeric(amt))) {
    stop("Error: `amt` must be of type numeric or function.")
  }
  if (!is.numeric(obs_times)) {
    stop("Error: `obs_times` must be of type numeric.")
  }
  if (!is.numeric(dose_times)) {
    stop("Error: `dose_times` must be of type numeric.")
  }
  if (is.null(read_fun)) {
    read_fun <- read.csv
  }
  if (!is.function(read_fun)) {
    stop("Error: `read_fun` must be of type function.")
  }
  if (is.integer(pop_size)) {
    pop_size  <- pop_size * n
  }
  if (is.null(pop_size)) {
    pop_size <- 10 * n
  }
  if (!is.numeric(pop_size) | pop_size %% 1 != 0) {
    stop("Error: `pop_size` must be of type integer.")
  }

  # initialize df
  df <- NULL

  # construct time vector and evid vector
  times <- c(dose_times, obs_times)
  evid <- c(rep(1, length(dose_times)), rep(0, length(obs_times)))
  n_timepoints <- length(times)

  # determine whether to use real or synthetic data
  if (!is.null(data)) {
    # use real data

    # use dataframe if supplied, or read from file
    if (is.data.frame(data)) {
      df <- data
    } else {
      df <- read_fun(data)
    }

  } else {
    # use synthetic data
    if (original_id != FALSE) {
      stop("Error: cannot use orginial ids with synthetic data.")
    }

    df <- dplyr::tibble("ID" = 1:pop_size)

    # convert to uppercase and truncate to 4 letters if needed
    names(param) <- toupper(names(param))
    # names(param) <- lapply(names(param), substring, 1, 4)

    # check param names
    if (length(dplyr::intersect(names(param),
                         c("ID", "TIME", "EVID", "AMT", "DV"))) > 0) {
      stop('Error: `param` cannot contain any of "ID", "TIME", "EVID", "AMT", or
           "DV".')
    }

    # sample distributions for each variable in param
    for (var in names(param)) {
      # extract name of sampling function, e.g. rnorm
      fun <- param[[var]][[1]]

      # extract function arguments
      fun_args <- c(pop_size, tail(param[[var]], -1))

      # sample pop_size times from the distribution
      df[var] <- do.call(fun, fun_args)
    }
  }


  # deal with original ids
  if (original_id != FALSE) {
    if (is.character(original_id) & original_id %in% colnames(df)) {
      dplyr::rename(df, "ID" = dplyr::all_of(original_id))
    }
  }
  dplyr::rename(df, "ID" = dplyr::matches("^id$", ignore.case = TRUE))

  if (!"ID" %in% colnames(df)) {
    stop("Error: data does not contain column named `id`
               (case insensitive).")
  }

  if (!is.null(include)) {
    # convert include criteria to R expression
    include <- str2expression(include)
    if (!is.logical(eval(include, envir = df))) {
      stop("Error: `include` requires an expression that evaluates to TRUE or
            FALSE, e.g.`AGE >= 6`.")
    }
    # apply include filter
    df <- filter(df, eval(include, envir = df))
  }

  # ensure one row per ID
  df <- dplyr::group_by(df, ID)
  df <- dplyr::slice(df, 1)
  df <- dplyr::ungroup(df)

  if (original_id == FALSE) {
    df$ID <- 1:nrow(df)
  }

  # sample randomly
  if (n < nrow(df)) {
    df <- dplyr::slice_sample(df, n = n, replace = replace)
  }

  # update n, in case fewer than n patients fit the requirements for include
  n <- nrow(df)

  # duplicate rows before creating time columns
  df <- dplyr::slice(df, rep(1:n, each = n_timepoints))

  # create TIME, EVID, and DV columns
  df$TIME <- rep(times, n)
  df$EVID <- rep(evid, n)
  df$DV   <- 0

  # assign dose amounts
  if (is.numeric(amt)) {
    df <- dplyr::mutate(df, AMT = dplyr::if_else(EVID == 1, amt, 0))
  } else {
    # extract formals from dosing function and apply where EVID = 1
    dose_args <- as.list(dplyr::syms(names(formals(amt))))
    df <- dplyr::mutate(df,
                        AMT = dplyr::if_else(EVID == 1,
                                             do.call(amt, eval(dose_args)), 0))
  }

  # create group column, if specified
  if (is.numeric(group)) {
    df$GR <- group
  }

  # sort by ID and TIME
  df <- dplyr::arrange(df, ID, TIME)

  # compute TAD
  df <- dplyr::group_by(df, ID)
  df <- dplyr::group_modify(~{
          evid <- pull(.x, EVID)
          copy <- .x
          copy$TAD <- 0
          last_dose <- as.double(pull(.x, TIME)[2])
          for(j in seq(nrow(.x))) {
            if(evid[j] == 1) {
              last_dose <- as.double(pull(.x, TIME)[j])
            }
            if(evid[j] == 0) {
              copy[j, "TAD"] <- as.double(pull(.x, TIME)[j]) - last_dose
            }
          }
          copy
        })
    df <- dplyr::ungroup(df)
    df <- dplyr::arrange(df, ID)
    df <- dplyr::relocate(df, ID, TIME, EVID, AMT, DV, TAD)

  return(df)
}
