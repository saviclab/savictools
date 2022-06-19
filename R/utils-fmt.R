#' @title Better number formatting
#' @author Sandy Floren
#'
#' @description
#' Round numbers without truncating trailing zeroes.
#'
#' @param x a numeric vector.
#' @param digits number of digits to round to
#'
#' @rdname formatting
#' @export

round_format <- function (x, digits)
{
  round_lt_0 <- FALSE
  if (digits < 0) {
    fmt <- "%.0f"
    round_lt_0 <- TRUE
  }
  else {
    fmt <- paste0("%.", digits, "f")
  }
  if (round_lt_0) {
    suppressWarnings(sprintf(fmt, round(as.numeric(x), digits)))
  }
  else {
    suppressWarnings(sprintf(fmt, as.numeric(x)))
  }
}



#' @title Round up from 5s

#' @description `rounde()` is a rounding function that rounds up when rounding off a 5,
#' instead of to the nearest even number, which is what `round()` does.
#' @param x A numeric vector
#' @param digits Number of significant significant digits to round to
#'
#' @rdname formatting
#' @export
rounde <- function(x, digits = 0) {
  expo <- 10 ^ digits
  return(ifelse(abs(x * expo) - floor(abs(x * expo)) < 0.5,
                sign(x * expo) * floor(abs(x * expo)),
                sign(x * expo) * (floor(abs(x * expo)) + 1)) / expo)
}

