#' @title find_triples
#' @author Sandy Floren
#'
#' @description A general-purpose function to count how many triples of numerics occur
#' in a vector within a fixed interval of one another.
#'
#' `find_triples` will begin a searching for the next triple at the index of the
#' final member of the most recently found triple.
#'
#' @param x a vector of type numeric
#' @param from a numeric lower bound for the length between two observations
#' @param to a numeric upper bound for the length between two observations
#'
#' @usage find_triples(x, from = 4.0, to = 12.0)
#'
#' @returns An integer representing the number of triples.
#'
#' @examples
#'
#' # 1 triple
#'  find_triples(c(0, 4, 12, 16))
#'
#' # No triples
#' find_triples(c(0, 12, 25))
#'
#' # 2 triples
#' find_triples(c(0, 12, 24, 36, 48))
#'
#' @export

find_triples <- function(x, from = 4.0, to = 12.0) {
  length <- length(x)
  count <- 0
  i <- 1

  while(i < length - 1) {
    # get first item of potential triple
    first <- x[i]

    # scan for other items in range of interest
    in_range <- FALSE
    j <- i + 1
    while((x[j] - first) <= to & j < length) {
      if(x[j] >= first + from) {
        in_range <- TRUE
        break
      }
      j <- j + 1
    }

    # check if an item was found in range. If not, start new search at x[i + 1]
    if(!in_range) {
      i <- i + 1
      next
    }

    # now x[j] is the second element of the potential triple
    in_range <- FALSE
    second <- x[j]
    k <- j + 1
    while((x[k] - second) <= to & k <= length) {
      if(x[k] >= second + from) {
        in_range <- TRUE
        break
      }
      k <- k + 1
    }

    # check if an item was found in range. If not, start new search at x[i + 1]
    if(!in_range) {
      i <- i + 1
      next
    }

    # triple found. Increment count and start new search at x[k]
    count <- count + 1
    i <- k
  }
  count
}
