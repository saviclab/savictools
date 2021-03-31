#!/usr/bin/env Rscript
#' @title nmdb
#' @author Alexander Floren
#'
#' @param modfile
#'
#' @description
#'
#' @usage
#'
#' @returns
#'
#' @examples
#'
#' @export

nmdb <- function(modfile) {
  mod <- read_nm_model(file = modfile)

  mod %>%
    group_by(problem) %>%
    group_walk(~ {
      columns <- filter(.x, subroutine == "inp")$code
      columns <- strsplit(columns, "\\s+")[[1]]

      fname <- filter(.x, subroutine == "dat")$code
      fname <- strsplit(fname, "\\s+")[[1]][1]

      # Test column names for exact equality
      dat <- read.csv(fname)
      if (!identical(columns, colnames(dat))) {
        cat(paste0("Column names in ", fname, " do not match $INPUT in ", modfile, "\n"))
        return()
      }

    })
}

main <- function() {
  args <- commandArgs()
  filename <- args[1]
  nmdb(filename)
}
