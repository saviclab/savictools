% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expwt.R
\name{expwt}
\alias{expwt}
\title{Calculate expected weight for under-5 children.}
\usage{
expwt(data, units = c("months", "years", "weeks"))
}
\arguments{
\item{data}{A data frame or data frame extension with AGE and SEX columns.}

\item{units}{Units for age. Default is "months".}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} with an appended column, EXPWT, containing the expected
weight,
}
\description{
Calculate expected weight in under-5 children. Expected weight
is defined as that weight which would result in a weight-for-age z-score of 0
based on age and sex.
}
\author{
Sandy Floren
}
