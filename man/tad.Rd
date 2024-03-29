% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tad.R
\name{tad}
\alias{tad}
\title{Calculate time after dose (TAD)}
\usage{
tad(data, cond = "", expand = FALSE)
}
\arguments{
\item{data}{A data frame or data frame extension in NONMEM format..}

\item{cond}{A filtering condition that specifies for which rows to calculate TAD, as a string (optional).}

\item{expand}{Whether to expand the ADDL column in the result.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} with an appended column, TAD, containing the
time after since the most recent dose for each observation record.
}
\description{
\code{tad()} computes the time after dose, adding a new column to a data frame.
}
\details{
\code{tad()} assumes NONMEM-formatted data. It will expand the ADDL column internally,
but will return the data in its non-expanded form unless \code{expand} is set to TRUE.

Expressions in \code{...} are used to determine under what conditions a
row of \code{data} should count as a "dose" for calculating time after dose. This
is especially useful if there is more than one type of dose event, and TAD
should only apply to one of them. For example, suppose the DV column of
\code{data} contains concentrations of a drug \emph{X}. If a flag column "Y_FLAG"
exists to indicate that a drug \emph{Y} is given, as opposed to \emph{X}, passing
\code{Y_FLAG = 0} to \code{...} will ensure that TAD is only calculated with respect to
drug \emph{X} dosing events.
}
\examples{
# Basic TAD calculation
tad(pk_example)

# compute TAD only for even ID numbers, and return in expanded form
tad(pk_example, "ID \%\% 2 == 0", expand = TRUE)

}
\author{
Sandy Floren
}
