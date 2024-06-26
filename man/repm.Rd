% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repm.R
\name{repm}
\alias{repm}
\title{Matrix generalization of rep. If 'x' is a vector, the ordinary rep() is used.}
\usage{
repm(x, times = 1, byrow = FALSE, length.out = NULL)
}
\arguments{
\item{x}{is a matrix or vector.}

\item{times}{is the times to repeat the matrix/vector.}

\item{byrow}{is TRUE if the matrix is to be repeated vertically, and FALSE if horizontally.}

\item{length.out}{is the number of columns of the output matrix if byrow==FALSE, the number of rows of the output matrix if byrow==TRUE, and the length of the output vector if the input is a vector.}
}
\description{
Matrix generalization of rep. If 'x' is a vector, the ordinary rep() is used.
}
\examples{
\dontrun{}

}
