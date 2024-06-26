% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.TSD_modifyd000.R
\name{write.TSD_modifyd000}
\alias{write.TSD_modifyd000}
\title{When 'x' is given with variables in a dissimilar order than an existing file to which the data should be appended, modifyd000() alters the dimension information to match the existing file.}
\usage{
write.TSD_modifyd000(y, lablPresent)
}
\arguments{
\item{y}{is a vector of dimension information stored as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'..}

\item{lablPresent}{is a vector of the labels present in the existing file.}
}
\description{
When 'x' is given with variables in a dissimilar order than an existing file to which the data should be appended, modifyd000() alters the dimension information to match the existing file.
}
\examples{
\dontrun{}

}
