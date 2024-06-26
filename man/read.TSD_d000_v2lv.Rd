% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.TSD_d000_v2lv.R
\name{read.TSD_d000_v2lv}
\alias{read.TSD_d000_v2lv}
\title{Vector representation of dimension data -> list representation as a function of variables.}
\usage{
read.TSD_d000_v2lv(d000v, conname = NULL)
}
\arguments{
\item{d000v}{is the dimension vector given as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'.}

\item{conname}{is the file path of the connection from which the dimension data was read. Only used in the error message.}
}
\description{
Vector representation of dimension data -> list representation as a function of variables.
}
\examples{
\dontrun{}

}
