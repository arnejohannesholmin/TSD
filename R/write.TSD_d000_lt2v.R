#*********************************************
#*********************************************
#' List representation of dimension data as a function of time steps -> vector representation.
#'
#' @param d000l  is a list of dimension information, as returnerd from write.TSD_get_d000_lv().
#' @param numt  is the number of time steps.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_d000_lt2v
#'
write.TSD_d000_lt2v<-function(d000l, numt){
	
	if(numt==0){
		return(0)
	}
	# Return a vector of one 0 for each time step if no dimension data is present in the input:
	else if(length(d000l$dim)==0){
		return(double(numt))
	}
	ndims <- lapply(d000l$dim, function(y) if(is.list(y)) sapply(y, length) else length(y))
	narrays <- unlist(lapply(ndims, function(y) length(y[sum(y)!=0])))
	
	# Return a list of the following data combined by 'c' at each time step: (1) narrays: Number of arrays with dimension, (2) ndims: Number of dimensions of each variable, (3) d000l$var: The indices of the variables, (4) lapply(d000l$dim, unlist): The dimensions unlisted:
	Map(c, narrays, ndims, d000l$var, lapply(d000l$dim, unlist))
	#
	#
	#out <- NULL
	#for(i in 1:numt){
	#	if(narrays[i]==0){
	#		out <- c(out, 0)
	#		}
	#	else{
	#		out <- c(out, c(unlist(narrays[i])), c(unlist(ndims[i])), c(unlist(d000l$var[i])), c(unlist(d000l$dim[i])))
	#		}
	#	}
	#out
}
