#*********************************************
#*********************************************
#' Function for extracting the dimensions of 'x'.
#'
#' @param y  is an R object accepted by write.TSD().
#' @param numt is the number of time steps. NA
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_get_d000_lv
#'
write.TSD_get_d000_lv<-function(y, numt, ts){
	
	# Get the dimensions from the data, one list element for each variable:	
	dimension = lapply(y, write.TSD_fun_d000, numt=numt, ts=ts)
	# Get the indices at which each variable has dimension. This is done by moving through the variables (elements in the list 'dimension') and for each element moving through the list of time steps and finding time steps with more than one dimension:
	### indx = lapply(dimension, function(z) which(unlist(lapply(z, function(w) length(w)>1), use.names=FALSE)))
	indx = lapply(y, write.TSD_fun_d000, numt=numt, ts=ts, ind.out=TRUE)
	# Get the variables that have dimensions at at least one time step:
	var = which(sapply(indx, function(z) length(z)>0))
	# Strip 'dimension' and 'indx' of empty elements:
	dimension = dimension[var]
	indx = indx[var]
	# Then strip each element of 'dimension' of empty elements:
	for(i in seq_along(dimension)){
		thisdimension = dimension[[i]]
		thisindx = indx[[i]]
		dimension[[i]] = thisdimension[thisindx]
		#dimension[[i]] = dimension[[i]][[indx[[dimension[[i]]]]]]
	}
	# Output the list of dimensions:
	list(var=var, indx=indx, dims=dimension)
}
