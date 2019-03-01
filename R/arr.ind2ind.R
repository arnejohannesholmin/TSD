#*********************************************
#*********************************************
#' Transforms array indexes to vector indexes. For indexes out of bounds NA is returned. The function is transformed from the function which().
#'
#' @param arr.ind  is either a list of subscripts, as typed into [], or an array indexes, to be transformed to vector indexes.
#' @param shape  is the dimension of the array.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname arr.ind2ind
#'
arr.ind2ind <- function(arr.ind, shape){
	
	############### LOG: ###############
	# Start: 2009-05-18 - Function transformed from the function which().
	# Update: 2009-07-27 - Added NA returned for the subscripts that are out of bounds. Added support for decimal input in 'arr.ind'. Fixed bug concerning vector input.
	# Last: 2010-05-09 - Added support for 'arr.ind' list input.
	
	##### Preparation #####
	# 2018-10-31: Removed the allow.outside option, and hard coded it. allow.outside = TRUE has neveer been used.
	allow.outside <- FALSE
	# If the input array indices are empty, return:
	if(length(arr.ind)==0){
		return()
	}
	# Number 'nd' of target dimensions:
	nd <- length(shape)
	# If 'arr.ind' is given as a list, the array indexes are expanded from the list:
	if(is.list(arr.ind)){
		arr.ind <- expand.grid(arr.ind)
	}
	# Round of arr.ind:
	#arr.ind=floor(arr.ind)
	# Dimension of 'arr.ind':
	dima <- dim(arr.ind)
	# Support for input vector for a single point:
	if(is.null(dima)){
		if(length(arr.ind) != nd){
			stop("If 'arr.ind' is a vector 1 index will be returned, in which case the length of 'arrind' must match the number of dimensions, i.e., the length of 'shape'.")
		}
		#arr.ind <- rep(arr.ind, length.out=nd)
		dim(arr.ind) <- c(1, nd)
		dima <- c(1, nd)
	}
	if(dima[2] != nd){
		stop("The lengths of the inputs must agree")
	}
	
	
	##### Execution and output #####
	if(!allow.outside){
		invalid <- double(dima[1])
	}
	prodlist <- c(1, cumprod(shape[-nd]))
	out <- double(dima[1]) + 1
	for(i in seq_len(nd)){
		#invalid=invalid + (arr.ind[,i]<1) + (arr.ind[,i]>shape[i]) # The following saves time:
		# Record those indices that are outside of the shape, and insert NAs at the end:
		if(!allow.outside){
			invalid <- invalid | (arr.ind[,i] < 1)
			invalid <- invalid | (arr.ind[,i] > shape[i])
		}
		out <- out + prodlist[i] * (arr.ind[,i] - 1)
	}
	if(!allow.outside){
		out[invalid > 0] <- NA
	}
	return(out)
}
