#*********************************************
#*********************************************
#' Function extracting the dimensions of the variable (possibly list) 'y'.
#'
#' @param y  is an R object accepted by write.TSD().
#' @param numt  is the number of time steps.
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#' @param ind.out Logical: If TRUE output indices instead of the dimensions.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_fun_d000
#'
write.TSD_fun_d000 <- function(y, numt, ts, ind.out=FALSE){
	
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	
	# List elements:
	if(is.list(y)){
		# Empty variables get either 0 or NULL depending on ind.out=F/T:
		if(length(y) == 0){
			if(ind.out){
				return(integer(0))
			}
			else{
				return(list(0))
			}
		}
		# Get the dims of each list element, or a vector of which list elements having dimension depending on ind.out=F/T:
		else{
			if(ind.out){
				return(which(unlist(lapply(y, function(x) length(dim(x))>0), use.names=FALSE)))
			}
			else{
				return(lapply(y, dim))
			}
			#d000 = lapply(y, dim)
			}
		}
	# Array elements:
	else{
		d = if(any(is.null(y), is.vector(y), is.factor(y))) length(y) else dim(y)
		ld = length(d)
		thists = write.TSD_get_ts(d=d, ts=ts, numt=numt, ischar=is.character(y))
		
		# If the last dimension is in 'thists' (which is the dimension indices of the current array that are selected or disselected by 'ts'), or if the last dimension is equal to 'numt', regard this dimension as the time dimension:
		if(length(d) && ((ld %in% thists && ld>1) || (d[ld] == numt))){
			if(ind.out){
				if(length(d[-ld])>1){
					return(seq_len(d[ld]))
				}
				else{
					return(integer(0))
				}
			}
			else{
				return(rep(list(d[-ld]), d[ld]))
			}
			#d000 = rep(list(d[-ld]), d[ld])
			}
		# Else write only one time step, and set the dimension of that time step to the dimension of the array:
		else{
			if(ind.out){
				if(length(d)>1){
					return(1)
				}
				else{
					return(integer(0))
				}
			}
			else{
				return(list(d))
			}
			#d000 = list(d)
			}
		}
	}
