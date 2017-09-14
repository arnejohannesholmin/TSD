#*********************************************
#*********************************************
#' Function extracting the dimensions of the variable (possibly list) 'y'.
#'
#' @param y  is an R object accepted by write.TSD().
#' @param numt  is the number of time steps.
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_fun_d000
#'
write.TSD_fun_d000<-function(y, numt, ts){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Function extracting the dimensions of the variable (possibly list) 'y'.
	########## DEPENDENCIES: ###########
	#
	############ DETAILS: ############
	#
	############ VALUE: ############
	#
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	#
	############ EXAMPLES: ############
	#
	############ VARIABLES: ############
	# ---y--- is an R object accepted by write.TSD().
	# ---numt--- is the number of time steps.
	# ---ts--- is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
 	

	##################################################
	##################################################
	# List elements:
	if(is.list(y)){
		if(length(y) == 0){
			list(0)
			}
		else{
			d000 = lapply(y, dim)
			}
		}
	# Array elements:
	else{
		d = if(any(is.null(y), is.vector(y), is.factor(y))) length(y) else dim(y)
		ld = length(d)
		thists = write.TSD_get_ts(d=d, ts=ts, numt=numt, ischar=is.character(y))
		
		# If the last dimension is in 'thists' (which is the dimension indices of the current array that are selected or disselected by 'ts'), or if the last dimension is equal to 'numt', regard this dimension as the time dimension:
		if(length(d) && ((ld %in% thists && ld>1) || (d[ld] == numt))){
			d000 = rep(list(d[-ld]), d[ld])
			}
		# Else write only one time step, and set the dimension of that time step to the dimension of the array:
		else{
			d000 = list(d)
			}
		}
	##################################################
	##################################################
	}
