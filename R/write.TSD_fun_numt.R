#*********************************************
#*********************************************
#' Function to be used for extracting the number of time steps 'numt' if not given. The function 'fun_numt' is applied to each list element of 'x'. For list variables of 'x' the length is extracted, for matrices the number of columns is extracted and for vectors 'numt' is set to 1.
#'
#' @param y  is an R object accepted by write.TSD().
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_fun_numt
#'
write.TSD_fun_numt<-function(y, ts){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Function to be used for extracting the number of time steps 'numt' if not given. The function 'fun_numt' is applied to each list element of 'x'. For list variables of 'x' the length is extracted, for matrices the number of columns is extracted and for vectors 'numt' is set to 1.
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
	# ---ts--- is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
 	

	##################################################
	##################################################
	if(length(y)==0){
		numt=1
		}
	else if(is.list(y)){
		numt=length(y)
		}
	# Array elements:
	else{
		d = if(any(is.null(y), is.vector(y), is.factor(y))) length(y) else dim(y)
		ld=length(d)
		thists = write.TSD_get_ts(d=d, ts=ts, numt=NULL, ischar=is.character(y))
		
		# If the last dimension is in 'thists' (which is the dimension indices of the current array that is selected or diselected by 'ts') regard this dimension as the time dimension:
		if(ld %in% thists){
			numt=d[ld]
			}
		# Else write only one time step, including the whole array:
		else{
			numt=1
			}
		}
	##################################################
	##################################################
	}
