#*********************************************
#*********************************************
#' Function used for collapsing string vectors, arrays or lists to strings of length 1 at each time step (only used on elements of the data that are recognized as string type).
#'
#' @param y  is a string vector to collapse into a single string with split_char filled in between the vector elements.
#' @param numt  is the number of time steps.
#' @param split_char  is a character used to split strings into vector elemens when restoring the vector.
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_collapse_string
#'
write.TSD_collapse_string<-function(y, numt, split_char="\u001F", ts="last"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	# Last: 2015-03-16 - Fixed bug with collapsing strings, by putting a requirement on the length of string vectors to be equal to the number of time steps in order to write one element per time step.
	########### DESCRIPTION: ###########
	# Function used for collapsing string vectors, arrays or lists to strings of length 1 at each time step (only used on elements of the data that are recognized as string type).
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
	# ---y--- is a string vector to collapse into a single string with split_char filled in between the vector elements.
	# ---numt--- is the number of time steps.
	# ---split_char--- is a character used to split strings into vector elemens when restoring the vector.
	# ---ts--- is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
 	

	##################################################
	##################################################
	addSplit_char = function(x){
		paste(c(x,""), collapse=split_char)
		}
	
	# Each element of a list of strings is considered to be one time step and is collapsed:
	if(is.list(y)){
		unlist(lapply(y, addSplit_char))
		}
	else{
		d = if(any(is.null(y), is.vector(y), is.factor(y))) length(y) else dim(y)
		ld=length(d)
		thists = write.TSD_get_ts(d=d, ts=ts, numt=numt, ischar=is.character(y))
		
		# If the input is a vector and of the same length as the number of time steps, and 1 is in 'thists', it is assumed that there is one element per time step:
		if(ld==1 && prod(d)==numt && 1 %in% thists){
			#addSplit_char(y)
			y
			}
		# If the input is a vector and NOT of the same length as the number of time steps, collapse into one time step:
		else if(ld==1 && prod(d)!=numt){
			addSplit_char(y)
			}
		## If the input is a vector and NOT of the same length as the number of time steps, pad to the number of time steps by empty string (not if numt==1):
		#else if(ld==1 && prod(d)!=numt){
		#	c(sapply(y, addSplit_char), character(numt-d))
		#	}
		# If the last dimension is in 'thists' (which is the dimension indices of the current array that is selected or diselected by 'ts') regard this dimension as the time dimension:
		else if(ld %in% thists){
			apply(y, ld, addSplit_char)
			}
		# Else write only one time step, including the whole array collapsed:
		else{
			addSplit_char(y)
			}
		}
	##################################################
	##################################################
	}
