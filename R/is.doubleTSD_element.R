#*********************************************
#*********************************************
#' Simple function for determining whether an element of the data 'y' is too large to be represented as float with presicion for whole numbers.
#'
#' @param y  is an R object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.doubleTSD_element
#'
is.doubleTSD_element<-function(y){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Simple function for determining whether an element of the data 'y' is too large to be represented as float with presicion for whole numbers.
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
	# is.doubleTSD_element(3e13)
	# is.doubleTSD_element(3L)
	# is.doubleTSD_element(NA)
	# is.doubleTSD_element(list(NULL, 3e13))
	############ VARIABLES: ############
	# ---y--- is an R object.
	

	##################################################
	##################################################
	if(length(y)==0){
		FALSE
		}
	else{
		# Maximum integer presicion that a float can hold:
		fun = function(z, limit = 2^24){
			is.numeric(z) && ((min(z, na.rm=TRUE) < -limit)  ||  (max(z, na.rm=TRUE) > limit))
			}
		if(is.list(y)){
			suppressWarnings(any(unlist(lapply(y, fun))))
			}
		else{
			suppressWarnings(fun(y))
			}
		}
	##################################################
	##################################################
	}
