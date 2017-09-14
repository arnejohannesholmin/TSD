#*********************************************
#*********************************************
#' Returns the lengths of the object 'x'.
#'
#' @param x  is the input object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname len_all
#'
len_all<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-05-09 - Finished.
	########### DESCRIPTION: ###########
	# Returns the lengths of the object 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the input object.
		
	
	##################################################
	##################################################
	if(is.list(x)){
		lapply(x, length)
		}
	else{
		length(x)
		}
	##################################################
	##################################################
	}
