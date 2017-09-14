#*********************************************
#*********************************************
#' Removing NAs, returning a vector. Not used since na.omit() serves the same purpose.
#'
#' @param x  Input object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname rm.na
#'
rm.na<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-11-13 - Clean version.
	########### DESCRIPTION: ###########
	# Removing NAs, returning a vector. Not used since na.omit() serves the same purpose.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- Input object.
	
	
	##################################################
	##################################################
	x[!is.na(x)]
	##################################################
	##################################################
	}
