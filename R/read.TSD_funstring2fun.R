#*********************************************
#*********************************************
#' Simple function for extracting the function saved as a string using function2character().
#'
#' @param x  is a function string.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_funstring2fun
#'
read.TSD_funstring2fun<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Simple function for extracting the function saved as a string using function2character().
	########## DEPENDENCIES: ###########
	#
	############ DETAILS: ############
	#
	############ VALUE: ############
	#
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	# \code{\link{function2character}}
	############ EXAMPLES: ############
	# f <- function(s,r) s+2*r
	# f = function2character(f, "FUNCTION")
	# read.TSD_funstring2fun(f)(13,4)
	############ VARIABLES: ############
	# ---x--- is a function string.
	

	##################################################
	##################################################
	if(is.character(x) && identical(substr(x, 1, 8), "FUNCTION")){
		f="temporary_write_and_source_function"
		cat(x, file=f)
		source(f)
		unlink(f)
		FUNCTION
		#eval(parse(text=out))
		}
	else{
		x
		}
	##################################################
	##################################################
	}
