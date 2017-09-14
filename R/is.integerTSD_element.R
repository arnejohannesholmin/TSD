#*********************************************
#*********************************************
#' Simple function for determining whether an element of the data 'y' is of integer type.
#'
#' @param y  is an R object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.integerTSD_element
#'
is.integerTSD_element<-function(y){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Simple function for determining whether an element of the data 'y' is of integer type.
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
	# is.integerTSD_element(3L)
	# is.integerTSD_element(3)
	# is.integerTSD_element(NA)
	# is.integerTSD_element(list(NULL, 3L))
	############ VARIABLES: ############
	# ---y--- is an R object.
	

	##################################################
	##################################################
	if(length(y)==0){
		NA
		}
	else{
		if(is.list(y)){
			isinteger=unlist(lapply(y, is.integer))
			isnull=unlist(lapply(y, function(z) length(z)==0))
			any(isinteger) & all(isinteger | isnull)
			}
		else{
			is.integer(y)
			}
		}
	##################################################
	##################################################
	}
