#*********************************************
#*********************************************
#' Simple function for determining whether an element of the data 'y' is of character type.
#'
#' @param y  is an R object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.charTSD_element
#'
is.charTSD_element<-function(y){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Simple function for determining whether an element of the data 'y' is of character type.
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
	# is.charTSD_element("string")
	# is.charTSD_element(3)
	# is.charTSD_element(NA)
	# is.charTSD_element(NULL)
	############ VARIABLES: ############
	# ---y--- is an R object.
	

	##################################################
	##################################################
	if(length(y)==0){
		NA
		}
	else{
		if(is.list(y)){
			ischar=unlist(lapply(y, is.character))
			isnull=unlist(lapply(y, function(z) length(z)==0))
			any(ischar) & all(ischar | isnull)
			}
		else{
			is.character(y)
			}
		}
	##################################################
	##################################################
	}
