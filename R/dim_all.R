#*********************************************
#*********************************************
#' Returns the dimension of the object 'x'. If 'x' is a list, a list of dimensions is returned. If 'x' is a vector length(x) is returned. If 'x' is NULL, 'null.out' is returned.
#'
#' @param x  is the input object.
#' @param null.out	  is the value to return from NULL.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname dim_all
#'
dim_all<-function(x, null.out=NULL, old=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-15 - Finished.
	# Start: 2012-07-24 - Added support for extracting the dimenstion of list elements down to the 5'th level.
	########### DESCRIPTION: ###########
	# Returns the dimension of the object 'x'. If 'x' is a list, a list of dimensions is returned. If 'x' is a vector length(x) is returned. If 'x' is NULL, 'null.out' is returned.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the input object.
	# ---null.out	--- is the value to return from NULL.
		
	
	##################################################
	##################################################
	##### Preparation ####
	notArray <- function(x){
		is.list(x) && !is.data.frame(x)
	}
	# Function for internal use which returnes the dimension of an array and the length of a vector and 'null.out' from NULL:
	dims.array = function(x,null.out=NULL){
		if(length(x)==0){
			return(null.out)
			}
		else if(is.vector(x) || is.factor(x)){
			return(length(x))
			}
		else if(is.data.frame(x) || is.array(x)){
			return(dim(x))
			}
		}
		
	
	##### Execution and output #####
	# lapply used if 'x' is a list, down to the 5'th level:
	if(notArray(x)){
		if(old){
			# function(y1) if(is.list(y1)) lapply(y1,dims.array) else dims.array(y1)
			# function(y2) if(is.list(y2)) lapply(y2,dims.array) else dims.array(y2)
			# function(y3) if(is.list(y3)) lapply(y3,dims.array) else dims.array(y3)
			# function(y4) if(is.list(y4)) lapply(y4,dims.array) else dims.array(y4)
			# function(y5) if(is.list(y5)) lapply(y5,dims.array) else dims.array(y5)
			lapply(x,  function(y1) if(notArray(y1) ) 
				lapply(y1,  function(y2) if(notArray(y2))
					lapply(y2,  function(y3) if(notArray(y3))
						lapply(y3,  function(y4) if(notArray(y4))
							lapply(y4,  function(y5) if(notArray(y5))
								lapply(y5,  dims.array)
							else dims.array(y5))
						else dims.array(y4))
					else dims.array(y3))
				else dims.array(y2))
			else dims.array(y1))
			}
		# Not good for data.frames:
		else{
			rapply(x,  dims.array, how="replace")
			}
		}
	else{
		dims.array(x, null.out=null.out)
		}
	##################################################
	##################################################
	}
