#*********************************************
#*********************************************
#' Splits file paths into parts. If only one file path is given, a vector is returned. If multiple file paths are given in a vector, a list is returned.
#'
#' @param x  is a vector of file paths.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname pathparts
#'
pathparts<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-10-03 - Clean version.
	# Last: 2013-10-09 - Fixed bug on Windows (now comparing old and new 'f').
	########### DESCRIPTION: ###########
	# Splits file paths into parts. If only one file path is given, a vector is returned. If multiple file paths are given in a vector, a list is returned.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a vector of file paths.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	pathparts1=function(f){
		if(is.na(f)){
			return(NA)
			}
		parts=NULL
		oldf=""
		while(oldf!=f){
			oldf=f
			parts=c(basename(f),parts)
			f=dirname(f)
			}
		parts
		}
	
	
	########## Execution and output ##########
	if(length(x)==1){
		pathparts1(x)
		}
	else{
		lapply(x,pathparts1)
		}
	##################################################
	##################################################
	}
