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
	}
