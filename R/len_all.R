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
	
	if(is.list(x)){
		lapply(x, length)
		}
	else{
		length(x)
		}
	}
