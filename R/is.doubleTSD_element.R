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
	}
