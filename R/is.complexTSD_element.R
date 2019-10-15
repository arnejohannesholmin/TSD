#*********************************************
#*********************************************
#' Simple function for determining whether an element of the data 'y' is of complex type.
#'
#' @param y  is an R object.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.complexTSD_element
#'
is.complexTSD_element<-function(y){
	
	if(length(y)==0){
		NA
		}
	else{
		if(is.list(y)){
			isinteger=unlist(lapply(y, is.complex))
			isnull=unlist(lapply(y, function(z) length(z)==0))
			any(isinteger) & all(isinteger | isnull)
			}
		else{
			is.complex(y)
			}
		}
	}
