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
	}
