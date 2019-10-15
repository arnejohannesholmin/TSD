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
	}
