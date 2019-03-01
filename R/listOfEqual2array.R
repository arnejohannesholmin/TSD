#*********************************************
#*********************************************
#' (Internal) Convert a list of equal lengths to an array.
#'
#' @param x	A list.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname listOfEqual2array
#' 
listOfEqual2array <- function(x){
	dims = lapply(x, dim_all)
	if(identical(rep(dims[[1]],length(dims)), unlist(dims, use.names=FALSE))){
		x = unlist(x, use.names=FALSE)
		thisdim = c(dims[[1]], length(dims))
		dim(x) = thisdim
		}
	x
	}
