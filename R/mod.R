#*********************************************
#*********************************************
#' (Internal) Returns the modulus of x by y, but keeping y when x<percent><percent>y returns 0.
#'
#' @param x,y	The input vectors.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname mod
#' 
mod <- function(x, y){
	(x-1) %% y + 1
	}
