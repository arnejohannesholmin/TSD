#*********************************************
#*********************************************
#' Returning TRUE at all even or odd values.
#'
#' @param x  is the R-object to be checked for even or odd values.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname even
#'
even <- function(x){
	# Start: 2008-11-14 - Clean version.
	!as.logical(x%%2)
}
#'
#' @export
#' @rdname even
#'
odd <- function(x){
	# Start: 2008-11-14 - Clean version.
	as.logical(x%%2)
}
