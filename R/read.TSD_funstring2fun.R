#*********************************************
#*********************************************
#' Simple function for extracting the function saved as a string using \code{\link{function2character}}).
#'
#' @param x  is a function string.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' f <- function(s,r) s+2*r
#' f <- function2character(f, "FUNCTION")
#' read.TSD_funstring2fun(f)(13, 4)
#' }
#'
#' @export
#' @rdname read.TSD_funstring2fun
#'
read.TSD_funstring2fun <- function(x){
	
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.

	if(is.character(x) && identical(substr(x, 1, 8), "FUNCTION")){
		f <- tempfile()
		cat(x, file=f)
		source(f)
		unlink(f)
		FUNCTION
		
		# The following code may be an option, but there might be an issue with environment:
		#atFunction <- grep(" function(", x, fixed=TRUE)
		#x <- substring(x, atFunction)
		#eval(parse(text=x))
	}
	else{
		x
	}
}
