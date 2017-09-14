#*********************************************
#*********************************************
#' Execute a Function Call, allowing for further arguments given as ...
#'
#' @param what  either a function or a non-empty character string naming the function to be called.
#' @param args  a list of arguments to the function call. The names attribute of args gives the argument names.
#' @param ...  futher agruments to be merged with 'args'. These arguments have presidence over those in 'args'.
#'
#' @export
#' @rdname do.callUnique
#'
# Function used for calling a function allowing for duplicated arguments:
do.callUnique <- function(what, args, ...){
	args <- c(list(...), args)
	args <- args[!duplicated(names(args))]
	do.call(what=what, args=args)
}
