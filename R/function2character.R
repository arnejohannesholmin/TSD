#*********************************************
#*********************************************
#' Converts a function to a string starting with " <- function", which can be sourced by R if pasted a name for the function prior to the string.
#'
#' @param x  is the function.
#' @param fname  is the optional function name.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname function2character
#'
function2character <- function(x, fname=""){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Converts a function to a string starting with " <- function", which can be sourced by R if pasted a name for the function prior to the string.
	########## DEPENDENCIES: ###########
	#
	############ DETAILS: ############
	#
	############ VALUE: ############
	#
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	#
	############ EXAMPLES: ############
	# f <- function(s,r) s+2*r
	# f = function2character(f, "fun")
	# eval(parse(text = f))(13,4)
	############ VARIABLES: ############
	# ---x--- is the function.
	# ---fname--- is the optional function name.
	

	##################################################
	##################################################
	if(is.function(x)){
		#f_name=as.character(substitute(x))
		f_formals <- formals(x)
		f_namesformals <- names(f_formals)
		f_body <- paste(format(body(x)),collapse="\n")
		col1 <- unlist(f_namesformals)
		col3 <- unlist(lapply(f_formals,deparse))
		col2 <- c(""," = ")[(nchar(col3)>0) + 1]
		arguments <- paste(apply(cbind(col1, col2, col3), 1, paste,collapse=""),collapse=", ")
		#out <- paste0(fname, " <- function(", arguments, "){\n", f_body, "\n}",   collapse="")
		out <- paste0(fname, " <- function(", arguments, ")\n", f_body, "\n",   collapse="")
		paste0(out, "\n")
		}
	else{
		x
		}
	##################################################
	##################################################
	}
