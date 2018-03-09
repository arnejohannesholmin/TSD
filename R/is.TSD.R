#*********************************************
#*********************************************
#' Tests whether the file(s) given by 'x' has the Time Step Data format (TSD) by reading the first four bytes as character and comparing to the TSD file header "\%TSD".
#'
#' @param x  is a directory or a vector of file paths.
#' @param recursive  is used in list.files() when 'x' is a directory.
#' @param accept.dir  is TRUE to permit giving 'x' as a directory, which is only used for a special reason in read.TSD().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.TSD
#'
is.TSD<-function(x, recursive=TRUE, accept.dir=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-01-17 - Clean version.
	########### DESCRIPTION: ###########
	# Tests whether the file(s) given by 'x' has the Time Step Data format (TSD) by reading the first four bytes as character and comparing to the TSD file header "%TSD".
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
	#
	############ VARIABLES: ############
	# ---x--- is a directory or a vector of file paths.
	# ---recursive--- is used in list.files() when 'x' is a directory.
	# ---accept.dir--- is TRUE to permit giving 'x' as a directory, which is only used for a special reason in read.TSD().


	##################################################
	##################################################
	##### Preparation #####
	# Get file pahts:
	finfo <- file.info(x)
	if(length(x)==1 && is.na(finfo$isdir)){
		warning("Non-existing path")
		return(FALSE)
	}
	else if(length(x)==1 && !identical(finfo$isdir, FALSE)){
		if(accept.dir){
			x <- list.files(x, recursive=recursive, full.names=TRUE)
		}
		else{
			warning(paste(x, "is a directory"))
			return(FALSE)
		}
	}
	
		
	##### Execution and output #####
	out <- rep(NA, length(x))
	for(i in seq_along(x)){
		first4 <- readChar(x[i], nchars=4, useBytes=TRUE)
		out[i] <- identical(first4, "%TSD")
		#if(!out[i]){
		#	print(first4)
		#}
	}
	names(out) <- basename(x)
	out
	##################################################
	##################################################
}
