#*********************************************
#*********************************************
#' Pads numerics with zeros at the beginning.
#'
#' @param x  is a numeric vector.
#' @param n  is the number of characters in the returned strings.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname zeropad
#'
zeropad<-function(x, n=max(nchar(x))){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-02-14 - Clean version.
	########### DESCRIPTION: ###########
	# Pads numerics with zeros at the beginning.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric vector.
	# ---n--- is the number of characters in the returned strings.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	#ncharx=nchar(x)
	#ncharz=n-ncharx
	#nz=max(ncharz)
	
	#z=character(nz+1)
	#for(i in seq_along(z)){
	#	z[i]=paste(double(i-1),collapse="")
	#	}
		
		
	########## Execution and output ##########
	#paste(z[ncharz+1], x, sep="")
	if(length(x)==length(n)){
		sapply(seq_along(x), function(i) formatC(x[i], width=n[i], format="d", flag="0"))
		}
	else{
		formatC(x, width=n, format="d", flag="0")
		}
	##################################################
	##################################################
	}

