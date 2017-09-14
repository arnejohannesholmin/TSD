#*********************************************
#*********************************************
#' Converts to complex from data stored with the real in the first half and the imaginary in the second part of the vector 'x'.
#'
#' @param x  is a vector where the first half is the real part and the second half is the imaginary part.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname vecRealImaginary2complex
#'
vecRealImaginary2complex = function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-05-11 - Clean version.
	########### DESCRIPTION: ###########
	# Converts to complex from data stored with the real in the first half and the imaginary in the second part of the vector 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a vector where the first half is the real part and the second half is the imaginary part.
	

	##################################################
	##################################################
	if(length(x)){
		halflength = length(x)/2
		halfind = seq_len(halflength)
		complex(real=x[halfind], imaginary=x[halfind+halflength])
		}
	##################################################
	##################################################
	}
