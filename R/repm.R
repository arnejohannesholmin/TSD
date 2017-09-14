#*********************************************
#*********************************************
#' Matrix generalization of rep. If 'x' is a vector, the ordinary rep() is used.
#'
#' @param x  is a matrix or vector.
#' @param times  is the times to repeat the matrix/vector.
#' @param byrow  is TRUE if the matrix is to be repeated vertically, and FALSE if horizontally.
#' @param length.out  is the number of columns of the output matrix if byrow==FALSE, the number of rows of the output matrix if byrow==TRUE, and the length of the output vector if the input is a vector.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname repm
#'
repm<-function(x,times=1,byrow=FALSE,length.out=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-03-01 - Finished.
	# Last: 2009-08-01 - Changed method to a simpler method using rep() and dim() (and t() in the case byrow=TRUE). New method is approximately twice as fast as the old one. Old version is located in the "-unused" directory.
	########### DESCRIPTION: ###########
	# Matrix generalization of rep. If 'x' is a vector, the ordinary rep() is used.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a matrix or vector.
	# ---times--- is the times to repeat the matrix/vector.
	# ---byrow--- is TRUE if the matrix is to be repeated vertically, and FALSE if horizontally.
	# ---length.out--- is the number of columns of the output matrix if byrow==FALSE, the number of rows of the output matrix if byrow==TRUE, and the length of the output vector if the input is a vector.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# If 'x' is a vector, rep() is performed:
	if(is.null(dim(x))){
		return(rep(x,times=times,length.out=length.out))
		}
	# Dimensions of 'x':
	nrowx=nrow(x)
	ncolx=ncol(x)
	
	
	##### Execution and output #####
	# If byrow==FALSE, 'x' is to be extended horizontally:
	if(!byrow){
		# Times and length.out needs to be updated:
		if(is.null(length.out)){
			length.out=times*ncolx
			}
		# Output:
		x=rep(x,length.out=length.out*nrowx)
		dim(x)=c(nrow=nrowx,ncol=length.out)
		x
		}
	# Else if byrow==TRUE, 'x' is to be extended horizontally:
	else{
		# Times and length.out needs to be updated:
		if(is.null(length.out)){
			length.out=times*nrowx
			}
		# Output:
		x=t(x)
		x=c(x)
		x=rep(x,length.out=length.out*ncolx)
		dim(x)=c(ncolx,length.out)
		x=t(x)
		x
		}
	##################################################
	##################################################
	}
