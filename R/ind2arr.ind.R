#*********************************************
#*********************************************
#' Transforms vector indexes to array indexes. The function is cloned from the function which().
#'
#' @param ind  is the index(es) to be transformed to array indexes.
#' @param shape  is the dimension of the array.
#' @param perm  is the permutation i. e. the order of the dimensions along which the indexes given in 'ind' are organized. If 'shape' is 2:4 and 'perm' is c(3,1,2), the indexes in 'ind' are first organized along the third dimension, then along the first, and finally along the second dimension of a matrix of dimensions 2:4.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname ind2arr.ind
#'
ind2arr.ind<-function(ind,shape=NULL,perm=NULL,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-05-18 - Function cloned from the function which().
	# Last: 2009-05-21 - Added support for 'perm'.
	########### DESCRIPTION: ###########
	# Transforms vector indexes to array indexes. The function is cloned from the function which().
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---ind--- is the index(es) to be transformed to array indexes.
	# ---shape--- is the dimension of the array.
	# ---perm--- is the permutation i. e. the order of the dimensions along which the indexes given in 'ind' are organized. If 'shape' is 2:4 and 'perm' is c(3,1,2), the indexes in 'ind' are first organized along the third dimension, then along the first, and finally along the second dimension of a matrix of dimensions 2:4.
		
	
	##################################################
	##################################################
	# If the input array indices are empty, return:
	if(length(ind)==0){
		return()
		}
	if(!is.null(shape)){
		# Dimnesions of the imaginary matrix:
		m <- length(ind)
		rank <- length(shape)
		rankseq <- 1:rank
		# See description of 'perm':
		if(!all(perm%in%rankseq)){
			stop(paste("The elements of 'perm' need to be in the range [1:",rank,"]"),"")
			}
		if(is.null(perm)){
			perm <- rankseq
			}
		# Remove 1 here and add 1 to the end to avoid zeros in the output:
		ind1 <- ind - 1
		ind <- 1 + ind1%%shape[perm[1]]
		restperm=perm[-1]
		lastdim=shape[perm[1]]
		ind <- matrix(ind, nrow = m, ncol = rank)
		# Adding colnames to 'ind':
		if (rank == 2){
			colnames(ind)=c("row", "col")
			}
		else{
			colnames(ind)=paste("shape", 1:rank, sep = "")
			}
		if (rank >= 2) {
			denom <- 1
			for (i in 2:rank) {
				denom <- denom * shape[perm[i-1]]
				nextd1 <- ind1%/%denom
				# Array indexes are put along the proper dimension as given by 'perm':
				ind[, perm[i]] <- 1 + nextd1%%shape[perm[i]]
				}
			}
		}
	ind
	##################################################
	##################################################
	}
