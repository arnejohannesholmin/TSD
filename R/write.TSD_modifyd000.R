#*********************************************
#*********************************************
#' When 'x' is given with variables in a dissimilar order than an existing file to which the data should be appended, modifyd000() alters the dimension information to match the existing file.
#'
#' @param d000  is a vector of dimension information stored as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'..
#' @param lablPresent  is a vector of the labels present in the existing file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_modifyd000
#'
write.TSD_modifyd000<-function(y, lablPresent){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# When 'x' is given with variables in a dissimilar order than an existing file to which the data should be appended, modifyd000() alters the dimension information to match the existing file.
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
	# ---d000--- is a vector of dimension information stored as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'..
	# ---lablPresent--- is a vector of the labels present in the existing file.
	

	##################################################
	##################################################
	if(length(y$var)==0){
		return(list())
	}
	else{
		y$var <- lapply(y$var, function(x) lablPresent[match(x, lablPresent[, 1]), 2])
		return(y)
	}
	
	### lx <- length(y)
	### at <- 1
	### while(at<lx){
	### 	narrays <- y[at]
	### 	at <- at + 1
	### 	if(narrays>0){
	### 		ndim <- y[at + seq(0, narrays-1)]
	### 		at <- at + narrays
	### 		# Match against the existing file:
	### 		y[at+seq(0, narrays-1)] <- lablPresent[match(y[at+seq(0, narrays-1)], lablPresent[, 1]), 2]
	### 		at <- at + narrays + sum(ndim)
	### 		}
	### 	}
	### y
	##################################################
	##################################################
	}
