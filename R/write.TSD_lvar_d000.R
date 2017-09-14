#*********************************************
#*********************************************
#' Simple function for extracting the lengths of the dimension variable at each time step.
#'
#' @param d000  is a vector of dimension information stored as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'..
#' @param numt  is the number of time steps.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_lvar_d000
#'
write.TSD_lvar_d000<-function(d000, numt){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Simple function for extracting the lengths of the dimension variable at each time step.
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
	# ---numt--- is the number of time steps.
	

	##################################################
	##################################################
	ind=0
	lvar=NULL
	for(i in 1:numt){
		narray=d000[ind+1]
		if(length(narray)>0){
			if(is.na(narray)){
				stop("Arrays at each time step is not implemented in this version")
				}
			thislvar=1+2*narray+sum(d000[ind+1+seq_len(narray)])
			lvar=c(lvar, thislvar)
			ind=ind+thislvar
			}
		else{
			lvar=c(lvar, 0)
			}
		}
	lvar
	##################################################
	##################################################
	}
