#*********************************************
#*********************************************
#' Vector representation of dimension data -> list representation as a function of variables.
#'
#' @param d000v  is the dimension vector given as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'.
#' @param conname  is the file path of the connection from which the dimension data was read. Only used in the error message.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_d000_v2lv
#'
read.TSD_d000_v2lv<-function(d000v, conname=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Vector representation of dimension data -> list representation as a function of variables.
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
	# ---d000v--- is the dimension vector given as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the indices for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p'.
	# ---conname--- is the file path of the connection from which the dimension data was read. Only used in the error message.
	

	##################################################
	##################################################
	##### Preparation #####
	# Length of the vector:
	ldata <- length(d000v)
	# Initialization:
	add <- 0
	i <- 1
	indx <- vector("list", 0)
	dims <- vector("list", 0)
	
	##### Execution and output #####
	# Loop to be ended if the end of the vector 'data' is reached:
	while(ldata>add){
		# Read the number of variables holding dimensions at the current time step:
		narrays <- d000v[add + 1]
		add <- add + 1
		# Only read dimension data for the current time step if narrays>0:
		if(narrays>0){
			if(narrays>1e6){
				stop(paste("Corrupted file: ", conname))
				}
			# Read the number of dimensions for the variables at the current time step:
			ndims <- d000v[add + 1:narrays]
			add <- add + narrays
			is0 <- ndims==0
			# Break while loop if data is to short:
			if( ldata < (add + narrays + sum(ndims)) ){
				break
				}
			# Read indexes and dimensions:
			indx[i] <- list(d000v[add + 1:narrays])
			add <- add + narrays
			
			temp <- vector("list", narrays)
			temp[!is0] <- split(d000v[add + 1:sum(ndims)], rep(1:narrays, ndims))
			#dims[[i]] <- split(d000v[add + 1:sum(ndims)], rep(1:narrays, ndims))
			dims[[i]] <- temp
			add <- add + sum(ndims)
			}
		i <- i + 1
		}
	# Return a list of empty elements if no dimension data is given in the input:
	if(length(indx)==0){
		return(list(var=NULL, indx=vector("list", 0), dims=vector("list", 0)))
		}
	# Prepare output so that the lists are of variables, not time steps:
	# A vector of the numbers of the variables for which dimension information is given for at least one time step:
	var <- sort(unique(unlist(indx)))
	# 'time' is a vector of the time steps associated with the elements of the dimension information list 'dims':
	time <- rep(seq_along(indx), sapply(indx, length))
	# 'unlistindx' is a vector of the variable indexes given in 'indx', used for the reorganization of the lists 'indx' and 'dims':
	unlistindx <- unlist(indx, recursive=FALSE)
	# Create the "transposed" list of 'indx', which are the time steps for which dimension information is given for each variable affected:
	indx <- split(time, unlistindx)
	# Create the "transposed" list of 'dims', which are the dimensions information associated with 'indx':
	dims <- split(unlist(dims, recursive=FALSE), unlistindx)
	# Output: 
	list(var=var, indx=indx, dims=dims)
	##################################################
	##################################################
	}
