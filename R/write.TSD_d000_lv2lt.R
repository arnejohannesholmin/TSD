#*********************************************
#*********************************************
#' List representation as a function of variables -> list representation as a function of time steps.
#'
#' @param d000l  is a list of dimension information, as returnerd from write.TSD_get_d000_lv().
#' @param numt  is the number of time steps.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_d000_lv2lt
#'
write.TSD_d000_lv2lt<-function(d000l, numt){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# List representation as a function of variables -> list representation as a function of time steps.
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
	# ---d000l--- is a list of dimension information, as returnerd from write.TSD_get_d000_lv().
	# ---numt--- is the number of time steps.
	

	##################################################
	##################################################
	if(length(d000l) == 3){
		if(length(d000l$var) == 0){
			return(list(var=list(), dims=list()))
			}
		var = vector("list", numt)
		# 'Uindx' is the time indices at which dimension data are stored for the variables. 'ovar' is the order in which to sort the data, which is the actual transformation from variable sorting to time step sorting:
		Uindx = unlist(d000l$indx)
		ovar = order(Uindx)
		# 'L' is a vector of the numbers of time steps for which dimension data are stored, for each variable:
		L = sapply(d000l$indx, length)
		# 'V' is a vector of the variable indices associated with the time indices in 'Uindx':
		V = rep(d000l$var, L)
		# 'd' is the one step flattended list of dimension data:
		d = unlist(d000l$dims, recursive=FALSE, use.names=FALSE)
		# Restructure the variable indices:
		var[sort(unique(Uindx))] = split(V[ovar], Uindx[ovar])
		# The dimension information list to be returned:
		D = vector("list", numt)
		# Restructure the dimension data:
		D[sort(unique(Uindx))] = split(d[ovar], Uindx[ovar])
		# Output:
		list(var=var, dims=D)
		}
	# Return the input if not given as a list of 3 elements of the proper names:
	else{
		warning("Input must be a list of 3 elements named \"var\", \"indx\" and \"dims\"")
		d000l
		}
	##################################################
	##################################################
	}
