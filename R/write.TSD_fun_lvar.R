#*********************************************
#*********************************************
#' Function extracting the lengths of the variables, if not specified (a matrix of dimension [numt, nvar]).
#'
#' @param y  is an R object accepted by write.TSD().
#' @param numt  is the number of time steps.
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname write.TSD_fun_lvar
#'
write.TSD_fun_lvar<-function(y, numt, ts){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	# Last: 2015-10-01 - Updated to treat stings simply by counting the number of characters (before the split character was included, but in the new version the character variables are collapsed to one sting per time step for each variable before using write.TSD_fun_lvar()).
	########### DESCRIPTION: ###########
	# Function extracting the lengths of the variables, if not specified (a matrix of dimension [numt, nvar]).
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
	# ---y--- is an R object accepted by write.TSD().
	# ---numt--- is the number of time steps.
	# ---ts--- is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts=3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts=-2 matrices are not regarded as having time along the second dimension.
 	

	##################################################
	##################################################
	# Extract the lengths if the variable is a list:
	if(is.list(y)){
		if(any(sapply(y, is.character))){
			lvar = unlist(lapply(y, nchar), use.names=FALSE)
			}
		else{
			lvar = unlist(lapply(y, length), use.names=FALSE)
			}
		}
	# Array elements:
	else{
		d = if(any(is.null(y), is.vector(y), is.factor(y))) length(y) else dim(y)
		if(length(d)>0){
			ld = length(d)
			thists = write.TSD_get_ts(d=d, ts=ts, numt=numt, ischar=is.character(y))
			
			# If the last dimension is in 'thists' (which are the dimension indices of the current array that are selected by 'ts') regard this dimension as the time dimension:
			if(ld %in% thists){
				if(is.character(y)){
					lvar = nchar(y)
					}
				else if(ld>1){
					lvar = rep(prod(d[-ld]), d[ld])
					}
				else if(ld==1){
					if(numt==d){
						lvar = double(d) + 1
						}
					else{
						lvar = d
						}
					}
				else{
					0
					}
				}
			# Else write only one time step, including the whole array (since prod() returns 1 for empty sets, include the length(d)>0 condition):
			else{
				if(is.character(y)){
					lvar = sum(nchar(y))
					}
				else{
					lvar = prod(d)
					}
				}
			}
		else{
			lvar = 0
			}
		}
	c(lvar, double(max(0, numt-length(lvar))))
	##################################################
	##################################################
	}
