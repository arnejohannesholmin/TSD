#*********************************************
#*********************************************
#' Expands a list of indexes as given to extract() to actual indexes as input to []. If given as a vector and not a list, 'ind' is assumed to represent the subset along the first dimension. Also if this is the case, and 'ind' is given as a single numeric, the 'ind' farthest values are selected, or the proportion 'ind' is ind<=1. This type of subset selection is not supported if 'ind' is a list. 
#'
#' @param ind  is the index specifyer, given as a list of indexes, where negative represent exclusion, empty list elements, zeros, or FALSE represent no subset, and too high indexes are ignored. If 'ind' has length 0, all possible indices are retuned; if 'ind' is not list, it is transformed to list, and if ind has length shorter than the number of elements in 'dim', it is filled with empty list elements implying that all data are selected along those dimensions. If 'ind' is a single integer, this is interpreted as the number of indexes as counted from the end of the first dimenstion. Logical subscripts are allowed. There are pre-defined index-settings for different acoustical instruments:
#' @param dim  is the dimension of the array to subset.
#' @param drop  is TRUE for returning a vector if a list of only one element (one dimension) is returned.	
#' @param pad  determines at which end of the dimensions to pad with zeros if the length of 'ind' is shorter than the number of dimensions. Using pad="start" applies 'ind' on the last dimensions.	
#' @param msg  is FALSE to not print information about the sampling intervals selected by 'ind'.
#' @param ...  is resent to allow passing variables from other functions to this function.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname ind.expand
#'
ind.expand<-function(ind, dim, drop=FALSE, pad=c("end","start"), msg=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2012-06-21 - Clean version.
	# Update: 2012-09-02 - Added the option 'allow' for allowing 'ind' to be subset if it is too long for the dimension given. Else no subsetting is done.
	# Update: 2013-08-27 - Added support for logical subscripts.
	# Update: 2013-10-05 - Removed 'allow' and added 'drop'.
	# Last: 2014-10-22 - Added 'pad'.
	
	##################################################
	##################################################
	##### Preparation #####
	l<-length(dim)
	# Function for expanding along a single dimension:
	ind.expand.single=function(ind.single,dim.single){
		# Transform logical substcipt to integer:
		if(is.logical(ind.single)){
			ind.single=which(ind.single)
			}
		# If 0 or NULL, return the entire vector:
		if(identical(ind.single,0) || length(ind.single)==0){
			ind.single=seq_len(dim.single)
			}
		# Error similar to the one issued in ordinary subscripts "[]":
		else if(min(ind.single)<0 && max(ind.single)>0){
			stop("only 0's may be mixed with negative subscripts")
			}
		# Discard too high indices:
		else if(max(ind.single)>dim.single){
			ind.single=ind.single[ind.single<=dim.single]
			}
		# Apply negative indices:
		else if(max(ind.single)<=0){
			ind.single=seq_len(dim.single)[ind.single]
			}
		ind.single
		}
	
	
	##### Execution #####
	# Transform 'ind' to a list:
	if(is.list(ind)){
		lind=length(ind)
		if(lind<l){
			if(strff("e",pad[1])){
				ind=c(ind,as.list(double(l-lind)))
				}
			else{
				ind=c(as.list(double(l-lind)),ind)
				}
			}
		else if(lind>l){
			ind=ind[seq_len(l)]
			}
		# All list element that equal 0 or NULL are interpreted as 1:length. All indexes outside of 1:length are discarded:
		for(i in seq_len(l)){
			ind[[i]]=ind.expand.single(ind[[i]],dim[i])
			}
		}
	else{
		if(length(ind)>1){
			ind=ind.expand.single(ind,dim[1])
			}
		else if(length(ind)==1){
			if(ind<(-1)){
				ind=list(seq_len(-ind))
				}
			else if(ind<0){
				ind=round(dim[1]*(-ind))
				if(msg){
					cat("Voxels chosen from ",ind," to the length of the beams (",dim[1],")\n",sep="")
					}
				ind=list(seq(ind,dim[1]))
				}
			else if(ind<=1){
				ind=round(dim[1]*(1-ind))
				if(msg){
					cat("Voxels chosen from ",ind," to the length of the beams (",dim[1],")\n",sep="")
					}
				ind=list(seq(ind,dim[1]))
				}
			else{
				ind=list(seq(dim[1]-ind+1,dim[1]))
				}
			}
		else{
			ind=vector("list",l)
			for(i in seq_len(l)){
				ind[[i]]=ind.expand.single(ind[[i]],dim[i])
				}
			}
		}	
	
	##### Output #####
	if(length(ind)==1 && drop){
		ind[[1]]
		}
	else{
		ind
		}
	##################################################
	##################################################
	}
