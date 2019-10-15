#*********************************************
#*********************************************
#' Funciton used for merging data stored in a list, where each list element contains data from one file
#'
#' @param x  is a list containing some data that should be merged, and that preferably has equal dimension up to the last one (which might be missing). If not equal in dimension, 'pad' can be set to TRUE to pad with missing values.
#' @param pad  is TRUE to pad with missing values to make the list elements fit in the dimension up to the last dimension.
#' @param add1  Logical: if TRUE, add one empty dimension to all elements of the data.
#' @param split  Logical: if TRUE, split each list element to have last dimenstion equal to 1.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail head
#'
#' @export
#' @rdname mergeListKeepDimensions
#'
mergeListKeepDimensions<-function(x, pad=TRUE, add1=FALSE, split=TRUE){
	
	########## Preparation ##########
	if(!is.list(x)){
		return(x)
		}
	# Get the dimensions of each list element:
	dimx = lapply(x, dim_all)
	ndimx = unlist(lapply(dimx, length))
	# Add empty dimension to all elements:
	if(all(ndimx==0)){
		return(list())
		}
	if(add1){
		if(all(ndimx==ndimx[1])){
			dimx = lapply(dimx, append, 1)
			}
		else{
			dimx[ndimx<max(ndimx)] = lapply(dimx[ndimx<max(ndimx)], append, 1)
			}
		}
	# Update the number of dimensions:
	ndimx = unlist(lapply(dimx, length))
	# Expand the dimensions to match the maximum number of dimensions:
	maxndimx = max(ndimx)
	if(any(ndimx!=maxndimx)){
		dimx = lapply(dimx, function(y) c(y,ones(maxndimx-length(y))))
		}
	ldimx = unlist(lapply(dimx, prod))
	maxdimx = dimx[which.max(ldimx)]
	# If all elements have identical dimensions except possibly the last one, merge the elements:
	firstdimx = matrix(unlist(lapply(dimx, head, maxndimx-1)), ncol=maxndimx-1, byrow=TRUE)
	lastdimx = unlist(lapply(dimx, tail, 1))
	# Get the maximum of ecah of the first dimenstions, and check whether the first-dimensions are equal:
	maxfirstdimx = suppressWarnings(apply(firstdimx, 2, max))
	equalUptoLast = all(t(firstdimx) == maxfirstdimx)
	
	
	########## Execution ##########
	# Pad with NAs if requested, resulting in equal first dimensions:
	if(pad){
		for(i in seq_along(x)){
			temp = NAs(maxfirstdimx, lastdimx[i])
			if(maxndimx==1){
				temp = c(x[[i]])
				}
			else if(maxndimx==2){
				temp[seq_len(firstdimx[i,1]),] = c(x[[i]])
				}
			else if(maxndimx==3){
				temp[seq_len(firstdimx[i,1]),seq_len(firstdimx[i,2]),] = c(x[[i]])
				}
			else if(maxndimx==4){
				temp[seq_len(firstdimx[i,1]),seq_len(firstdimx[i,2]),seq_len(firstdimx[i,3]),] = c(x[[i]])
				}
			else if(maxndimx==5){
				temp[seq_len(firstdimx[i,1]),seq_len(firstdimx[i,2]),seq_len(firstdimx[i,3]),seq_len(firstdimx[i,4]),] = c(x[[i]])
				}
			x[[i]] = temp
			}
		equalUptoLast = TRUE
		}
	# Merge to an array:
 	if(equalUptoLast){
 		x = unlist(x)
 		dim(x) = c(maxfirstdimx, sum(lastdimx))
 		}
	# If split==TRUE, split each list element to have last dimenstion equal to 1 ??????????????:
	else if(split){
		out = vector("list", sum(lastdimx))
		at = 0
		for(i in seq_along(x)){
			for(j in seq_len(lastdimx[i])){
				indices = length(dim_all(x[[i]]))-1
				out[[at+j]] = do.call("[", c(list(x[[i]]), c(as.list(!logical(indices)), list(j))))
				}
			at = at + lastdimx[i]
			}
		x = out
		}
	
	
	########## Output ##########
	x
}
