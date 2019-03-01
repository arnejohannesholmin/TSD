#*********************************************
#*********************************************
#' Sets the range of the input object 'x' to [a,b] (stretching or shrinking and displacing 'x').
#' The calculation is done by the following procedure:
#' (x - min(x)) * (b-a)  /  (max(x) - min(x)) + a   =   x * s - min(x) * s + a , s=(b-a)/(max(x)-min(x))
#' 									 = x*s+q , q=a-min(x)*s
#'
#' @param x				The input object. If all elements are equal, all elements are replaced by 'a'.
#' @param a				The lower value of the new range, or the range if length(a)>1. Defaulted to 0.
#' @param b				The upper value of the new range. Defaulted to 1.
#' @param clamp			Either (1) a vector of two element setting the boundaries outside which the data are clamped to the boundary values, (2) a vector of three elements, indicating that the first two elements are in fraction of the data range, or (3) NULL to keep the original data. As an example, if x=c(-12,-4,-3,0,2,7), and clamp=c(-3,3), the 'x' is transformed to x=c(-3,-3,-3,0,2,3) before setting the range.
#' @param check.ends	Logical: If TRUE (default) chech that the end points are inside requested range.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname setrange
#'
setrange<-function(x, a=0, b=1, clamp=NULL, check.ends=TRUE){
	
	############### LOG: ###############
	# Start: 2009-06-09 - Finished.
	# Last: 2014-11-25 - Added clamp.
	
	# Apply the clamping of the data:
	if(length(clamp)==3){
		# Convert clamp from % to values:
		minx = min(x,na.rm=TRUE)
		maxx = max(x,na.rm=TRUE)
		clamp = minx + clamp[1:2]*(maxx-minx)
		}
	if(length(clamp)==2){
		x[x<clamp[1]] = clamp[1]
		x[x>clamp[2]] = clamp[2]
		}
	
	# The hue should move from 'start' to 'end':
	# 'a' can be given as the new range:
	if(length(a)>1){
		b = a[2]
		a = a[1]
		}
	
	minx = min(x,na.rm=TRUE)
	maxx = max(x,na.rm=TRUE)
	# If all elements of 'x' are equal, a vector of 'a' and of the same length as 'x' is returned:
	if(minx==maxx){
		return(double(length(x))+a)
		}
	
	
	s = (b-a)/(maxx-minx)
	q = a-minx*s
	x = x*s+q
	# Assure that the end points are what they should be:
	if(check.ends){
		#x[which.min(x)]=min(a,b)
		#x[which.max(x)]=max(a,b)
		x[x<min(a,b)] = min(a,b)
		x[x>max(a,b)] = max(a,b)
		}
		
	
	x
}
