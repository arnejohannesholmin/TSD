#*********************************************
#*********************************************
#' (Internal:) Get the 'ts' object, which defines along which dimension time steps are organized.
#'
#' @param d  is a vector of the dimensions of the array.
#' @param ts  (see write.TSD).
#' @param  numt  is the number of time steps.
#' @param ischar  is TRUE if the variable is character, in which case all string elements are collapsed to one vector if 'numt' is given in write.TSD() and does not fit the length of the string vector.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail
#'
#' @export
#' @rdname write.TSD_get_ts
#'
write.TSD_get_ts <- function(d, ts, numt=NULL, ischar=FALSE){
	ld = length(d)
	if(length(ts)==0){
		if(ischar){
			thists = 1
			}
		else if(length(numt) && tail(d,1)<=numt){
			thists = ld
			}
		else{
			thists = 1
			}
		}
	else if(identical(tolower(substr(ts,1,1)), "l")){
		thists = ld
		}
	else{
		thists = seq_len(ld)[ts]
		}
	# If the last dimension equals 'numt', 'y' is assumed to have time along this dimension:
	if(length(numt) && d[ld]==numt){
		thists = ld
		}
	thists
	}
		
