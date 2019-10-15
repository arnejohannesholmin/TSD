#*********************************************
#*********************************************
#' Function identifying whether a sequence of bytes of length 4*n at the current position is a float or not, and storing the item read.
#'
#' @param con  is a connection to a binary file, as returned by file(x,"rb").
#' @param n  is the number of elements to read.
#' @param endian  is the endianness of the file, changed if the float is not read properly.
#' @param max_var  is the maximum value accepted as float.
#' @param fixed_endian  is TRUE if the endian should not be swaped, and readable set to FALSE for wrong endian.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_isnvar
#'
read.TSD_isnvar<-function(con, n=1L, endian=.Platform$endian, max_var=1e10, fixed_endian=FALSE){
	
	##### Preparation #####
	readable = TRUE
	
	
	##### Execution and output #####
	# Check for probable float (this is a very weak test, since between 5 and 10 percent of strings of characters in the collection of lower and upper characters, and the numbers from 0 to 9, result in TRUE. However, it is unlikely that a TSD file has four characters at the beginning of the file):
	x = readBin(con, what=double(), n=n, size=4, signed=TRUE, endian=endian)
	if(length(x)==0){
		return(list())
		}
	if(any(x!=round(x) | max(x)>=max_var)){
		seek(con, -4*n, origin="current")
		if(fixed_endian){
			readable = FALSE
			}
		else{
			endian="swap"
			x = readBin(con, what=double(), n=n, size=4, signed=TRUE, endian=endian)
			if(x!=round(x) || max(x)>=max_var){
				readable = FALSE
				}	
			}
		}
	list(readable=readable, x=x, endian=endian)
}
