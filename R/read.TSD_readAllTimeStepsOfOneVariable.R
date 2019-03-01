#*********************************************
#*********************************************
#' (Internal) Read all time steps of one variable in a TSD file.
#'
#' @param numtInFile	The number of time steps in the file.
#' @param lvar			The lvar info read from the header of the file.
#' @param labl			The labl info read from the header of the file.
#' @param con			A connection to a binary file, as returned by file(x,"rb").
#' @param index			A matrix of the positions in the type of the variables and time steps.
#' @param dtyp			A matrix of the positions in the type of the variables and time steps.
#' @param datatypes		The data types of each element of 'labl'.
#' @param datasizes		The data sizes of each element of 'labl'.
#' @param endian		The endianness of the file, changed if the float is not read properly.
#' @param arecomplex	Logical vector labeling variables as complex type.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_readAllTimeStepsOfOneVariable
#' 
read.TSD_readAllTimeStepsOfOneVariable <- function(numtInFile, lvar, labl, con, index, dtyp, datatypes, datasizes, endian, arecomplex){
	
	# Read all time steps:
	t = seq_len(numtInFile)
	out=vector("list", numtInFile)
	
	# For loop through the elements of 't' using the sequences tseq and varseq=seq_along(var):
	for(p in t){
		# Run through the requested variables at this time step:
		var <- which("indt"==labl)
		if(lvar[t[p], var]!=0){
			# Read from the correct position of the file:
			s=seek(con, index[t[p], var], origin="start")
			if(dtyp[var]=="char"){
				thisout = readChar(con, lvar[t[p], var], useBytes=TRUE)
				}
			else{
				thisout = readBin(con, datatypes[var], n=lvar[t[p], var], size=datasizes[var], endian=endian, signed=TRUE)
				}
			# Change convert to complex:
			if(arecomplex[var]){
				halflength = length(thisout)/2
				halfind = seq_len(halflength)
				thisout = complex(real=thisout[halfind], imaginary=thisout[halfind+halflength])
				}
			out[[p]] = thisout
			}
		}
	out
	}
