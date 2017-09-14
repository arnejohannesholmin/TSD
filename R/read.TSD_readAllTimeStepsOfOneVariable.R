#*********************************************
#*********************************************
#' (Internal) Read all time steps of one variable in a TSD file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_readAllTimeStepsOfOneVariable
#' 
read.TSD_readAllTimeStepsOfOneVariable <- function(numtInFile, lvar, var, con, index, dtyp, valid_datatypes, dtyp_present, datasizes, endian, arecomplex){
	
	# Read all time steps:
	t = seq_len(numtInFile)
	out=vector("list", numtInFile)
	
	# For loop through the elements of 't' using the sequences tseq and varseq=seq_along(var):
	for(p in t){
		# Run through the requested variables at this time step:
		if(lvar[t[p], var]!=0){
			# Read from the correct position of the file:
			s=seek(con, index[t[p], var], origin="start")
			if(dtyp[var]=="char"){
				thisout = readChar(con, lvar[t[p], var], useBytes=TRUE)
				}
			else{
				thisout = readBin(con, valid_datatypes[dtyp_present][var], n=lvar[t[p], var], size=datasizes[dtyp_present][var], endian=endian, signed=TRUE)
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
