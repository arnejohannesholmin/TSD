#*********************************************
#*********************************************
#' Updates headers of TSD files for which extra rows of zeros was reserved using the option 'reserve' in write.TSD. If fewer rows of zeros was reserved, than the number of time steps to append to the TSD file, UpdateHeaderTSD() will cause trouble.
#'
#' @param filename  is the name of the TSD file for which the header is to be updated.
#' @param lvar  is the variable lengths vector given as c(lvar_1_1, lvar_1_2, ... , lvar_1_nvar, lvar_2_1, lvar_2_2, ... , lvar_2_nvar, ... , lvar_numt_1, lvar_numt_2, ... , lvar_numt_nvar).
#' @param pos  is the position in bytes from the start of the file, at which the header is to be updated.
#' @param endian  is the endian of the existing TSD file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname UpdateHeaderTSD
#'
UpdateHeaderTSD <- function(filename, lvar, pos, endian = "big"){
	
	# Start: 2010-06-17 - Clean version.
	# Update: 2013-11-11 - Changed to write floats or longs depending on 'lvar_float'.
	# Last: 2014-09-27 - Removed 'lvar_float', for the new format with T000 and R000 in the file header.
	
	# 2014-02-12: It was discovered that tilde expansion is not accepted by the c++ funcitons UpdateHeaderTSD_long():
	filename = path.expand(filename)
	
	nlvar = length(lvar)
	# Swap endinanness if 'endian' is different from the endianness used by the system:
	if(endian != .Platform$endian){
		tempfilename = file.path(Sys.getenv("HOME"), "tempfile_endian_swap_UpdateHeaderTSD")
		writeBin(lvar, con=tempfilename, size=4, endian="swap")
		lvar = readBin(tempfilename, what=double(), n=nlvar, size=4)
		unlink(tempfilename)
	}
		
	
	.C("UpdateHeaderTSD_long", as.character(filename), as.integer(lvar), as.integer(nlvar), 4L, as.integer(pos), PACKAGE="TSD")
	#.Call("UpdateHeaderTSD_long", as.character(filename), as.integer(lvar), as.integer(nlvar), 4L, as.integer(pos), PACKAGE="TSD")
}


