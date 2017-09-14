#*********************************************
#*********************************************
#' A function for extracting the first and last value of 'indt'.
#'
#' @param labl  is a vector of the TSD labels of the file.
#' @param con  is a connection to a binary file, as returned by file(x,"rb").
#' @param index  is a matrix of the positions in the type of the variables and time steps.
#' @param valid_datatypes  are a vector of data types read by read.TSD().
#' @param datasizes  are the sizes in bytes of the data types in 'valid_datatypes'.
#' @param endian  is the endianness of the file, changed if the float is not read properly.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_getindtFirstLast
#'
read.TSD_getindtFirstLast<-function(labl, con, index, dtyp_present, valid_datatypes=c("character", "int", "int", "int", "double", "double"), datasizes=c(1, 1, 2, 4, 4, 8), endian=.Platform$endian){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# A function for extracting the first and last value of 'indt'.
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
	# ---labl--- is a vector of the TSD labels of the file.
	# ---con--- is a connection to a binary file, as returned by file(x,"rb").
	# ---index--- is a matrix of the positions in the type of the variables and time steps.
	# ---valid_datatypes--- are a vector of data types read by read.TSD().
	# ---datasizes--- are the sizes in bytes of the data types in 'valid_datatypes'.
	# ---endian--- is the endianness of the file, changed if the float is not read properly.
	

	##################################################
	##################################################
	# If 'indt' is requested, a file of many pings may cause reading 'indt' to be a slow process. Thus a test is made for continnuity of 'indt', in which case only the first and last value is read, and the sequence between these is returned as 'indt':
	indtat=which("indt"==labl)
	# Get the first 'indt':
	seek(con, index[1, indtat], origin="start")
	indt_first=readBin(con, valid_datatypes[dtyp_present][indtat], n=1, size=datasizes[dtyp_present][indtat], endian=endian, signed=TRUE)
	# Get the last 'indt':
	seek(con, index[nrow(index), indtat], origin="start")
	indt_last=readBin(con, valid_datatypes[dtyp_present][indtat], n=1, size=datasizes[dtyp_present][indtat], endian=endian, signed=TRUE)
	# If the first and the last value of the indt variable span a sequence of step 1, we automatically have the entire sequence:
	if(length(indt_first)>0 && length(indt_last)>0 && (indt_last-indt_first+1) == nrow(index)){
		indt_con=seq(indt_first, indt_last)
		}
	else{
		indt_con=NULL
		}
	indt_con
	##################################################
	##################################################
	}
