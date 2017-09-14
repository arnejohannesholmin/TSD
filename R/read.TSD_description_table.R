#*********************************************
#*********************************************
#' Reads the TSD description table 'x'.
#'
#' @param x  is the file containing a table of two columns named "Label" and "Description", specifying in the first column the TSD names, which are UTF-8 four character names, and in the second column the descriptions of the variables in the first column.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD_description_table
#'
read.TSD_description_table<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-01-17 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the TSD description table 'x'.
	########## DEPENDENCIES: ###########
	#
	############ DETAILS: ############
	#
	############ VALUE: ############
	# info.TSD() returns either a vector of strings with the variable names in <> followed by the descriptions. If clean is TRUE, the variable names are dropped.
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	# See \code{\link{info.TSD}}
	############ EXAMPLES: ############
	#
	############ VARIABLES: ############
	# ---x--- is the file containing a table of two columns named "Label" and "Description", specifying in the first column the TSD names, which are UTF-8 four character names, and in the second column the descriptions of the variables in the first column.
	
	
	##################################################
	##################################################
	TSD_description_table = scan(x, character(), sep="\t", quiet=TRUE)
	# In case scan did not get all the lines and separators:
	TSD_description_table = unlist(strsplit(TSD_description_table, "\n", fixed=TRUE))
	TSD_description_table = unlist(strsplit(TSD_description_table, "\t", fixed=TRUE))
	# Transform into a matrix:
	TSD_description_table = matrix(TSD_description_table, byrow=TRUE,ncol=2)
	colnames(TSD_description_table) = TSD_description_table[1,]
	TSD_description_table = TSD_description_table[-1,,drop=FALSE]
	#as.data.frame(TSD_description_table)
	##################################################
	##################################################
	}
