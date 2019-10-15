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
	
	TSD_description_table = scan(x, character(), sep="\t", quiet=TRUE)
	# In case scan did not get all the lines and separators:
	TSD_description_table = unlist(strsplit(TSD_description_table, "\n", fixed=TRUE))
	TSD_description_table = unlist(strsplit(TSD_description_table, "\t", fixed=TRUE))
	# Transform into a matrix:
	TSD_description_table = matrix(TSD_description_table, byrow=TRUE,ncol=2)
	colnames(TSD_description_table) = TSD_description_table[1,]
	TSD_description_table = TSD_description_table[-1,,drop=FALSE]
	#as.data.frame(TSD_description_table)
}
