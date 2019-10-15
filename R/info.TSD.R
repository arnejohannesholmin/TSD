#*********************************************
#*********************************************
#' Reads the description of four character TSD file variables. 
#'
#' @param labl  is the variable names to get the description of, or NULL to return all variables (default).
#' @param file  is the .dat tile holdeing the descriptions.
#' @param info.out  is TRUE if the information about the variable 'info' should be returned (seldom useful).
#' @param clean  is TRUE if only the information strings should be returned, and not added the variable name in <> at the start of each string.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname info.TSD
#'
info.TSD<-function(labl=NULL, file=file.path(system.file("extdata", package="TSD"), "TSD-format_names.dat"), info.out=FALSE, clean=FALSE){
	
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	# Start: 2015-08-25 - Added read.TSD_description_table().
	
	##### Preparation #####
	# Check for the existence of the table of TSD variable decritpions:
	if(!exists("TSD_description_table")){
		if(file.exists(file)){
			TSD_description_table = read.TSD_description_table(file)
			assign("TSD_description_table", TSD_description_table, envir=.GlobalEnv)
		}
		# Still missing, terminate:
		if(!exists("TSD_description_table")){
			return(NULL)
		}
	}
	if(is.list(labl)){
		labl=names(labl)
	}
	if(info.out && !"info"%in%labl){
		labl=c(labl,"info")
	}
	
		
	##### Execution #####
	# Retrun all variables if labl==NULL (in which case "info" has been added above):
	if(identical(labl,"info")){
		labl=TSD_description_table[, 1]
	}
	
	
	##### Output #####
	if(clean){
		TSD_description_table[match(labl,TSD_description_table[, 1]), ]
	}
	else{
		paste("<",labl,"> ",TSD_description_table[match(labl,TSD_description_table[, 1]), 2],sep="")
	}
}
