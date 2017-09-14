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
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	# Start: 2015-08-25 - Added read.TSD_description_table().
	########### DESCRIPTION: ###########
	# Reads the description of four character TSD file variables. 
	########## DEPENDENCIES: ###########
	#
	############ DETAILS: ############
	# info.TSD() attempts to locate a table named "TSD_description_table" with two columns, where the first column contains the TSD variable names, and the second the descriptions of the variables. If not present, read.TSD_description_table() tries to read the table from the file given by 'file'. The used should create a file with two columns separated by "\t" with the variable names and desciptions as described above, with a header line "Label\tDescription". To avoid that this file is read each time info.TSD() is run, run the following code at the start of the R-session, where 'file' is the full path to the file of the TSD names and descriptions:
	# TSD_description_table = read.TSD_description_table(file)
	############ VALUE: ############
	# info.TSD() returns either a vector of strings with the variable names in <> followed by the descriptions. If clean is TRUE, the variable names are dropped.
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	# \code{\link{read.TSD}} for reading TSD files.
	############ EXAMPLES: ############
	# # Create a character matrix 'TSD_description_table' if not alreaddy existing:
	# if(!exists("TSD_description_table")) TSD_description_table = cbind(c("var1","var2"), c("First variable","Second variable"))
	# x <- list(var1 = 1:12)
	# info.TSD(x)
	############ VARIABLES: ############
	# ---labl--- is the variable names to get the description of, or NULL to return all variables (default).
	# ---file--- is the .dat tile holdeing the descriptions.
	# ---info.out--- is TRUE if the information about the variable 'info' should be returned (seldom useful).
	# ---clean--- is TRUE if only the information strings should be returned, and not added the variable name in <> at the start of each string.
		
	
	##################################################
	##################################################
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
		TSD_description_table[match(labl,TSD_description_table[, 1]), 2]
		}
	else{
		paste("<",labl,"> ",TSD_description_table[match(labl,TSD_description_table[, 1]), 2],sep="")
		}
	##################################################
	##################################################
	}
