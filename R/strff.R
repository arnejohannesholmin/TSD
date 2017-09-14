#*********************************************
#*********************************************
#' Returns TRUE if any of the strings of 'first' occur as the first characters of the strings of 'str'.
#'
#' @param first  is the string holding the 'n' characters to find as starting characters in 'str'. Only the first element of 'first' is used.
#' @param str  is string in which to find 'first' as starting characters.
#' @param n  is the number of leading characters, defaulted to the length of 'first'.
#' @param ignore.case  is FALSE for case sensitivity.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname strff
#'
strff<-function(first,str,n=NULL,ignore.case=TRUE){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-08-06 - Clean version.
	# Update: 2013-02-28 - Change in the default value of 'n'.
	# Update: 2013-03-18 - Changed name from 'strcomp' to 'strfind', and reordered parameters so that the pattern is first, and then the string to find 'first' in. Also, the parameter 'case' was changed to 'ignore.case', with the oposite meaning.
	# Last: 2013-09-09 - Changed in output to return a logical vector of the same length as 'first', instead of applying any() to the output.
	########### DESCRIPTION: ###########
	# Returns TRUE if any of the strings of 'first' occur as the first characters of the strings of 'str'.
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---first--- is the string holding the 'n' characters to find as starting characters in 'str'. Only the first element of 'first' is used.
	# ---str--- is string in which to find 'first' as starting characters.
	# ---n--- is the number of leading characters, defaulted to the length of 'first'.
	# ---ignore.case--- is FALSE for case sensitivity.
	
	
	##################################################
	##################################################
	# If 'n' is given, subset the strings in 'first':
	if(length(n)>0){
		first=substr(first,1,n)
		}
	# Compare the strings:
	if(ignore.case){
		charmatch(tolower(first),tolower(str),nomatch=0)>0
		}
	else{
		charmatch(first,str,nomatch=0)>0
		}
	##################################################
	##################################################
	}

