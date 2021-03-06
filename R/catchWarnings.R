#*********************************************
#*********************************************
#' It returns a 2-element list, the first being the value of the expression given to it and the second being a list of all the warnings. catchWarnings() is based on suppressWarnings() and used similarly. Created by Bill Dunlap , TIBCO Software Inc - Spotfire Division , wdunlap tibco.com.
#'
#' @param expr  is an R-expression to be evaluated.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname catchWarnings
#'
catchWarnings<-function(expr){
	
	localWarnings <- list() 
	value <- withCallingHandlers(expr, 
		warning = function(w){
		localWarnings[[length(localWarnings)+1]] <<- w 
		invokeRestart("muffleWarning") 
		}) 
	list(value=value, warnings=localWarnings) 
}
