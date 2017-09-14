#*********************************************
#*********************************************
#' Run WinPath(), paste in a Windows file path, hit Enter, and get a valid path out (changing \ to /).
#'
#' @return
#'
#' @export
#'
WinPath <- function(){
	x <- readline()
	gsub("\\\\", "/", x)
##################################################
##################################################
}
