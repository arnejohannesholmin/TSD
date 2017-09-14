#*********************************************
#*********************************************
#' Get the number of time steps.
#'
#' @param x  is a list containing the required variables 'dirx' (vector of x-beam-directions), 'diry' (vector of y-beam-directions), 'dirz' (vector of z-beam-directions), and 'esnm' (name of the echosounder/sonar).
#'
#' @return
#'
#' @importFrom stats median
#'
#' @export
#' @rdname numt.TSD
#'
numt.TSD<-function(x){
	median(unlist(lapply(x, length)[labl.TSD("v")]))
	}
