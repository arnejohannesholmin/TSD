#*********************************************
#*********************************************
#' Transforms minutes to an array of days, hours and minutes.
#'
#' @param m  is the input minutes.
#' @param digits  is the number of digits of the "seconds" part of the output.
#' @param clock.out  is TRUE if the time excluding the days are to be returned as a string vector of elements HH:MM:SS.FFF (two digits).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname m2dhm
#'
m2dhm<-function(m,digits=3,clock.out=FALSE,names=c("d","h","m"),strip=FALSE,drop=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-07-22 - Finished.
	########### DESCRIPTION: ###########
	# Transforms minutes to an array of days, hours and minutes.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---m--- is the input minutes.
	# ---digits--- is the number of digits of the "seconds" part of the output.
	# ---clock.out--- is TRUE if the time excluding the days are to be returned as a string vector of elements HH:MM:SS.FFF (two digits).
	
	
	##################################################
	##################################################
	# Constants:
	ld=60*24
	lh=60
	# Calculation:
	d=floor(m/ld)
	m=m%%ld
	h=floor(m/lh)
	m=m%%lh
	if(clock.out){
		z=c("","0")
		paste(z[(h<10)+1],h,":",z[(m<10)+1],m,sep="")
		}
	else{
		out=cbind(d,h,m=round(m,digits=digits))
		colnames(out)=names
		if(strip){
			n0=which(apply(out,2,function(x) !all(x==0)))[1]
			out[,n0:4,drop=drop]
			}
		else{
			out
			}
		}
	##################################################
	##################################################
	}
