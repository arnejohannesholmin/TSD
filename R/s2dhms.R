#*********************************************
#*********************************************
#' Transforms seconds to an array of days, hours, minutes and seconds.
#'
#' @param sec  is the input seconds.
#' @param digits  is the number of digits of the "seconds" part of the output.
#' @param clock.out  is TRUE if the time excluding the days are to be returned as a string vector of elements HH:MM:SS.FFF (two digits).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname s2dhms
#'
s2dhms<-function(sec,digits=3,clock.out=FALSE,names=c("d","h","m","s"),strip=FALSE,drop=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-12-16 - Finished.
	# Last: 2010-09-01 - Added 'clock.out' for returning strings of times excluding days.
	########### DESCRIPTION: ###########
	# Transforms seconds to an array of days, hours, minutes and seconds.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---sec--- is the input seconds.
	# ---digits--- is the number of digits of the "seconds" part of the output.
	# ---clock.out--- is TRUE if the time excluding the days are to be returned as a string vector of elements HH:MM:SS.FFF (two digits).
	
	
	##################################################
	##################################################
	# Constants:
	ld=60*60*24
	lh=60*60
	lm=60
	# Calculation:
	d=floor(sec/ld)
	sec=sec%%ld
	h=floor(sec/lh)
	sec=sec%%lh
	m=floor(sec/lm)
	sec=sec%%lm
	if(clock.out){
		z=c("","0")
		paste(z[(h<10)+1],h,":",z[(m<10)+1],m,":",z[(sec<10)+1],round(sec,digits=digits),sep="")
		}
	else{
		out=cbind(d,h,m,s=round(sec,digits=digits))
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
