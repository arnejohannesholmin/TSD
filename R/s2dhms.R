#*********************************************
#*********************************************
#' Transforms seconds to an array of days, hours, minutes and seconds.
#'
#' @param sec			The input seconds.
#' @param m				The input minutes.
#' @param digits		The number of digits of the "seconds" part of the output.
#' @param clock.out		Logical: If TRUE the time excluding the days are to be returned as a string vector of elements HH:MM:SS.FFF (two digits).
#' @param names			Names of the output columns.
#' @param strip,drop	Logical: If TRUE strip off e.g. hours when 0, and in that case drop the output if drop=TRUE.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname s2dhms
#'
s2dhms<-function(sec, digits=3, clock.out=FALSE, names=c("d", "h", "m", "s"), strip=FALSE, drop=FALSE){
	# Constants:
	ld <- 60*60*24
	lh <- 60*60
	lm <- 60
	# Calculation:
	d <- floor(sec/ld)
	sec <- sec%%ld
	h <- floor(sec/lh)
	sec <- sec%%lh
	m <- floor(sec/lm)
	sec <- sec%%lm
	if(clock.out){
		z <- c("","0")
		paste0(z[(h<10)+1], h, ":", z[(m<10)+1], m, ":", z[(sec<10)+1], round(sec,digits=digits))
	}
	else{
		out <- cbind(d, h, m, s=round(sec,digits=digits))
		colnames(out) <- names
		if(strip){
			n0 <- which(apply(out, 2, function(x) !all(x==0)))[1]
			out[, n0:4, drop=drop]
		}
		else{
			out
		}
	}
}
#'
#' @export
#' @rdname s2dhms
#'
m2dhm <- function(m, digits=3, clock.out=FALSE, names=c("d", "h", "m"), strip=FALSE, drop=FALSE){
	# Constants:
	ld <- 60*24
	lh <- 60
	# Calculation:
	d <- floor(m/ld)
	m <- m%%ld
	h <- floor(m/lh)
	m <- m%%lh
	if(clock.out){
		z <- c("","0")
		paste0(z[(h<10)+1], h, ":", z[(m<10)+1], m)
	}
	else{
		out <- cbind(d, h, m=round(m, digits=digits))
		colnames(out) <- names
		if(strip){
			n0 <- which(apply(out, 2, function(x) !all(x==0)))[1]
			out[, n0:3, drop=drop]
		}
		else{
			out
		}
	}
}