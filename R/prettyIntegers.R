#*********************************************
#*********************************************
#' Compressed formating of an integer vector.
#'
#' @param x			An integer vector.
#' @param force		Logical: If TRUE the new string should be returned regardless of whether it has more characters than the original.
#' @param collapse	The string to separate numbers by.
#' @param sep		The string to separate numbers by when printing ranges.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname prettyIntegers
#'
prettyIntegers <- function(x, force=FALSE, collapse=", ", sep="..."){
	
	############### LOG: ###############
	# Start: 2014-01-29 - First version.
	
	lx <- length(x)
	
	if(lx < 3){
		paste(x, collapse=collapse)
		}
	else{
		d <- diff(x)
		dd <- diff(d)
		ddd <- diff(dd)
		# Get runs of equal diffs:
		runs1 <- d[seq_len(lx-2)]==1 & dd==0
		runsLarger <- c(FALSE, d[seq_len(lx-3)]!=1 & dd[seq_len(lx-3)]==0 & ddd==0, FALSE)
		starts1 <- which(diff(runs1)==1) + 1
		ends1 <- which(diff(runs1)==-1) + 2
		if(all(starts1 > ends1)){
			ends1 <- c(ends1, lx)
			starts1 <- c(1, starts1)
		}
		if(length(starts1) > length(ends1)){
			ends1 <- c(ends1, lx)
		}
		if(length(starts1) < length(ends1)){
			starts1 <- c(1, starts1)
		}
		startsLarger <- which(diff(runsLarger)==1)
		endsLarger <- which(diff(runsLarger)==-1) + 2
		if(length(startsLarger) > length(endsLarger)){
			endsLarger <- c(endsLarger, lx)
		}
		if(length(startsLarger) < length(endsLarger)){
			startsLarger <- c(1, startsLarger)
		}
		# Special cases:
		if(all(d==1)){
			starts1 <- 1
			ends1 <- lx
		}
		else if(all(d==d[1])){
			startsLarger <- 1
			endsLarger <- lx
		}
		# Convert to string and replace by sep
		newx <- x
		for(i in seq_along(starts1)){
			newx[seq(starts1[i] + 1, ends1[i] - 1)]  <-  c(sep, character(ends1[i] - starts1[i] - 2))
		}
		for(i in seq_along(startsLarger)){
			if(endsLarger[i] - startsLarger[i] - 3 > 0){
				newx[seq(startsLarger[i] + 2, endsLarger[i] - 1)]  <-  c(sep, character(endsLarger[i] - startsLarger[i] - 3))
			}
		}
		
		for(i in seq_along(ends1)){
			if(ends1[i] < lx){
				newx[ends1[i]]  <-  paste0(newx[ends1[i]], collapse)
			}
		}
		for(i in seq_along(endsLarger)){
			if(endsLarger[i] < lx){
				newx[endsLarger[i]]  <-  paste0(newx[endsLarger[i]], collapse)
			}
		}
		
		
		# Remove empry characters:
		newx <- newx[newx != ""]
		# If more characters are in the new string, return the old one if force==FALSE:
		x <- paste(x, collapse=collapse)
		newx <- paste(newx, collapse=collapse)
		if(nchar(newx) >= nchar(x) && !force){
			x
		}
		else{
			newx
		}
	}
}
