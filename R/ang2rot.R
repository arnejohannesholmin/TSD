#*********************************************
#*********************************************
#' Transforms from angles in the range [-pi,pi] to angles of rotation, so that when circling around an object, the angle increases beyond this range.
#'
#' @param ang  is a vector af angles as returned from atan2.
#' @param tol  is the angle difference defined as representing change from -pi to pi, or pi to -pi.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname ang2rot
#'
ang2rot <- function(ang, tol=5){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-05-14 - Clean version.
	########### DESCRIPTION: ###########
	# Transforms from angles in the range [-pi,pi] to angles of rotation, so that when circling around an object, the angle increases beyond this range.
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---ang--- is a vector af angles as returned from atan2.
	# ---tol--- is the angle difference defined as representing change from -pi to pi, or pi to -pi.
	
	
	##################################################
	##################################################
	diff_suppressNA <- function(x){
		isna <- is.na(x)
		temp <- c(0, diff(x[!isna]))
		d <- double(length(x))
		d[!isna] <- temp
		d[isna] <- 0
		d[-1]
	}
	# Get the differences of the angles:
	change <- diff_suppressNA(ang)
	
	# Compensate for too large or too small changes:
	up <- which(change>tol)
	down <- which(change<(-tol))
	change[up] <- change[up] - 2*pi
	change[down] <- change[down] + 2*pi
	change[is.na(change)] <- 0

	# Return:
	firstvalid <- min(which(!is.na(ang)))
	c(ang[seq_len(firstvalid-1)], ang[firstvalid], ang[firstvalid] + cumsum(change[seq(firstvalid,length(change))]))	
	##################################################
	##################################################
	}
