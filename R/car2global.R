#*********************************************
#*********************************************
#' Conversions between cartesian and spheircal coordinate systems.
#'
#' \code{car2global} Transforms points in a local coordinate system to the global coordinate system of Earth, given an origin in longitude, latitude and heave decimal values (DD). The output is in the range [-180,180] for longitude (East) and [-90,90] for latitude (North). Circular surface of the Earth is assumed in a neighbourhood around the 'origin'.
#' \code{car2pol} Transformation from cartesian to polar coordinates returned as 'r', 'theta' and 'phi' as in Zwillinger (1985, pp. 297-298) and on http://mathworld.wolfram.com/SphericalCoordinates.html.
#' \code{car2sph} Transformation from cartesian to spherical coordinates returned as 'r', 'theta' and 'phi' as in Zwillinger (1985, pp. 297-298) and on http://mathworld.wolfram.com/SphericalCoordinates.html.
#' \code{global2car} Transforms points in the global coordinate system of Earth, given in longitude, latitude and heave decimal values (DD) in the range [-180,180] for longitude (East) and [-90,90] for latitude (North), to a cartesian coordinate system centered at a reference position, with y-axis pointing North and z-axis pointing vertically outwards from the surface of the Earth. Circular surface of the Earth is assumed in a neighbourhood around the 'origin'.
#' \code{pol2car} Transformation from polar coordinates given as 'r' and 'theta', to cartesian coordinates.
#' \code{sph2car} Transformation from spherical (polar) coordinates given as 'r', 'theta' and 'phi' as in Zwillinger (1985, pp. 297-298) and on http://mathworld.wolfram.com/SphericalCoordinates.html, to cartesian coordinates.
#' \code{radiusEarth} Calculating the radius of Earth for the given latitude value(s).
#' \code{DD2DMS} Simple function converting from decimal degrees to degrees, minutes and seconds.
#' \code{decdeg2degmin} Converts decimal degrees to degrees and decimal minutes.
#' \code{decdeg2degminsec} Converts decimal degrees to degrees, minutes and decimal seconds.
#'
#' @param pos 		A list, matrix or vector of the points to be transformed.
#' @param origin	A vector of length 3 holding the origin of the cartesian coordinate system, given in longitude-latitude-heave decimal values (DD)).
#' @param list.out 	Logical: TRUE if the output should be put in a list.
#' @param format	The return format, either dec = decimal degrees (default); min = degrees and decimal minutes; or sec = degrees, minutes and decimal seconds.
#' @param x			represents the points to transform, structured in one of the 4 following ways:
#' @param y			is a vector of y-values (optional).
#' @param r			represents the points to transform, structured in one of the 4 following ways:
#' @param theta		is a vector of theta-values (optional).
#' @param phi		is a vector of phi-values (optional).
#' @param perm		is TRUE if the input points 'r' are given as a row matrix (see 'r').
#' @param nonneg	is TRUE if negative values of 'theta' should be added 2*pi.
#' @param list.out	is TRUE if the output should be put in a list.
#' @param lon		is the vector of longitude values.
#' @param lat		is the vector of latitude values.
#' @param toaxis	is TRUE if the radius to the axis of Earth is to be given.
#' @param digits	is the number of digits in the output of \code{DD2DMS}.
#' @param decdeg	Positions given as a two (or three in the case that heave is given) column matrix of longitude and latitude in decimal degrees.
#'
#' @return Position matrix in the requested format.
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname car2global
#'
car2global <- function(pos, origin=c(0,0,0), list.out=FALSE, format=c("dec", "min", "sec")){
	
	############### LOG: ###############
	# Start: 2010-02-14 - Clean version.
	# Last: 2010-06-09 - Removed data.frame output reducing CPU time to 40 %.
	
	##### Preparation #####
	# Support for list, matrix and vector input for 'pos':
	# List:
	if(is.list(pos)){
		namespos=names(pos)=tolower(names(pos))
		# 'origin' given in the list:
		if(!any(is.null(pos$lon0),is.null(pos$lat0))){
			origin=c(pos$lon0[1],pos$lat0[1],0)
		}
		
		hitspsx=grep("psx",namespos)
		hitspsy=grep("psy",namespos)
		hitspsz=grep("psz",namespos)
		# If non-unique names are present in the list:
		if(any(length(hitspsx)>1,length(hitspsy)>1,length(hitspsz)>1)){
			warning(paste("More than one set of 'psx*', 'psy*' and 'psz*' values. Selecting '",namespos[hitspsx[1]],"', '",namespos[hitspsy[1]],"' and '",namespos[hitspsz[1]],"'",sep=""))
		}
		
		if(any(length(hitspsx)>0,length(hitspsy)>0,length(hitspsz)>0)){
			if(length(hitspsx)==0){
				posx=0
			}
			else{
				posx=pos[[namespos[hitspsx[1]]]]
			}
			if(length(hitspsy)==0){
				posy=0
			}
			else{
				posy=pos[[namespos[hitspsy[1]]]]
			}
			if(length(hitspsz)==0){
				posz=0
			}
			else{
				posz=pos[[namespos[hitspsz[1]]]]
			}
			pos=cbind(posx,posy,posz)
		}
		else if(any(!is.null(pos$x),!is.null(pos$y),!is.null(pos$z))){
			if(is.null(pos$x)){
				pos$x=0
			}
			if(is.null(pos$y)){
				pos$y=0
			}
			if(is.null(pos$z)){
				pos$z=0
			}
			pos=cbind(pos$x,pos$y,pos$z)
		}
		# If names for longitude, latitude and heave (optional) are not given, [[1]] is interpreted as 'longitude', [[2]] is interpreted as 'latitude' and [[3]] is if present interpreted as 'heave'.
		else if(length(pos)>2){
			warning("First three list elements used as 'x', 'y' and 'z'")
			pos=cbind(pos[[1]],pos[[2]],pos[[3]])
		}
		else if(length(pos)>1){
			warning("First two list elements used as 'x' and 'y'")
			pos=cbind(pos[[1]],pos[[2]],0)
		}
		else if(length(pos)>0){
			warning("First list element used as 'x'")
			pos=cbind(pos[[1]],0)
		}
		else{
			warning("No postitions given")
			return(NULL)
		}
	}
	dimpos=dim(pos)
	# Matrix:
	if(length(dimpos)==2 && ncol(pos)<3){
		pos=cbind(pos,zeros(nrow(pos),3-ncol(pos)))
	}
	# Vector:
	else if(is.null(dimpos) && length(pos) %in% 1:3){
		pos=t(c(pos,double(3-length(pos))))
	}
	else if(length(dimpos)!=2){
		return(NULL)
	}
	# 'origin' must be a vector of length 3, or an integer specifying which global position to regard as the origin:
	lorigin=length(origin)
	# If origin is an integer in the range [1,nrow(pos)], the point pos[origin] is regarded as the 'origin':
	origin=as.numeric(origin)
	if(lorigin==2){
		origin=c(origin,0)
	}
	else if(lorigin<2){
		stop("'origin' of the global coordinate system must be given as a vector of 2 or more elements")
	}
	
	
	##### Execution and output #####
	# The positions from the origin divided by the radius of Earth at the latitude of origin:
	pos[,1] = origin[1] + pos[,1]/radiusEarth(origin[2],toaxis=TRUE)/pi*180
	pos[,2] = origin[2] + pos[,2]/radiusEarth(origin[2])/pi*180
	pos[,3] = pos[,3] + origin[3]
	colnames(pos) = c("lon","lat","heave")
	
	if("min"==format[1]){
		pos = decdeg2degmin(pos)
	}
	else if("sec"==format[1]){
		pos = decdeg2degminsec(pos)
	}
	
	# If list output is required:
	if(list.out){
		list(x=pos[,1], y=pos[,2], z=pos[,3])
	}
	else{
		pos
	}
}
#'
#' @export
#' @rdname car2global
#'
car2pol <- function(x,y=NULL,perm=FALSE,nonneg=FALSE,list.out=FALSE){
	
	############### LOG: ###############
	# Start: 2008-03-11 - Finished.
	# Update:  2009-03-08 - Changed input to more robust.
	# Update:  2009-07-28 - Removed input variable 'drop.out'. Added support for list input and changed interpretation of 'x' and 'y' so that if is.null(y) and 'x' is a vector or a single column matrix, then y=0.
	# Update:  2010-06-02 - Function altered to returning the output at each case of the input. Also added the option 'perm', allowing for row matrices. All in all reducing CPU time to 85 %.
	# Update:  2010-06-09 - Two options added: 'nonneg' and 'list.out' (see VARIABLES). CPU time reduction to 85 %.
	# Last:  2010-08-27 - Replaced data frame output by list output.
	
	##### Preparation, execution and output #####
	# List input for 'x':
	if(is.list(x)){
		names(x)=tolower(names(x))
		if(!is.null(x$x) && !is.null(x$y)){
			out=cbind(r=c(sqrt(x$x^2+x$y^2)), theta=c(atan2(x$y,x$x)))
		}
		else{
			out=cbind(r=c(sqrt(x[[1]]^2+x[[2]]^2)), theta=c(atan2(x[[2]],x[[1]])))
		}
	}
	# Array input for 'x':
	else if(is.null(y)){
		dimx=dim(x)
		if(length(dimx)==2){
			if(perm){
				if(dimx[1]<2){
					# Add zeros for the 'y' values:
					x=rbind(x,0)
				}
				out=rbind(r=sqrt(x[1,]^2+x[2,]^2), theta=atan2(x[2,],x[1,]))
			}
			else{
				if(dimx[2]<2){
					# Add zeros for the 'y' values:
					x=cbind(x,0)
				}
				out=cbind(r=c(sqrt(x[,1]^2+x[,2]^2)), theta=c(atan2(x[,2],x[,1])))
			}
		}
		else if(is.null(dimx)){
			if(length(x)<2){
				# Add zeros for the 'y' values:
				x=c(x,0)
			}
			out=c(r=sqrt(x[1]^2+x[2]^2), theta=atan2(x[2],x[1]))
		}
		else{
			stop("Invalid input")
		}
	}
	# Individual inputs:
	else{
		out=cbind(r=c(sqrt(x^2+y^2)), theta=c(atan2(y,x)))
	}
	# If non-negative 'theta' (azimuth angle) is required:
	if(nonneg){
		out[,2]=out[,2]%%(2*pi)
	}
	# If list output is required:
	if(list.out){
		list(r=out[,1], theta=out[,2])
	}
	else{
		out
	}
}
#'
#' @export
#' @rdname car2global
#'
car2sph <- function(x,y=NULL,z=NULL,perm=FALSE,nonneg=FALSE,list.out=FALSE){
	
	############### LOG: ###############
	# Start: 2008-03-11 - Finished.
	# Update:  2009-03-08 - Changed input to more robust.
	# Update:  2009-07-28 - Removed input variable 'drop.out'. Added support for list input and changed interpretation of 'x', 'y' and 'z' so that if is.null(y) or is.null(z) and 'x' is a vector or a matrix of less than 3 columns, then y=0 and/or z=0.
	# Update:  2010-06-02 - Function altered to returning the output at each case of the input. Also added the option 'perm', allowing for row matrices. All in all reducing CPU time to 90 %.
	# Update:  2010-06-09 - Two options added: 'nonneg' and 'list.out' (see VARIABLES). Also 'r' defined prior to all assignments of 'out'. CPU time reduction to 65 %.
	# Last:  2010-08-27 - Replaced data frame output by list output.

	##### Preparation, execution and output #####
	# Function to ensure non-NA values for the point (0,0,0):
	acos_noNA=function(y,x){
		out=acos(y/x)
		out[x==0]=0
		out
	}
	
	# List input for 'x':
	if(is.list(x)){
		names(x)=tolower(names(x))
		if(!is.null(x$x) && !is.null(x$y) && !is.null(x$z)){
			out=sqrt(x$x^2+x$y^2+x$z^2)
			out=cbind(r=c(out), theta=c(atan2(x$y,x$x)), phi=c(acos_noNA(x$z,out)))
		}
		else{
			out=sqrt(x[[1]]^2+x[[2]]^2+x[[3]]^2)
			out=cbind(r=c(out), theta=c(atan2(x[[2]],x[[1]])), phi=c(acos_noNA(x[[3]],out)))
		}
	}
	# Array input for 'x':
	else if(is.null(y) || is.null(z)){
		dimx=dim(x)
		if(length(dimx)==2){
			if(perm){
				if(dimx[1]<3){
					# Add zeros for the 'y' and/or 'z' values:
					xrest=double(dimx[2]*(3-dimx[1]))
					dim(xrest)=c(dimx[2],3-dimx[1])
					x=rbind(x,xrest)
				}
				out=sqrt(x[1,]^2+x[2,]^2+x[3,]^2)
				out=rbind(r=out, theta=atan2(x[2,],x[1,]), phi=acos_noNA(x[3,],out))
			}
			else{
				if(dimx[2]<3){
					# Add zeros for the 'y' and/or 'z' values:
					xrest=double(dimx[1]*(3-dimx[2]))
					dim(xrest)=c(dimx[1],3-dimx[2])
					x=cbind(x,xrest)
				}
				out=sqrt(x[,1]^2+x[,2]^2+x[,3]^2)
				out=cbind(r=c(out), theta=c(atan2(x[,2],x[,1])), phi=c(acos_noNA(x[,3],out)))
			}
		}
		else if(is.null(dimx)){
			if(length(x)<3){
				# Add zeros for the 'y' and/or 'z' values:
				xrest=double(3-length(x))
				x=c(x,xrest)
			}
			out=sqrt(x[1]^2+x[2]^2+x[3]^2)
			out=c(r=out, theta=atan2(x[2],x[1]), phi=c(acos_noNA(x[3],out)))
		}
		else{
			stop("Invalid input")
		}
	}
	# Individual inputs:
	else{
		out=sqrt(x^2+y^2+z^2)
		out=cbind(r=c(out), theta=c(atan2(y,x)), phi=c(acos_noNA(z,out)))
	}
	# If non-negative 'theta' (azimuth angle) is required:
	if(nonneg){
		out[,2]=out[,2]%%(2*pi)
	}
	# If list output is required:
	if(list.out){
		list(r=out[,1], theta=out[,2], phi=out[,3])
	}
	else{
		out
	}
}
#'
#' @export
#' @rdname car2global
#'
global2car <- function(pos, origin=1, list.out=FALSE){
	
	############### LOG: ###############
	# Start: 2009-07-23 - Clean version.
	# Update: 2010-02-20 - Added support for TSD input.
	# Last: 2010-06-09 - Removed data.frame output reducing CPU time to 75 %.
	
	##### Preparation #####
	# Support for list, matrix and vector input for 'pos':
	# List:
	if(is.list(pos)){
		namespos=names(pos)=tolower(names(pos))
		# 'origin' given in the list:
		if(!any(is.null(pos$lon0),is.null(pos$lat0))){
			origin=c(pos$lon0[1],pos$lat0[1],0)
		}
		
		# 'pos' given in the list:
		hitslon=grep("lon",namespos)
		hitslat=grep("lat",namespos)
		# If non-unique names are present in the list:
		if(any(length(hitslon)>1,length(hitslat)>1)){
			warning(paste("More than one set of 'lon*' and 'lat*' values. Selecting '",namespos[hitslon[1]],"' and '",namespos[hitslat[1]],"'",sep=""))
		}
		if(all(length(hitslon)>0,length(hitslat)>0)){
			if(!is.null(pos$psz)){
				pos=cbind(pos[[namespos[hitslon[1]]]],pos[[namespos[hitslat[1]]]],pos$psz)
			}
			else if(!is.null(pos$heav)){
				pos=cbind(pos[[namespos[hitslon[1]]]],pos[[namespos[hitslat[1]]]],pos$heav)
			}
			# Heave need not be given defaulting to 0:
			else{
				pos=cbind(pos[[namespos[hitslon[1]]]],pos[[namespos[hitslat[1]]]],0)
			}
		}
		# If names for longitude, latitude and heave (optional) are not given, [[1]] is interpreted as 'longitude', [[2]] is interpreted as 'latitude' and [[3]] is if present interpreted as 'heave'.
		else if(length(pos)>2){
			warning("First three list elements used as global coordinates")
			pos=cbind(pos[[1]],pos[[2]],pos[[3]])
		}
		else if(length(pos)>1){
			warning("First two list elements used as global coordinates")
			pos=cbind(pos[[1]],pos[[2]],0)
		}
		else{
			stop("Longitude AND latitude must be given (heave may be defaulted to 0)")
		}
	}
	dimpos=dim(pos)
	# Matrix:
	if(length(dimpos)==2 && ncol(pos)<3){
		pos=cbind(pos,zeros(nrow(pos),3-ncol(pos)))
	}
	# Vector:
	else if(is.null(dimpos) && length(pos) %in% 1:3){
		pos=t(c(pos,double(3-length(pos))))
	}
	else if(length(dimpos)!=2){
		return(NULL)
	}
	# If origin is an integer in the range [1,nrow(pos)], the point pos[origin] is regarded as the 'origin':
	if(is.list(origin)){
		if(all(!is.null(origin$lon),!is.null(origin$lat))){
			origin=c(origin$lon,origin$lat)
		}
		else{
			origin=c(origin[[1]],origin[[2]])
		}
	}
	origin=as.numeric(origin)
	# 'origin' must be a vector of length 3, or an integer specifying which global position to regard as the origin:
	lorigin=length(origin)
	if(lorigin==2){
		origin=c(origin,0)
	}
	else if(lorigin==1 && origin %in% 1:nrow(pos)){
		origin=pos[origin,]
	}
	else if(lorigin<2){
		warning("'origin' defaulted to the first position")
		origin=pos[1,]
	}
		
		
	##### Execution and output #####
	# The difference between 'pos' and 'origin' transformed to radians is mutiplied by the circumference of the Earth zonal in the x-direction and meridional in the y-direction. The z-diretion is already in meters:
	pos[,1]=(pos[,1]-origin[1])*pi/180*radiusEarth(pos[,2],toaxis=TRUE)
	pos[,2]=(pos[,2]-origin[2])*pi/180*radiusEarth(pos[,2])
	pos[,3]=pos[,3]-origin[3]
	colnames(pos)=c("x","y","z")
	# If list output is required:
	if(list.out){
		list(x=pos[,1], y=pos[,2], z=pos[,3])
	}
	else{
		pos
	}
}
#'
#' @export
#' @rdname car2global
#'
pol2car <- function(r,theta=NULL,perm=FALSE,list.out=FALSE){
	
	############### LOG: ###############
	# Start: 2008-03-11 - Finished.
	# Update:  2009-03-08 - Changed input to more robust.
	# Update:  2009-07-28 - Removed input variable 'drop.out'. Added support for list input and changed interpretation of 'r' and 'theta' so that if is.null(theta) and 'r' is a vector or a single column matrix, then theta=0.
	# Update:  2010-06-02 - Function altered to returning the output at each case of the input. Also added the option 'perm', allowing for row matrices. All in all reducing CPU time to 70 %.
	# Update:  2010-06-09 - One option added: 'list.out' (see VARIABLES). CPU time reduction to 100 %.
	# Last:  2010-08-27 - Replaced data frame output by list output.
	
	##### Preparation, execution and output #####
	# List input for 'r':
	if(is.list(r)){
		names(r)=tolower(names(r))
		if(!is.null(r$r) && !is.null(r$theta)){
			out=cbind(x=c(r$r)*cos(c(r$theta)), y=c(r$r)*sin(c(r$theta)))
		}
		else{
			out=cbind(x=c(r[[1]])*cos(c(r[[2]])), y=c(r[[1]])*sin(c(r[[2]])))
		}
	}
	# Array input for 'r':
	else if(is.null(theta)){
		dimr=dim(r)
		if(length(dimr)==2){
			if(perm){
				if(dimr[1]<2){
					# Add zeros for the 'theta' values:
					r=rbind(r,0)
				}
				out=rbind(x=r[1,]*cos(r[2,]), y=r[1,]*sin(r[2,]))
			}
			else{
				if(dimr[2]<2){
					# Add zeros for the 'theta' values:
					r=cbind(r,0)
				}
				out=cbind(x=r[,1]*cos(r[,2]), y=r[,1]*sin(r[,2]))
			}
		}
		else if(is.null(dimr)){
			if(length(r)<2){
				# Add zeros for the 'theta' and/or 'phi' values:
				r=c(r,0)
			}
			out=c(x=r[1]*cos(r[2]), y=r[1]*sin(r[2]))
		}
		else{
			stop("Invalid input")
		}
	}
	# Individual inputs:
	else{
		out=cbind(x=r*cos(theta), y=r*sin(theta))
	}
	# If list output is required:
	if(list.out){
		list(x=out[,1],y=out[,2])
	}
	else{
		out
	}
}
#'
#' @export
#' @rdname car2global
#'
sph2car <- function(r, theta=NULL, phi=NULL, perm=FALSE, list.out=FALSE){
	
	############### LOG: ###############
	# Start: 2008-03-11 - Finished.
	# Update:  2009-03-08 - Changed input to more robust.
	# Update:  2009-07-28 - Removed input variable 'drop.out'. Added support for list input and changed interpretation of 'r', 'rheta' and 'phi' so that if is.null(theta) or is.null(phi) and 'r' is a vector or a matrix of less than 3 columns, then theta=0 and/or phi=0.
	# Update:  2010-06-02 - Function altered to returning the output at each case of the input. Also added the option 'perm', allowing for row matrices. All in all reducing CPU time to 75 %.
	# Update:  2010-06-09 - One option added: 'list.out' (see VARIABLES). Also 'out' defined prior to all assignments of 'out'. CPU time reduction to 80 %.
	# Last:  2010-08-27 - Replaced data frame output by list output.
	
	##### Preparation, execution and output #####
	# List input for 'r':
	if(is.list(r)){
		names(r) <- tolower(names(r))
		if(!is.null(r$r) && !is.null(r$theta) && !is.null(r$phi)){
			out <- c(r$r)*sin(c(r$phi))
			out <- cbind(x=out*cos(c(r$theta)), y=out*sin(c(r$theta)), z=c(r$r)*cos(c(r$phi)))
		}
		else{
			out <- c(r[[1]])*sin(c(r[[3]]))
			out <- cbind(x=out*cos(c(r[[2]])), y=out*sin(c(r[[2]])), z=c(r[[1]])*cos(c(r[[3]])))
		}
	}
	# Array input for 'r':
	else if(is.null(theta) || is.null(phi)){
		dimr <- dim(r)
		if(length(dimr)==2){
			if(perm){
				if(dimr[1]<3){
					# Add zeros for the 'theta' and/or 'phi' values:
					rrest <- double(dimr[2]*(3-dimr[1]))
					dim(rrest) <- c(dimr[2],3-dimr[1])
					r <- rbind(r,rrest)
				}
				out <- r[1,]*sin(r[3,])
				out <- rbind(x=out*cos(r[2,]), y=out*sin(r[2,]), z=r[1,]*cos(r[3,]))
			}
			else{
				if(dimr[2]<3){
					# Add zeros for the 'theta' and/or 'phi' values:
					rrest <- double(dimr[1]*(3-dimr[2]))
					dim(rrest) <- c(dimr[1],3-dimr[2])
					r <- cbind(r,rrest)
				}
				out <- r[,1]*sin(r[,3])
				out <- cbind(x=out*cos(r[,2]), y=out*sin(r[,2]), z=r[,1]*cos(r[,3]))
			}
		}
		else if(is.null(dimr)){
			if(length(r)<3){
				# Add zeros for the 'theta' and/or 'phi' values:
				rrest <- double(3-length(r))
				r <- c(r,rrest)
			}
			out <- r[1]*sin(r[3])
			out <- c(x=out*cos(r[2]), y=out*sin(r[2]), z=r[1]*cos(r[3]))
		}
		else{
			stop("Invalid input")
		}
	}
	# Individual inputs:
	else{
		out <- r*sin(phi)
		out <- cbind(x=out*cos(theta), y=out*sin(theta), z=r*cos(phi))
	}
	# If list output is required:
	if(list.out){
		list(x=out[,1],y=out[,2],z=out[,3])
	}
	else{
		out
	}
}
#'
#' @export
#' @rdname car2global
#'
radiusEarth <- function(lat, toaxis=FALSE){
	
	############### LOG: ###############
	# Start: 2008-02-25 - Clean version.
	# Last:  2009-08-18 - Simplified.
	
	##### Preparation #####
	# From wikipedia through "http://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf":
	# Equatorial radius:
	a=6378137.0
	# Polar radius:
	b=6356752.3
	# Transform input latitude to radians:
	lat=lat/180*pi
	
	
	##### Execution and output #####
	# Numeric exentricity:
	epsilon<-sqrt(a^2-b^2)/a
	# Radius given 'lat':
	r=sqrt(b^2/(1-epsilon^2*(cos(lat))^2))
	if(toaxis){
		r*cos(lat)
	}
	else{
		r
	}
}
#'
#' @export
#' @rdname car2global
#'
DD2DMS <- function(lon=NULL,lat=NULL,digits=2){
		
	############### LOG: ###############
	# Start: 2013-06-20 - Clean version.

	##### Preparation #####
	# Longitude:
	east <- lon>0
	lon <- abs(lon)
	D_lon <- floor(lon)
	M_lon <- floor((lon-D_lon)*60)
	S_lon <- round((lon-D_lon-M_lon/60)*3600, digits=digits)
	# Latitude:
	north <- lat>0
	lat <- abs(lat)
	D_lat <- floor(lat)
	M_lat <- floor((lat-D_lat)*60)
	S_lat <- round((lat-D_lat-M_lat/60)*3600, digits=digits)
	
	
	##### Execution and output #####
	#if(numt>1){
	#	paste("Lon (first): ", expression(degree), D_lon, "  ", M_lon, "' ", S_lon, "'' ", c("W","E")[east+1], ",  Lat (first): ", D_lat, "  ", M_lat, "' ", S_lat, "'' ", c("S","N")[north+1],sep="",collapse="")
	#}
	#else{
	paste0("Lon: ", D_lon, "  ", M_lon, "' ", S_lon, "'' ", c("W","E")[east+1], ",  Lat: ", D_lat, "  ", M_lat, "' ", S_lat, "'' ", c("S","N")[north+1], collapse="")
	#}
}
#'
#' @export
#' @rdname car2global
#' 
decdeg2degmin <- function(decdeg, digits=3){
	if(length(dim(decdeg))==0){
		dim(decdeg) = c(1, length(decdeg))
	}
	londeg = floor(decdeg[,1])
	lonmin = round(60*(decdeg[,1]-londeg), digits=digits)
	latdeg = floor(decdeg[,2])
	latmin = round(60*(decdeg[,2]-latdeg), digits=digits)
	out = cbind(londeg=londeg, lonmin=lonmin, latdeg=latdeg, latmin=latmin)
	if(ncol(decdeg)>2){
		cbind(out, heave=decdeg[,3])
	}
	out
}
#'
#' @export
#' @rdname car2global
#' 
decdeg2degminsec <- function(decdeg, digits=3){
	if(length(dim(decdeg))==0){
		dim(decdeg) = c(1, length(decdeg))
	}
	londeg = floor(decdeg[,1])
	lonmin = 60*(decdeg[,1]-londeg)
	lonsec = round(60*(lonmin-floor(lonmin)), digits=digits)
	lonmin = floor(lonmin)
	latdeg = floor(decdeg[,2])
	latmin = 60*(decdeg[,2]-latdeg)
	latsec = round(60*(latmin-floor(latmin)), digits=digits)
	latmin = floor(latmin)
	out = cbind(londeg=londeg, lonmin=lonmin, lonsec=lonsec, latdeg=latdeg, latmin=latmin, latsec=latsec)
	if(ncol(decdeg)>2){
		cbind(out, heave=decdeg[,3])
	}
	out
}
