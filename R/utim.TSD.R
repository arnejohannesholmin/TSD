#*********************************************
#*********************************************
#' Transforms time input in TSD format to unix time. If Matlab time 'mtim' is given, it is assumed to be in UTC.
#'
#' \code{utim.TSD} Transforms time input in TSD format to unix time. If Matlab time 'mtim' is given, it is assumed to be in UTC.
#' \code{ftim.TSD} Transforms time input in TSD format to formatted time D=yyyymmdd and T=HHMMSS.FFF in a column matrix or vector.
#' \code{mtim.TSD} Transforms time input in TSD format to UTC Matlab time.
#' \code{utim2mtim} Transforms unix time to Matlab time, which will be in UTC.
#' \code{utim2ftim} Transforms unix time to formatted time D=yyyymmdd and T=HHMMSS.FFF in a column matrix or vector.
#' \code{utim2FILETIME} Transforms UNIX time to Windows FILETIME (the number of 100 nanoseconds since 1 January 1601).
#' \code{ftim2list} Transforms 'ftim' in the TSD file format to a list used by ftim2mtim() and ftim2utim().
#' \code{ftim2mtim} Transforms 'ftim' in the TSD file format to UTC Matlab time.
#' \code{ftim2utim} Transforms 'ftim' in the TSD file format to unix time.
#' \code{mtim2FILETIME} Transforms MATLAB serial date (Number of days since Jesus was born) to Windows FILETIME (the number of 100 nanoseconds since 1 January 1601).
#' \code{mtim2ftim} Transforms unix time to formatted time D=yyyymmdd and T=HHMMSS.FFF in a column matrix or vector.
#' \code{mtim2utim} Transforms Matlab time in UTC to unix time.
#' \code{FILETIME2mtim} Transforms Windows FILETIME to UTC Matlab time.
#' \code{FILETIME2utim} Transforms Windows FILETIME to UNIX time.
#' \code{interpret.mtim} Transforms time given either as MATLAB serial date number 'mtim' (number of days since January 0, 0000), 'utim' (number of seconds elapsed since UTC 00:00, 1970-01-01), or 'ftim' (yyyyddHHMMSS.FFF or yyyymmddSSSSS.FFF) to MATLAB serial date number.
#'
#' @param data  is a list with names as used in the TSD file format (usually from read.TSD()).
#' @param keep.list  is TRUE if 'ftim' is to be returned as a list even though only one vector of 'ftim' is extracted.
#' @param x  are the input time points. For conversions from ftim to other units, POSIXct is supported (such as the value returned from Sys.time()).
#' @param format  is a string specifying the format of the return values. "yyyy" denotes year, "mm" denotes month, "dd" denotes day, "HH" denotes hour, "MM" denotes minutes, "SS" denotes seconds and "FFF" denotes fractional seconds of 'digits' digits. (Similar to the MATLAB function timestr()). Also an index number given by 'indt' can be added in the output, as well as text and separators, such as "yyyy-mm-dd\\nHH:MM:SS.FFF\\nPing: indt", which separates the date, time, and ping number in acoustic data by line spaces.
#' @param digits  is the number of digits to return after seconds.
#' @param indt  is the optional index number to add to the output.
#' @param split  is TRUE if the formated time points are to be returned as a matrix of columns "D" and "T".
#' @param numeric  is TRUE if the time values are to be returned as a numeric column matrix (only in the case split==TRUE).
#' @param xBase  is the base of Windows FILETIME: xBase=unclass(as.POSIXct('1601-1-1', tz="UTC"))[1].
#' @param tz  is the the time zone. Overrides 'xBase' if not given as tz="UTC". See as.POSIXlt().
#' @param ...  arguments passed from or to other functoins.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname utim.TSD
#'
utim.TSD<-function(data, keep.list=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.
	# Update: 2010-03-03 - Altered to fully utilize the functions ftim2mtim, ftim2utim, mtim2ftim, mtim2utim, utim2ftim and utim2mtim.
	# Update: 2010-07-13 - Added support for list input.
	# Update: 2010-10-19 - Added support for multiple elements of identical names in the input.
	# Last: 2015-04-28 - Removed support for 'ctim', which can now be used freely as time information that do not interfer with the time information stored in 'utim', 'mtim', and 'ftim'.

	if(!any(rm.na(names(data)=="utim"))){
		if(any(rm.na(names(data)=="mtim"))){
			mtim <- data[names(data)=="mtim"]
			notEmpty <- unlist(lapply(mtim, length)>0)
			if(any(notEmpty)){
				mtim <- mtim[notEmpty]
			}
			utim <- lapply(mtim, function(x) if(is.list(x)) lapply(x, mtim2utim, ...) else mtim2utim(x, ...))
			if(length(utim)==1 && !keep.list){
				return(utim[[1]])
			}
			else{
				return(utim)
			}
		}
		else if(any(rm.na(names(data)=="ftim"))){
			ftim <- data[names(data)=="ftim"]
			notEmpty <- unlist(lapply(ftim, length)>0)
			if(any(notEmpty)){
				ftim <- ftim[notEmpty]
			}
			utim <- lapply(ftim, function(x) if(is.list(x)) lapply(x, ftim2utim, ...) else ftim2utim(x, ...))
			if(length(utim)==1 && !keep.list){
				return(utim[[1]])
			}
			else{
				return(utim)
			}
		}
	}
	else{
		utim <- data[names(data)=="utim"]
		notEmpty <- unlist(lapply(utim, length)>0)
		if(any(notEmpty)){
			utim <- utim[notEmpty]
		}
		if(length(utim)==1 && !keep.list){
			return(utim[[1]])
		}
		else{
			return(utim)
		}
	}
}
#' \dontrun{}
#'
#' @export
#' @rdname utim.TSD
#'
ftim.TSD<-function(data, keep.list=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.
	# Update: 2010-03-03 - Altered to fully utilize the functions ftim2mtim, ftim2utim, mtim2ftim, mtim2utim, utim2ftim and utim2mtim.
	# Update: 2010-07-13 - Added support for list input.
	# Update: 2010-10-19 - Added support for multiple elements of identical names in the input.
	# Update: 2014-05-27 - Added 'indt'.
	# Last: 2015-04-28 - Removed support for 'ctim', which can now be used freely as time information that do not interfer with the time information stored in 'utim', 'mtim', and 'ftim'.

	if(!any(rm.na(names(data)=="ftim"))){
		if(any(rm.na(names(data)=="utim"))){
			utim=data[names(data)=="utim"]
			notEmpty=unlist(lapply(utim,length)>0)
			if(any(notEmpty)){
				utim=utim[notEmpty]
			}
			ftim=lapply(utim,function(x) if(is.list(x)) lapply(x,utim2ftim,indt=data$indt,...) else utim2ftim(x,indt=data$indt,...))
			if(length(ftim)==1 && !keep.list){
				ftim[[1]]
			}
			else{
				ftim
			}
		}
		else if(any(rm.na(names(data)=="mtim"))){
			mtim=data[names(data)=="mtim"]
			notEmpty=unlist(lapply(mtim,length)>0)
			if(any(notEmpty)){
				mtim=mtim[notEmpty]
			}
			ftim=lapply(mtim,function(x) if(is.list(x)) lapply(x,mtim2ftim,indt=data$indt,...) else mtim2ftim(x,indt=data$indt,...))
			if(length(ftim)==1 && !keep.list){
				ftim[[1]]
			}
			else{
				ftim
			}
		}
	}
	else{
		ftim=data[names(data)=="ftim"]
		notEmpty=unlist(lapply(ftim,length)>0)
		if(any(notEmpty)){
			ftim=ftim[notEmpty]
		}
		if(length(ftim)==1 && !keep.list){
			ftim[[1]]
		}
		else{
			ftim
		}
	}
}
#'
#' @export
#' @rdname utim.TSD
#'
mtim.TSD<-function(data, keep.list=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.
	# Update: 2010-03-03 - Altered to fully utilize the functions ftim2mtim, ftim2utim, mtim2ftim, mtim2utim, utim2ftim and utim2mtim.
	# Update: 2010-07-13 - Added support for list input.
	# Update: 2010-10-19 - Added support for multiple elements of identical names in the input.
	# Last: 2015-04-28 - Removed support for 'ctim', which can now be used freely as time information that do not interfer with the time information stored in 'utim', 'mtim', and 'ftim'.

	if(!any(rm.na(names(data)=="mtim"))){
		if(any(rm.na(names(data)=="utim"))){
			utim=data[names(data)=="utim"]
			notEmpty=unlist(lapply(utim,length)>0)
			if(any(notEmpty)){
				utim=utim[notEmpty]
			}
			mtim=lapply(utim,function(x) if(is.list(x)) lapply(x,utim2mtim,...) else utim2mtim(x,...))
			if(length(mtim)==1 && !keep.list){
				mtim[[1]]
			}
			else{
				mtim
			}
		}
		else if(any(rm.na(names(data)=="ftim"))){
			ftim=data[names(data)=="ftim"]
			notEmpty=unlist(lapply(ftim,length)>0)
			if(any(notEmpty)){
				ftim=ftim[notEmpty]
			}
			mtim=lapply(ftim,function(x) if(is.list(x)) lapply(x,ftim2mtim,...) else ftim2mtim(x,...))
			if(length(mtim)==1 && !keep.list){
				mtim[[1]]
			}
			else{
				mtim
			}
		}
	}
	else{
		mtim=data[names(data)=="mtim"]
		notEmpty=unlist(lapply(mtim,length)>0)
		if(any(notEmpty)){
			mtim=mtim[notEmpty]
		}
		if(length(mtim)==1 && !keep.list){
			mtim[[1]]
		}
		else{
			mtim
		}
	}
}
#'
#' @export
#' @rdname utim.TSD
#'
utim2mtim<-function(x, ...){
	
	############### LOG: ###############
	# Start: 2010-02-15 - Clean version.

	##### Preparation #####
	# Matlab serial date at the start of UNIX/POSIX time:
	days1970=719529
	# The number of seconds in a day:
	nsec=86400
	
	
	##### Execution and output #####
	x/nsec+days1970
}
#'
#' @export
#' @rdname utim.TSD
#'
utim2ftim<-function(x, format="yyyymmddHHMMSS.FFF", digits=3, indt=NULL, split=FALSE, numeric=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.
	# Update: 2010-11-11 - Changed to resemble the function datestr() in MATLAB, taking an arbitrary format argument 'format', including key strings:
	# Last: 2014-05-27 - Added 'indt'.

	##### Preparation #####
	if(length(x)==0){
		return(NULL)
	}
	x = round(x, digits=digits)
	
	
	##### Execution #####
	# Get values:
	FFF = substr(x-floor(x), 3, 3-1+digits)
	# Transform time:
	x = as.character(as.POSIXct(x, "GMT", origin="1970-01-01 00:00:00 GMT"))
	yyyy = substr(x, 1, 4)
	mm = substr(x, 6, 7)
	dd = substr(x, 9, 10)
	HH = substr(x, 12, 13)
	MM = substr(x, 15, 16)
	SS = substr(x, 18, 19)
	allvalues = cbind(yyyy, mm, dd, HH, MM, SS, FFF, if(length(indt)==0) "" else indt)
	
	# Locate key strings:
	atyyyy = gregexpr("yyyy", format)[[1]]
	atmm = gregexpr("mm", format)[[1]]
	atdd = gregexpr("dd", format)[[1]]
	atHH = gregexpr("HH", format)[[1]]
	atMM = gregexpr("MM", format)[[1]]
	atSS = gregexpr("SS", format)[[1]]
	atFFF = gregexpr("FFF", format)[[1]]
	atindt = gregexpr("indt", format)[[1]]
	# 'allat' is a matrix of 3 columns where the first holds the positions of key strings, the second is the corresponding identification values (1: yyyy, 2: mm, 3: dd, 4: HH, 5: MM, 6: SS, 7: FFF, 8: indt) and the third is the lengths of the key strings:
	allat = cbind(unlist(c(atyyyy, atmm, atdd, atHH, atMM, atSS, atFFF, atindt)), rep(1:8, c(length(atyyyy), length(atmm), length(atdd), length(atHH), length(atMM), length(atSS), length(atFFF), length(atindt))))
	allat = cbind(allat, c(4,2,2,2,2,2,3,4)[allat[,2]])
	# Strip 'allat' of missing key strings:
	validat = which(allat[,1]!=-1)
	allat = allat[validat,]
	# Order 'allat' according to apparence in 'format':
	allat = allat[order(allat[,1]),]
	
	# Get separators:
	sepall = NULL
	for(i in seq_len(nrow(allat)-1)){
		sepall = c(sepall, substr(format, allat[i,1]+allat[i,3], allat[i+1,1]-1))
	}
	
	
	##### Output #####
	start = substr(format, 1, allat[1,1]-1)
	end = substring(format, allat[nrow(allat),1]+allat[nrow(allat),3])
	out = paste0(start, allvalues[,allat[1,2]])
	if(split){
		# Return column matrix of the date and time values:
		for(i in seq_along(sepall)){
			out = cbind(out, allvalues[,allat[i+1,2]])
		}
		if(numeric){
			dimout = dim(out)
			out = array(as.numeric(out), dim=dimout)
		}
	}
	else{
		# Return strings of the date and time values:
		for(i in seq_along(sepall)){
			thisval = allvalues[,allat[i+1,2]]
			thissep = rep(sepall[i], length(thisval))
			thissep[nchar(thisval)==0] = ""
			out = paste0(out, thissep, thisval)
		}
	}
	paste0(out, end)
}
#'
#' @export
#' @rdname utim.TSD
#'
utim2FILETIME<-function(x, xBase=-11644473600, tz="UTC"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-10-23 - Clean version.
	########### DESCRIPTION: ###########
	# Transforms UNIX time to Windows FILETIME (the number of 100 nanoseconds since 1 January 1601).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- are the time points given in UNIX time (the number of seconds since 1 January 1970).
	# ---xBase--- is the base of Windows FILETIME: xBase=unclass(as.POSIXct('1601-1-1', tz="UTC"))[1].
	# ---tz--- is the the time zone. Overrides 'xBase' if not given as tz="UTC". See as.POSIXlt().
	
	
	##################################################
	##################################################
	if(!tz=="UTC"){
		xBase=unclass(as.POSIXct('1601-1-1', tz=tz))[1]
	}
	# Convert from seconds to Windows FILETIME and return:
	(x-xBase)*1e7
	##################################################
	##################################################
}
#'
#' @export
#' @rdname utim.TSD
#'
ftim2list<-function(x, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.

	##### Preparation #####
	if(length(x)==0){
		return(NULL)
	}
	# Accept string specifications of the ftim:
	x = format(x, scientific=FALSE)
	x = gsub("[[:punct:]]", "", x)
	x = gsub("[[:space:]]", "", x)
	# Add 000000 at the end, and 20 at the beginning and 000000 at the end if the number of digits is 8 or 6, respectively:
	if(nchar(x[1])==6 && is.numeric(x[1])){
		last=as.numeric(substr(x,1,2))>as.numeric(substr(format(Sys.time(), "%Y"),3,4))
		x=paste0(c("20","19")[last+1],x,"000000")
	}
	else if(nchar(x[1])==8){
		x=paste0(x,"000000")
	}
	ncharx = nchar(x)
	FFF = double(length(x))
	if(any(ncharx>14)){
		larger = ncharx>14
		suppressWarnings(FFF[larger]<-as.numeric(paste0(".",substring(x[larger], 15))))
		suppressWarnings(x<-as.numeric(x))
		x[larger] = x[larger]/10^(ncharx[larger]-14)
	}
	else{
		suppressWarnings(x<-as.numeric(x))
	}
	# 'add' is a logical that is true if the time format is yyyymmddHHMMSS.FFF and false in the case yyyymmddSSSSS.FFF:
	add=x>1e13
	add[is.na(add)]=FALSE
	
	
	##### Execution #####
	# The calculation of 'y', 'm' and 'd':
	c=10^(9+add)
	y=floor(x/c)
	x=x-y*c
	c=10^(7+add)
	m=floor(x/c)
	x=x-m*c
	c=10^(5+add)
	d=floor(x/c)
	# If add==FALSE 'S' is the correct value for the seconds alpsed the given day. If add==TRUE some more calculations must be done:
	S=x-d*c
	if(any(add)){
		c=10^(3+add)
		H=floor(S/c)
		S=S-H*c
		c=10^(1+add)
		M=floor(S/c)
		S=S-M*c + H*3600 + M*60
	}
	# Add the digits of the seconds:
	S = floor(S) + FFF 
	# 'ally' is a year sequence used in the treatment of leap years:
	ally=0:max(y,na.rm=TRUE)
	# 'ly' is a locigal vector giving the leap years, and 'cly' are the number of leap years from year 0:
	ly=ally%%4 == 0 & (ally%%100 != 0 | ally%%400 == 0)
	cly=cumsum(ly)
	# 'cd' are the number of days elapsed through the months the given year:
	cd=c(0,31,59,90,120,151,181,212,243,273,304,334)
	# Subtract 1 from 'd' since the given day does not count:
	d=d+cd[m]-1
	
	
	##### Output #####
	list(y=y, cly=cly, d=d, ly=ly, m=m, S=S)
}
#'
#' @export
#' @rdname utim.TSD
#'
ftim2mtim<-function(x, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.

	# Interpret the ftim:
	temp = ftim2list(x,...)
	
	# Return the Matlab time:
	1 + temp$y*365 + temp$cly[temp$y] + temp$d + temp$ly[temp$y+1]*(temp$m>2) + temp$S/86400
}
#'
#' @export
#' @rdname utim.TSD
#'
ftim2utim<-function(x, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.

	# Interpret the ftim:
	temp = ftim2list(x,...)
	
	# Return the UNIX time:
	(temp$y-1970)*31536000 + (temp$cly[temp$y] + temp$d + temp$ly[temp$y+1]*(temp$m>2) - temp$cly[1970])*86400 + temp$S
}
#'
#' @export
#' @rdname utim.TSD
#'
mtim2FILETIME<-function(x, xBase=-11644473600, tz="UTC"){
	
	############### LOG: ###############
	# Start: 2013-10-23 - Clean version.

	##### Preparation #####
	if(!tz=="UTC"){
		xBase=unclass(as.POSIXct('1601-1-1', tz=tz))[1]
	}
	# The number of seconds in a day:
	nsec=86400
	# Matlab serial date at the start of UNIX/POSIX time:
	days1970=719529
	
	
	##### Execution and output #####
	# Convert from Windows FILETIME and return:
	( (x-days1970)*nsec - xBase) * 1e7
}
#'
#' @export
#' @rdname utim.TSD
#'
mtim2ftim<-function(x, format="yyyymmddHHMMSS.FFF", digits=3, indt=NULL, split=FALSE, numeric=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2010-01-30 - Clean version.

	##### Preparation #####
	if(length(x)==0){
		return(NULL)
	}
	# Transform to seconds (including the leading hour used in Matlab time):
	x=x*86400-86400
	x=round(x,digits=digits)
	
	
	##### Execution and output #####
	# Get values:
	FFF=substr(x-floor(x),3,3-1+digits)
	# Transform time:
	x=as.character(as.POSIXct(x,"GMT",origin="0000-01-01 00:00:00 GMT"))
	yyyy=substr(x,1,4)
	mm=substr(x,6,7)
	dd=substr(x,9,10)
	HH=substr(x,12,13)
	MM=substr(x,15,16)
	SS=substr(x,18,19)
	allvalues=cbind(yyyy,mm,dd,HH,MM,SS,FFF,if(length(indt)==0) "" else indt)
	
	# Locate key strings:
	atyyyy=gregexpr("yyyy",format)[[1]]
	atmm=gregexpr("mm",format)[[1]]
	atdd=gregexpr("dd",format)[[1]]
	atHH=gregexpr("HH",format)[[1]]
	atMM=gregexpr("MM",format)[[1]]
	atSS=gregexpr("SS",format)[[1]]
	atFFF=gregexpr("FFF",format)[[1]]
	atindt=gregexpr("indt",format)[[1]]
	# 'allat' is a matrix of 3 columns where the first holds the positions of key strings, the second is the corresponding identification values (1: yyyy, 2: mm, 3: dd, 4: HH, 5: MM, 6: SS, 7: FFF) and the third is the lengths of the key strings:
	allat=cbind(unlist(c(atyyyy,atmm,atdd,atHH,atMM,atSS,atFFF,atindt)),rep(1:8,c(length(atyyyy),length(atmm),length(atdd),length(atHH),length(atMM),length(atSS),length(atFFF),length(atindt))))
	allat=cbind(allat,c(4,2,2,2,2,2,3,4)[allat[,2]])
	# Strip 'allat' of missing key strings:
	validat=which(allat[,1]!=-1)
	allat=allat[validat,]
	# Order 'allat' according to apparence in 'format':
	allat=allat[order(allat[,1]),]
	
	# Get separators:
	sepall=NULL
	for(i in seq_len(nrow(allat)-1)){
		sepall=c(sepall,substr(format,allat[i,1]+allat[i,3],allat[i+1,1]-1))
	}
	
	
	##### Output #####
	out=allvalues[,allat[1,2]]
	if(split){
		# Return column matrix of the date and time values:
		for(i in seq_along(sepall)){
			out=cbind(out,allvalues[,allat[i+1,2]])
		}
		if(numeric){
			dimout=dim(out)
			out=array(as.numeric(out),dim=dimout)
		}
	}
	else{
		# Return strings of the date and time values:
		for(i in seq_along(sepall)){
			thisval=allvalues[,allat[i+1,2]]
			thissep=rep(sepall[i],length(thisval))
			thissep[nchar(thisval)==0]=""
			out=paste(out,thissep,thisval,sep="")
		}
	}
	out
}
#'
#' @export
#' @rdname utim.TSD
#'
mtim2utim<-function(x, ...){
	
	############### LOG: ###############
	# Start: 2010-03-03 - Clean version.

	##### Preparation #####
	days1970=719529
	# The number of seconds in a day:
	nsec=86400
	
	
	##### Execution #####
	(x-days1970)*nsec
}
#'
#' @export
#' @rdname utim.TSD
#'
FILETIME2mtim<-function(x, xBase=-11644473600, tz="UTC"){
	
	# Start: 2011-11-09 - Clean version.
	##### Preparation #####
	if(!tz=="UTC"){
		xBase=unclass(as.POSIXct('1601-1-1', tz=tz))[1]
		}
	# The number of seconds in a day:
	nsec=86400
	# Matlab serial date at the start of UNIX/POSIX time:
	days1970=719529
	
	# Convert to seconds:
	x.sec=as.numeric(x)*1e-7
	
	
	##### Execution and output #####
	(x.sec+xBase)/nsec+days1970
}
#'
#' @export
#' @rdname utim.TSD
#'
FILETIME2utim<-function(x, xBase=-11644473600, tz="UTC"){

	############### LOG: ###############
	# Start: 2011-11-09 - Clean version.
	##### Preparation #####
	if(!tz=="UTC"){
		xBase=unclass(as.POSIXct('1601-1-1', tz=tz))[1]
		}
	# Convert to seconds:
	x.sec=as.numeric(x)*1e-7
	
	##### Execution and output #####
	x.sec+xBase
}
#'
#' @export
#' @rdname utim.TSD
#'
interpret.mtim<-function(x){
	
	############### LOG: ###############
	# Start: 2013-07-11 - Clean version.

	##### Preparation #####
	ncharx=nchar(x)
	m=ncharx<=6
	u=ncharx %in% c(9,10)
	f=ncharx %in% c(8,13,14)
	

	##### Execution #####
	mtim=NAs(length(x))
	mtim[m]=x[m]
	mtim[u]=utim2mtim(x[u])
	mtim[f]=ftim2mtim(x[f])
	
	
	##### Output #####
	mtim
}
