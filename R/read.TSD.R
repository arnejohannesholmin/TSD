#*********************************************
#*********************************************
#' Reads a file using the Time Step Data format and outputs as an R-list object where the elements are named according to the four character names provided in the file 'con'. See the documentation on echoIBM for specification of the Time Step Data format (TSD). If reading large files, it can sometimes be faster to read all variables as opposed to some of  the variables. Reading all time steps of a certain segmentation file over 21601 time steps dropped from 26.958 sec to 6.864 sec (system.time(aa_new<-read.TSD("~/Data/echoIBM/SX90_biomassEstimation/Events/SX90_biomassEstimation_E0006_one_school_at_the_time_directional_fish_20logR/SX90/tsd/SX90_biomassEstimation_E0006_T00001_sfnr_0001.seg",t="all", header=FALSE, info=FALSE))).
#'
#' @param con  is the file object or the name of the TSD-file to be read.
#' @param t  is a vector of the time steps to read (in the range [1, numt]). If t=="all", all time steps are read, and if t=="none" only the header is returned, (or if header==FALSE list() is returned).
#' @param var  is a vector of the variables to read, either given as a character vector holding the names of the variables to read, as specified in the TSD-format, or as the number of the variable in 'labl', if 'labl' is known. If var="all", all variables are read. If none of the elements of 'var' are in [1, nvar], or if var=="none", (or if header==FALSE list() is returned). See below for legal variable names:
#' @param dimension  is TRUE to return data with dimension.
#' @param header  is TRUE if the number 'nvar', the lengths 'lvar', the labels 'labl', the types 'dtyp' of the variables, and the number 'numt' of time steps are to be returned (at the end of the output list). If the header (list of variables nvar, labl, lvar, dtyp, numt, endian) of the file is known or has already been read, time can be saved by setting header equal to the existing header, in which case the header will not be read.
#' @param max_var  is the realistic maximum number of variables, used when separating character strings and floats by the internal function read.TSD_isnvar().
#' @param indt  is TRUE if the time points specified in 't' are to be cross referenced to time indexes 'indt' in the data, if present.
#' @param d000.out  is TRUE if the dimension data variable 'd000' should be printed (not returned).
#' @param keep.all  is TRUE if time steps requested through 't' should be returned as empty list elements in the output. This could prevent collapsing variables with equal dimensions for all valid time steps to be collapsed into arrays, as obtained using dimension=TRUE.
#' @param drop.out  is TRUE to drop empty dimensions of arrays and unlist lists of length 1.
#' @param info  is TRUE if the variable 'info' returned from info.TSD() should be added to the output (applied in read.TSD()).
#' @param silent  is FALSE to print progress bar. If the 'silent' should be set to FALSE in general, use options(verbose=TRUE).
#' @param raw  is TRUE to read files quicker by reading the information as raw, and converting to the appropriate data types aferwards, which only applies if the number of time steps exceed 20 and the number of values of the file does not exceed 'raw'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname read.TSD
#'
read.TSD <- function(con, t=1, var="all", dimension=TRUE, header=FALSE, max_var=1e10, indt=FALSE, d000.out=FALSE, keep.all=FALSE, drop.out=TRUE, info=FALSE, silent=!getOption("verbose"), raw=1e3){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-01-17 - Clean version.
	# Update: 2010-01-29 - Updated to support choice of pings and variables. This saves memory for binary files but not for text files.
	# Update: 2010-03-03 - Added support for reading time variables that are not directly accessable in the data, but derived using utim.TSD(), mtim.TSD() or ftim.TSD().
	# Update: 2010-03-03 - Changed the dimensions of 'lvar' (back) to [numt x nvar].
	# Update: 2010-08-30 - Added support for reading TSD files containing dimension data. Dimension data must be specified as an additional variable named "d000" structured as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the index numbers for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p', collapsed into a vector. Also renamed the option 'matrix.out' to 'dimension', incorporating the possibility to add dimension to dimensionless data.
	# Update: 2010-09-13 - Modified the the action of splitting the outputs into time steps, so that empty time steps at the end of the time vector are dropped.
	# Update: 2010-09-21 - Changed from appending to 'out' at each time step, to assigning zeros to 'out' and inserting at the correct positions at each time step. Time usage reduced to 40 % for a .vessel-file of 20000 time steps.
	# Update: 2010-11-15 - Added the option 'indt'.
	# Update: 2010-11-19 - Added the option 'all.list'.
	# Update: 2011-03-09 - Added the option of getting header variables even though header==FALSE.
	# Update: 2011-06-02 - Fixed bug when reading indt: seek(con, index[numt, var[indtat]]) changed to seek(con, index[max(which(lvar[, var[indtat]]>0)), var[indtat]]).
	# Update: 2012-02-11 - Added the option 'clean' for cleaning empty fields in lists and unlisting if only one list element is present.
	# Update: 2012-07-31 - Added the option 'r000' for specifying the number of time steps. This is used when reading 'lvar' and reduces CPU time.
	# Update: 2012-09-11 - Fixed a bug when adding dimension to data when the first time step is not read ('dim(out[[i]][[j]]) = out$d000$dims[[dimind]][[dimind_time]]' changed to 'dim(out[[i]][[t[j]]]) = out$d000$dims[[dimind]][[dimind_time]]').
	# Update: 2012-12-12 - Added info to the output.
	# Update: 2013-05-07 - Added the parameter 'info' controling whether 'info' should be added to the output.
	# Update: 2013-09-24 - Fixed clean=TRUE.
	# Update: 2013-11-11 - Added support for new file format with longs for 'lvar' and the string "0000" to separate 'lvar' and 'labl'.
	# Update: 2013-12-04 - Added 'silent'.
	# Update: 2013-12-22 - Fixed bug with 'equalt' and 'Nnonzero' not being restricted to the required time steps.
	# Update: 2014-02-10 - Removed 'split_char' from the parameters and fixed it to "\u001F" (INFORMATION SEPARATOR ONE). Also added the option of writing functions.
	# Update: 2014-09-10 - Fixed bug which did not save NA as NA, but as 0.
	# Update: 2014-09-24 - Added the option of seting the header on input, to save time when reading files with large headers (typically files with a large number of time steps and varying variable lengths).
	# Update: 2014-09-27 - Changed to fit the new format with numt and r000 in the file header.
	# Update: 2014-12-12 - Added support for complex variables through the data types "cpxf" and "cpxd".
	# Update: 2015-05-11 - Significant speed up when all variables are read from a continous sequence of time steps, in which all the data are read in one go using raw, and each variable converted to its given data type and organized in a list.
	# Last: 2015-05-14 - Added 'raw'.
	

	##################################################
	##################################################
	##### Preparation #####
	### Connection: ###
	# Get the name of the file:
	if("file" %in% is(con)){
		conname = summary(con)$description
		close(con)
		}
	else if(is.character(con)){
		conname = con
		}
	else{
		stop("Invalid connection. Must either be a file object or a character string")
		}
	
	split_char = "\u001F"
	### End of Connection ###
	
	### Header: ###
	# Only the header is read if t=="none" or var=="none" (or none of the elements of 'var are available). 'timeout' is the time variables to return:
	if(identical(t, "none") || identical(var, "none")){
		t = 0
		var = NULL
		timeout = NULL
		}
	# If only 'indt' is requested, try to read only the first and last value and see if all time points are in sequence:
	onlyindt = identical("indt",tolower(var))
	
	# If header variable names are given in 'var', store these and add the requested header variables at the end in the case header=FALSE:
	headernames = c("nvar", "numt", "r000", "labl", "lvar", "dtyp", "numt", "endian")
	if(!isTRUE(header)){
		headervar = intersect(var, headernames)
		}
	
	# The relevant file types to be read, using R-terminology, is "int" and "double". The connection between java-terminology and R-terminology is c("byte", "short int", "long int", "float", "double") ~ c("int", "int", "int", "double", "double"):
 	valid_datatypes = c("character", "int", "int", "int", "double", "double", "double", "double")
 	valid_dtyp = c("char", "byte", "shrt", "long", "floa", "doub", "cpxf", "cpxd")
 	datasizes = c(1, 1, 2, 4, 4, 8, 4, 8)
	### End of Header ###
	
	
	##### Execution #####
	### Read the file in binary mode: ###
	# The files are structured in a specific way. (1) a four byte float holding the number of variables. (2) a four byte long holding the number of time steps of the file. (3) a four byte long holding the number of rows of the 'lvar' information. (4) the lvar information, given as a long-matrix of one row for each time step or only one row if all variables have constant number of elements for each time step. (5) a vector of four character labels of the variables. (6) a vector of four character data dype information strings:
	
	# Test for the TSD format:
	if(!is.TSD(conname, recursive=FALSE, accept.dir=FALSE)){
		warning(paste("The path\"", conname, "\"is not a TSD file", sep=""))
		return(list())
		}
	con = file(conname, "rb")
	seek(con, 4)
	
	# The header may be given in the input:
	if(is.list(header) && all(c("nvar", "numt", "r000", "labl", "lvar", "dtyp", "endian", "dtyp") %in% names(header))){
		nvar = header$nvar
		numt = header$numt
		r000 = header$r000
		lvar = header$lvar
		labl = header$labl
		dtyp = header$dtyp
		endian = header$endian
		}
	else{
		# 1: Number of variables:
		nvar = read.TSD_isnvar(con, n=1L, max_var=max_var)
		if(length(nvar$x)==0){
			return(list())
			}
		endian = nvar$endian
		nvar = nvar$x
		
		# Number of time steps:
		numt = readBin(con, what=integer(), n=1, size=4, signed=TRUE, endian=endian)
		if(numt!=round(numt)){
			warnings("The bytes 5 to 8 are supposed to be a long giving the number of time steps contained in the file, but does not give a whole number")
			}
		else if(numt<1){
			warnings("The bytes 5 to 8 was negative or 0, and was set to 1 in an attempt to read the data regardless")
			}
			
		# Number of rows of 'lvar':
		r000 = readBin(con, what=integer(), n=1, size=4, signed=TRUE, endian=endian)
		if(r000!=round(r000)){
			warnings("The bytes 9 to 12 are supposed to be a long giving the number of time steps stored in the header variable 'lvar', but does not give a whole number")
			}
		else if(r000<1){
			warnings("The bytes 9 to 12 was negative or 0, and was set to 1 in an attempt to read the data regardless")
			}
			
		# 2: Lengths of variables:
		lvar = readBin(con, what=integer(), n=r000*nvar, size=4, signed=TRUE, endian=endian)
		lvar = matrix(lvar, nrow=r000, byrow=TRUE)
		lvar_out = lvar
		if(r000<2 && numt>1){
			suppressWarnings(lvar<-matrix(lvar, nrow=numt, ncol=nvar, byrow=TRUE))
			}
				
		# 3: Names of variables:
		labl = readChar(con=con, nchars=rep(4, nvar), useBytes=TRUE)
		
		# 4: Variable types:
		dtyp = readChar(con=con, nchars=rep(4, nvar), useBytes=TRUE)
	}
	
	
	# Incices to complex variables, which are given as two vectors, one real and one imaginary, and are trancformed to complex here:
	arecomplex = substr(dtyp,1,3)=="cpx"
	
	# 5: Treatment of 'lvar' and creating the matrix 'index' specifying the positions to read from:
	# Identifying the sizes and the data types to be used when reading the data using readBin(). In R c("byte", "shrt", "long", "floa", "doub") corresponds to c("int", "int", "int", "double", "double") with size c(1, 2, 4, 4, 8) bytes as specified in readBin:
	dtyp_present = match(dtyp, valid_dtyp)
	
	# 'lvar_char' is the same as 'lvar' only for character variables the lengths are set to 1. To be used when inserting to the output:
	lvar_char = lvar
	
	# Set the length of all character strings to 1:
	isCharFinite = lvar_char[, dtyp=="char"]
	isCharFinite[isCharFinite>0] = 1
	lvar_char[, dtyp=="char"] = isCharFinite
	# The header length:
	headersize = 4*4 + 4*r000*nvar + 4*length(labl) + 4*length(dtyp)
	# Create the indices used when reading the data:
	index = headersize + c(0, cumsum(c(aperm(lvar)*datasizes[dtyp_present]))[-(length(lvar))])
	index = matrix(index, nrow=max(r000,numt), ncol=nvar, byrow=TRUE)
	
	# If 'indt' is given as input, and a variable named 'indt' is present in the data, 't' is cross referenced to these time steps indexes so that if indt=1, 11, 21, and t=1:12, the first two time steps are read:
	if(!identical(t, 0) && !identical(var, 0) && indt && "indt" %in% labl){
		# 'indtat' is the indices of the 'indt' variables:
		indtat = which("indt"==labl)
		# When 'indt' is requested, a file of many pings may cause reading 'indt' to be a slow process. Thus a test is made for continnuity of 'indt', in which case only the first and last value is read, and the sequence between these is returned as 'indt':
		indt_con = read.TSD_getindtFirstLast(labl, con, index, dtyp_present, valid_datatypes, datasizes, endian)
		# Otherwise, read the indt variable convensionally:
		indt_con = read.TSD_readAllTimeStepsOfOneVariable(nrow(index), lvar=lvar, var=indtat, con=con, index=index, dtyp=dtyp, valid_datatypes=valid_datatypes, dtyp_present=dtyp_present, datasizes=datasizes, endian=endian, arecomplex=arecomplex)
 		
		# Treat 't' given 'indt_con':
 		if(identical(t, "all")){
			t = seq_along(indt_con)
			}
	 	else if(identical(t, "last") || identical(t, "end")){
			t = length(indt_con)
			}
	 	else if(identical(t, "firstlast") || identical(t, "range")){
			t = unique(c(1, length(indt_con)))
			}
		else{
			t = match(as.numeric(t), indt_con)
			}
 		}
	else{
		# Treat 't' given 'numt':
		if(identical(t, "all")){
			t = seq_len(numt)
			}
	 	else if(identical(t, "last") || identical(t, "end")){
			t = numt
			}
		else if(identical(t, "firstlast") || identical(t, "range")){
			t = unique(c(1, numt))
			}
		else{
			t = match(as.numeric(t), seq_len(numt))
			}
		}
	
	if(!keep.all){
		t = t[!is.na(t)]
		}
	validt = !is.na(t)
	cleant = t[validt]
				
	# Define the sequence along 't':
	tseq = seq_along(t)
	
	# Set 'var' to NULL if 't' is empty:
	if(length(t)==0){
 		var = NULL
 		}
 	
	# If var=="all", all variables are read (default):
	if(identical(var, "all")){
		var = labl
		}
						
	# Convert to indices. Only the variables specified in 'var' are read:
	validtimevar = c("mtim", "utim", "utmp", "utma", "ctim", "ftim")
	timeout = intersect(var, validtimevar)
	if(is.character(var)){
		# If "time" is in 'var', all available time variables are read:
		# 'timeout' is a vector of the time variables that are to be returned:
		timeout = intersect(validtimevar, var)
		if("time" %in% var){
			timeout = c(validtimevar, "indt")
			var = unique(c(var, "numt", "indt", validtimevar))
			}
		# If any of "mtim", "utim" and "ftim" are requested, all time variables are read, but only the required returned:
		if(any(c("mtim", "utim", "ftim") %in% var)){
			var = unique(c(var, "numt", "indt", "mtim", "utim", "ctim", "ftim"))
			}
		# Add dimension data to 'var' if present:
		if(length(var)>0 && "d000" %in% labl){
			var = unique(c(var, "d000"))
			}
		varChar = var
		var = which(labl %in% var)
		}
			
	# Numerically given variables are cropped to the sequence of variables
	var = var[var>=1 & var<=nvar]
	# Character variables:
	charvar = which(dtyp[var]=="char")
	# Sequence of length equal to the number of variables:
	varseq = seq_along(var)
	
	# Define the output as a list:
	if(length(t)>0){
		out = rep(list(vector("list", length(t))), length(var))
		}
	else{
		out = list()
		}
		
	
	# 6: Variables:
	# If 'indt' is requested, a file of many pings may cause reading 'indt' to be a slow process. Thus a test is made for continnuity of 'indt', in which case only the first and last value is read, and the sequence between these is retunred as 'indt':
	if(onlyindt && numt>0){
		indtat = which("indt"==labl[var])
		# If indt==TRUE, 't' has already been matched to 'indt' of the file:
		if(indt){
			out[[indtat]][cleant] = as.list(cleant)
			varseq = seq_along(var)[-indtat]
			}
		else{	
			# If 'indt' is requested, a file of many pings may cause reading 'indt' to be a slow process. Thus a test is made for continnuity of 'indt', in which case only the first and last value is read, and the sequence between these is returned as 'indt':
			indt_con = read.TSD_getindtFirstLast(labl, con, index, dtyp_present, valid_datatypes, datasizes, endian)
			# Otherwise, read the indt variable convensionally:
			if(length(indt_con)==0){
				indt_con = read.TSD_readAllTimeStepsOfOneVariable(nrow(index), lvar=lvar, var=which("indt"==labl), con=con, index=index, dtyp=dtyp, valid_datatypes=valid_datatypes, dtyp_present=dtyp_present, datasizes=datasizes, endian=endian, arecomplex=arecomplex)
				}
			
 			# Insert to the output:
 			if(length(indt_con)>0){
				out[[indtat]][validt] = as.list(indt_con[cleant])
				varseq = seq_along(var)[-indtat]
				}
			}
		}
	
	# Plot dots for the progress if silent==FALSE:
	if(!silent){
		infostring = paste0("Reading time steps of file", basename(conname), ":")
		cat(infostring, "\n", sep="")
		totalsteps = length(tseq)
		stepfact = nchar(infostring)/totalsteps
		oldvalue = 0
		}
	
	# If all variables are requested for a continuous sequence of time steps, and the number of values per time step is not high, read all the data as raw, and convert to the apropriate variables afterwards:
	if(is.numeric(raw)){
		raw = sum(lvar[t[1],])<raw
		}
	mint_raw = 0
	
	# Note whether the reading of raw data resulted in a list of time steps or a flat vector of all data for each variable:
	readRawAndEqualTimeSteps <- logical(length(varseq))
	# Here we require that all of the following three occur:
	#	(1) the user has specified 'raw'
	#	(2) the number of requested time steps exceeds mint_raw AND all variables are requested
	#	(3) either only one time step is requested OR a continnous sequence of time steps is requested:
	if(raw && (length(tseq)>mint_raw && identical(varseq,var)) && (length(t)==1 || all(diff(t)==1))){
		# Seek to the position of the 
		s = seek(con, index[t[tseq[1]], 1], origin="start")
		lvarBytes = lvar * rep(datasizes[dtyp_present], each=nrow(lvar))
		index = index - headersize
		#index = index - index[t[tseq[1]], 1]
		sumEachVar = colSums(lvar[t[tseq], ,drop=FALSE])
		# Read the requested time steps as raw, reading all variables:
		raw <- readBin(con, what="raw", n=sum(lvarBytes[t[tseq], ]), endian=endian)
		
		# Qheck for each variable whether all time steps has equal length:
		equalTimeSteps <- apply(lvarBytes[t[tseq], , drop=FALSE], 2, function(xx) all(xx==xx[1]))
		if(all(equalTimeSteps)){
			# Set dimensions to a variable x time matrix:
			dim(raw) <- c(length(raw) / length(tseq), length(tseq))
		}
		
		# Convert to the variables:
		for(i in varseq){
			
			# 1. If the variable has constant length between time steps, pick out the variables as rows in the (in that case) matrix 'raw':
			if(all(equalTimeSteps)){
				theseRows <- index[1, i] + seq_len(lvarBytes[t[tseq[1]], i])
				thisout <- c(raw[theseRows, ])
			}
			else{
				# Here an error was fixed on 2015-09-08, where "index[tseq, i]" was used instead of the correct "index[t[tseq], i]"
				# Extreme care must be taken here, to set the starting points of each value RELATIVE to the time steps that has been read, which implies using only 'tseq' in "index[tseq, i]", whereas the number of bytes should be relative to the available time steps in the file, causing t[tseq] used in "lvarBytes[t[tseq], i]". This is because the data has been read from the position set by seek() above:
				starts <- rep(index[t[tseq], i], lvarBytes[t[tseq], i])
				steps <- sequence(lvarBytes[t[tseq], i])
				thisout <- starts + steps
				thisout = raw[thisout]
			}
			
			# Convert the raw vector to the apropriate data:
			if(dtyp[var[i]]=="char"){
				thisout = readChar(thisout, nchars=lvar[t[tseq],i], useBytes=TRUE)
				#thisout = strsplit(thisout, "")[[1]]
				}
			else{
				thisout = readBin(thisout, what=valid_datatypes[dtyp_present][var[i]], n=length(thisout), size=datasizes[dtyp_present][var[i]], endian=endian, signed=TRUE)
				}
			
			# Split into time steps, but only if there are actually differing lengths of time steps:
			if(!equalTimeSteps[i] && dtyp[var[i]]!="char"){
				if(sumEachVar[i]>1e6){
					l = rbind(c(1,cumsum(lvar[t[tseq], i])[-length(t[tseq])]+1), cumsum(lvar[t[tseq], i]))
					l = split(l, rep(seq_along(t[tseq]),each=2))
					thisout = lapply(l, function(x) if(x[2]>=x[1]) thisout[seq.int(x[1],x[2])] else NULL)
					}
				else{
					temp = split(thisout, rep(index[t[tseq], i], lvar[t[tseq], i]))
					thisout = vector("list", length(tseq))
					thisout[lvar[t[tseq], i]>0] = temp
				}
			}
			else{
				readRawAndEqualTimeSteps[i] <- TRUE
			}
			
			# Paste together stings:
			if(dtyp[var[i]]=="char"){
				thisout = lapply(thisout, paste0, collapse="")
				}
			# Remove names on 'thisout':
			names(thisout) <- NULL
			# Convert to complex:
			if(arecomplex[i]){
				thisout = lapply(thisout, vecRealImaginary2complex)
				}
			if(length(thisout)>0){
				out[[i]] = thisout
				}
			else{
				out[[i]] = thisout
				}
			}
		}
	
	############################################################
	########## (Change itroduced on 2016-07-18, saving #########
	########### time when reading all variables, and ###########
	############ particularly when also reading all ############
	########### time steps, of a file with say 57000 ###########
	############## time steps and 8 variables.): ###############
	############################################################
	
	########## (1) If all variables and all time steps are read, skip seek(): ##########
	else if(identical(varseq,var) && length(t)==numt){
		# For loop through the elements of 't' and 'var', using the sequences tseq and varseq=seq_along(var):
		for(p in tseq){
			# Print a dot if the floor of the new value exceeds the old value in:
			if(!silent){
				thisvalue = floor(p*stepfact)
				if(thisvalue > oldvalue){
					cat(rep(".", thisvalue-oldvalue), sep="")
					if(p==totalsteps){
						cat("\n")
						}
					oldvalue = thisvalue
					}
				}
			# Run through the requested variables at this time step:
			for(i in varseq){
				if(validt[p] && lvar[t[p], var[i]]!=0){
					if(dtyp[var[i]]=="char"){
						thisout = readChar(con, lvar[t[p], var[i]], useBytes=TRUE)
						}
					else{
						thisout = readBin(con, valid_datatypes[dtyp_present][var[i]], n=lvar[t[p], var[i]], size=datasizes[dtyp_present][var[i]], endian=endian, signed=TRUE)
						}
					# Change convert to complex:
					if(arecomplex[i]){
						halflength = length(thisout)/2
						halfind = seq_len(halflength)
						thisout = complex(real=thisout[halfind], imaginary=thisout[halfind+halflength])
						}
					out[[i]][[p]] = thisout
					}
				}
			}
		}
	########## (2) If all variables but not all time steps are read, use seek() only between time steps: ##########
	else if(identical(varseq,var)){
		# For loop through the elements of 't' and 'var', using the sequences tseq and varseq=seq_along(var):
		for(p in tseq){
			# Print a dot if the floor of the new value exceeds the old value in:
			if(!silent){
				thisvalue = floor(p*stepfact)
				if(thisvalue > oldvalue){
					cat(rep(".", thisvalue-oldvalue), sep="")
					if(p==totalsteps){
						cat("\n")
						}
					oldvalue = thisvalue
					}
				}
			s = seek(con, index[t[p], 1], origin="start")
			# Run through the requested variables at this time step:
			for(i in varseq){
				if(validt[p] && lvar[t[p], var[i]]!=0){
					if(dtyp[var[i]]=="char"){
						thisout = readChar(con, lvar[t[p], var[i]], useBytes=TRUE)
						}
					else{
						thisout = readBin(con, valid_datatypes[dtyp_present][var[i]], n=lvar[t[p], var[i]], size=datasizes[dtyp_present][var[i]], endian=endian, signed=TRUE)
						}
					# Change convert to complex:
					if(arecomplex[i]){
						halflength = length(thisout)/2
						halfind = seq_len(halflength)
						thisout = complex(real=thisout[halfind], imaginary=thisout[halfind+halflength])
						}
					out[[i]][[p]] = thisout
					}
				}
			}
		}
	########## (3) Old version using seek() at all time steps and all variables to be read: ##########
	else{
		# For loop through the elements of 't' and 'var', using the sequences tseq and varseq=seq_along(var):
		for(p in tseq){
			# Print a dot if the floor of the new value exceeds the old value in:
			if(!silent){
				thisvalue = floor(p*stepfact)
				if(thisvalue > oldvalue){
					cat(rep(".", thisvalue-oldvalue), sep="")
					if(p==totalsteps){
						cat("\n")
						}
					oldvalue = thisvalue
					}
				}
			# Run through the requested variables at this time step:
			for(i in varseq){
				if(validt[p] && lvar[t[p], var[i]]!=0){
					s = seek(con, index[t[p], var[i]], origin="start")
					if(dtyp[var[i]]=="char"){
						thisout = readChar(con, lvar[t[p], var[i]], useBytes=TRUE)
						}
					else{
						thisout = readBin(con, valid_datatypes[dtyp_present][var[i]], n=lvar[t[p], var[i]], size=datasizes[dtyp_present][var[i]], endian=endian, signed=TRUE)
						}
					# Change convert to complex:
					if(arecomplex[i]){
						halflength = length(thisout)/2
						halfind = seq_len(halflength)
						thisout = complex(real=thisout[halfind], imaginary=thisout[halfind+halflength])
						}
					out[[i]][[p]] = thisout
					}
				}
			}
		}
	# Close connection:
	close(con)
	
	if(keep.all){
		out <- lapply(out, function(x) if(length(x)==0) vector("list", length(t)) else x)
	}
	
	##### Output #####
	# Preparing the output, if any:
	if(length(var)>0 && length(t)>0){
		# Adding the names provided in the file:
		names(out) = labl[var]
		# Split character strings:
		if(length(charvar)>0){
			out[charvar] = lapply(out[charvar], function(xx) lapply(xx, function(x) if(length(x)) strsplit(x, split_char, fixed=TRUE)[[1]]))
			}
		
		# Adding dimension if dimension==TRUE:
		if(isTRUE(dimension) || tolower(dimension)=="t"){
			# If present, dimension information 'd000' is extracted from 'out$d000' and cross referenced against length information 'lvar':
			if(!is.null(out$d000)){
				# Store the 'd000' vector:
				out$d000 = unlist(out$d000[validt], use.names=FALSE)
				out$d000v = out$d000
				# Transform the 'd000' vector to a list:
				out$d000 = read.TSD_d000_v2lv(out$d000, conname)
				# Remove "d000" from 'var':
				var = var[labl[var]!="d000"]
				
				# If dimension information is given, this is assigned to the relevant data:
				addDimension = which(var %in% out$d000$var)
				doNotAddDimension = which(!var %in% out$d000$var)
				for(i in addDimension){
					# Get the position of the current variable in the list of variables in out$d000$var:
					dimind = which(out$d000$var==var[i])
					# Test for equality of the dimensions for ALL time steps. This also implies that t and cleant are identical:
					allDimensionsEqual = all(tseq %in% out$d000$indx[[dimind]])  &&  all(sapply(out$d000$dims[[dimind]], function(x) x == out$d000$dims[[dimind]][[1]]))
					# Collapse variables with equal dimensions into arrays with time along the last dimension:
					if(allDimensionsEqual){
						# If the data were read using raw, and time steps had identical lengths for the current variable, skip unlisting:
						if(!readRawAndEqualTimeSteps[i]){
							out[[i]] = unlist(out[[i]], use.names=FALSE)
						}
						
						newSingleDimension = out$d000$dims[[dimind]][[1]]
						if(prod(newSingleDimension)>0){
							###   # Add a time dimension if drop.out==FALSE, and the number of time steps is > 1, and also requiring that not all dimensions are 1 (single values, in which case )
							###   # if(!drop.out && (length(t)>1 && !all(newSingleDimension==1))){
							###   #if(!drop.out || (length(t)>1 && !all(newSingleDimension==1))){
							###   	print(134513451)
							###   	printt(newSingleDimension)
							###   	printt(drop.out)
							###   	printt(!drop.out || !all(newSingleDimension==1))
							###   	printt(t)
							###   	printt(dim(out[[i]]))
							###   if((length(t)>1 && (!drop.out || !all(newSingleDimension==1)))){ # Removed on 2017-03-16, since also files of only one time steps should be returned with time dimension it drop.out=FALSE.
							###   	
							###   ### if(!drop.out || !all(newSingleDimension==1)){
							###   	dim(out[[i]]) = c(newSingleDimension, length(t))
							###   	}
							###   else if(length(t)==1 && length(newSingleDimension)>1){
							###   	dim(out[[i]]) = newSingleDimension
							###   	}
								
								
								
								
							# First add single time dimensions and time:
							dim(out[[i]]) = c(newSingleDimension, length(t))
							# Then remove the time dimension if drop.out==TRUE and time is of length 1:
							if(drop.out && length(t)==1){
								thisdim <- dim(out[[i]])
								dim(out[[i]]) <- thisdim[-length(thisdim)]
							}
						}
					}
					# Otherwise treat each time step separately:
					else{
						addDimension_j = which(tseq %in% out$d000$indx[[dimind]])
						for(j in addDimension_j){
							dimind_time = which(out$d000$indx[[dimind]]==tseq[j])
							dim(out[[i]][[j]]) = out$d000$dims[[dimind]][[dimind_time]]
							}
						}
					}
					
				# Collapse variables with equal lengths into arrays with time along the second dimension, only done if all time steps are valid:
				if(all(validt)){
					# Update 'lvar_char' with the new string lengths:
					if(length(charvar)>0){
						lvar_char[t, charvar] = unlist(lapply(out[charvar], function(x) lapply(x, length)), use.names=FALSE)
						}
					# Get indices of the variables that have equal length for each time step:
					# Fixed bug here by inserting  & x != 0, ensuring that variables with no data for the requested time steps are not collapsed if keep.all==TRUE:
					allLengthsEqual = apply(lvar_char[t, var[doNotAddDimension], drop=FALSE], 2, function(x) all(x == x[1] & if(keep.all) x != 0 else TRUE))
					for(i in doNotAddDimension[allLengthsEqual]){
						out[i] = list(unlist(out[[i]], use.names=FALSE))
						# Add time step dimenstion if both the number of time steps and the lengths of the variables exceed 1:
						if(length(out[[i]])>0 && (!drop.out || (length(t)>1 && lvar_char[t[1], var[i]]>1))){
							dim(out[[i]]) = c(lvar_char[t[1], var[i]], length(t))
							}
						}
					}
				}
			}
		# If only one time steps is present, or if only one element is present for each and every time step of a variable, drop the list:
		else if(drop.out){
			if(length(t)==1){
				out = lapply(out, unlist)
				}
			else{
				allLengths1 = apply(lvar_char[t, var, drop=FALSE], 2, function(x) all(x == 1))
				out[allLengths1] = lapply(out[allLengths1], unlist)
				}
			}
		
		# Adding time variables:
		if("utim" %in% timeout){
			out$utim = utim.TSD(out)
			}
		if("mtim" %in% timeout){
			out$mtim = mtim.TSD(out)
			}
		if("ftim" %in% timeout){
			out$ftim = ftim.TSD(out)
			}
		
		# Revert to the vector representation of 'd000':
		namesout = names(out)
		if(all(c("d000", "d000v") %in% namesout)){
			out[namesout=="d000"] = out[namesout=="d000v"]
			out[namesout=="d000v"] = NULL
			}
		
		# Remove out$d000 if required:
		if(!d000.out){
			out[names(out)=="d000"] = NULL
			}
		
		# Convert strings starting with "FUNCTION" to functions:
		out = lapply(out, function(x) if(is.list(x)) lapply(x, read.TSD_funstring2fun) else read.TSD_funstring2fun(x))
		} # End of if any output.
	
	# Creating the header object, to be returned if header==TRUE, and partly if any of the header names c("nvar", "labl", "lvar", "dtyp", "numt", "endian").
	out_header = list()
	out_header$nvar = nvar
	out_header$numt = numt
	out_header$r000 = r000
	out_header$labl = labl
	out_header$lvar = lvar_out
	out_header$dtyp = dtyp
	out_header$numt = as.double(numt)
	
	if(endian=="swap"){
		out_header$endian = setdiff(c("little", "big"), .Platform$endian)
		}
	else{
		out_header$endian = .Platform$endian
		}
	# Add info to the output:
	if(info && length(out)>0){
		out$info = info.TSD(names(out), file=NULL)
		}
	
	# Add 'out_header' if header==TRUE:
	if(!identical(header, FALSE)){
		out = c(out, out_header)
		}
	# Else add only the requested header variables (if any):
	else{
		out[headervar] = out_header[headervar]
		}
	out
	##################################################
	##################################################
	}
