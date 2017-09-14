#*********************************************
#*********************************************
#' Writes files using the Time Step Data format. See the documentation on echoIBM for specification of the Time Step Data format (TSD). Handling of missing values differ between data types, as given in the table below, which gives the values that are stored in the file when given as NA, NaN, NA_real_, or NA_integer_ in the data:
#' NA
#' NA
#' NA
#' NA
#' NA
#' NA
#' and similar for the complex data types.
#'
#' @param x  is the list containing the data to write.
#' @param con  is the connection object or a character string naming the output file.
#' @param t  is a vector of the time steps to write (in the range [1, number of pings]). If none of the elements of 't' are in [1, number of pings], only the header is written. If t == "all", all time points are written and if t == "none", none of the time points are written.
#' @param var  is a vector of the variables to be written, either given as a character vector holding the names of the variables, as specified in the TSD-format, or as the number of the variables in 'labl', if 'labl' is known. If var == "all", all varialbes are written and if var == "none", none of the variables are written.
#' @param header  is a list containing one or more of the following two elements ("nvar" and "lvar" are always extracted from the data as of 2011-12-06):
#' @param numt  is the number of time points of the data 'x' overriding any existing information about the number of time steps, given in 'header' or 'x' (intended to only be specified if all time steps are arranged in the same vector for all variables).
#' @param dimension  is an object specifying whether the dimensions of the variables at each time step are to be stored in the file. 'dimension' may have 4 values:
#' @param ts  is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts = 3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts = -2 matrices are not regarded as having time along the second dimension.
#' @param append  is TRUE if data is to be appended to the end of 'con' (only valid when 'con' is a character string), in which case the header is not written. No check made for compatibility to the existing file.
#' @param ow  is TRUE if the user wish to overwrite existing file.
#' @param ...  is to allow variables passed on from other methods.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats na.omit
#'
#' @export
#' @rdname write.TSD
#'
write.TSD<-function(x, con, t="all", var="all", header=NULL, numt=NULL, dimension=TRUE, ts="last", reserve=0, keep.null=TRUE, dup=FALSE, endian=.Platform$endian, sep=" ", append=FALSE, ow=TRUE, keep.float=FALSE, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-02-04 - Clean version.
	# Update: 2010-06-17 - Lots of changes over the last months: (1) Default endianness changed to "big", because endianness is like Colins, "big" is better (and is best for windows, while ok for unix), (2) added the option 'reserve' for reserving header space to append data later, (3) added the C++ method "UpdateHeaderTSD" used when appending data of diffing variable lengths, (4) changed the dimensions of 'lvar' to [numt x nvar], and other stuff.
	# Update: 2010-09-09 - Cleaned up the method of finding 'numt', 'lvar' and treating the variables that do not fit exactly into these.
	# Update: 2010-09-09 - Added support for reading TSD files containing dimension data. Dimension data must be specified as an additional variable named "d000" structured as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the index numbers for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p', collapsed into a vector. Also added the option 'dimension' for specifying whether dimension should be added to the data, or simply for specifying the dimension. Cleaned up the treatment of differing numbers of time steps for the variables, so that the largest number of time steps extracted from the data is reagarded as the true number of time steps. Arrays not complying with this number of time steps are written as dimensionless vectors at the first time step.
	# Update: 2010-09-22 - Sorted out the addign of dimension through d000. Observed error when writing arrays. The number of dimensions are seeked in the last dimension of arrays in this version. Then the max of the 'numt' of every variable is taken to be the overall 'numt'. This reduces contol over the number of time steps. See the last version for better handling of the number of time steps in variables.
	# Update: 2010-09-22 - Sorted out the treatment of time steps for arrays, through the input 'ts'. 
	# Update: 2011-12-06 - Changed so that the only header variables that can be specified in 'header' are the data types 'dtyp' and the labels 'labl'. The number of variables and the lengths of the variables are extracted from the data in any case. All header variables specified in the data are ignored.
	# Update: 2012-07-31 - Added transformation from double to integer when dtyp == "shrt".
	# Update: 2013-07-16 - Removed 'split_char' and renamed 'collapse_char' to 'split_char'. Also changed the treatment of strings, so that the dimension of a string array is kept at each time step.
	# Update: 2013-07-17 - Added "long" as a data type representing four byte long integer, since "shrt" only applies to integers with values in the range −32768 to 32767. Integers detected in 'x' are from now on written as "long", and writing integers as "shrt" requires specification of this in the header.
	# Update: 2013-08-07 - Changed to write d000 at every time step (d000 = 0 at time steps where no dimenstion exists for any of the variables).
	# Update: 2013-08-07 - Fixed bug related to d000.
	# Update: 2013-08-13 - Added support for 'x' with variables not present in the existing file, or with order inconsistent with the variable order in the existing file, when append == TRUE.
	# Update: 2013-08-22 - Added 'dup'.
	# Update: 2013-11-11 - Added 'lvar_float'.
	# Update: 2014-01-27 - Fixed bug: When 'numt' was given, it did not have the desired effect on the dimension information. Now, if numt equals the last dimension of a variable, time is assumed along that last dimension, just as if the number of dimensions of a variable is in 'ts'.
	# Update: 2014-02-10 - Removed 'split_char' from the parameters and fixed it to "\u001F" (INFORMATION SEPARATOR ONE). Also added the option of writing functions.
	# Update: 2014-04-24 - Added 'numt_old' for skipping the reading of existing file when appending.
	# Update: 2014-07-02 - Added the function is.doubleTSD_element() to identify variable that should be saved in double presicion.
	# Update: 2014-09-10 - Fixed bug which did not save NA as NA, but as 0.
	# Update: 2014-09-27 - NEW FORMAT: Added the numer of time steps 'numt' in the file as the second long (after 'nvar'), and the number of rows of 'lvar' 'r000' to speed up the reading of the header and appending to the header. Also removed non-binary writing.
	# Update: 2014-12-12 - Added support for complex variables through the data types "cpxf" and "cpxd".
	# Update: 2015-10-01 - Updated to treat stings simply by counting the number of characters (before the split character was included, but in the new version the character variables are collapsed to one sting per time step for each variable before using write.TSD_fun_lvar()).
	# Last: 2016-07-04 - Fixed several bugs related to the use of 'numt' and 't', involving extracting 'lvar', 'singleindex' and dimensions first, and then subsetting these during writing.
	########### DESCRIPTION: ###########
	# Writes files using the Time Step Data format. See the documentation on echoIBM for specification of the Time Step Data format (TSD). Handling of missing values differ between data types, as given in the table below, which gives the values that are stored in the file when given as NA, NaN, NA_real_, or NA_integer_ in the data:
	#	<value>				<data type>
	#				floa	doub	shrt	long
	#	NA			0		NaN		0		NA
	#	NaN			NaN		NaN		0		NA
	#	NA_real_	NaN		NA		0		NA
	#	NA_integer_	NaN		NA		0		NA
	# and similar for the complex data types.
	########## DEPENDENCIES: ###########
	# UpdateHeaderTSD()
	############ DETAILS: ############
	#
	############ VALUE: ############
	#
	############ REFERENCES: ############
	#
	############ SEAALSO: ############
	#
	############ EXAMPLES: ############
	#
	############ VARIABLES: ############
	# ---x--- is the list containing the data to write.
	# ---con--- is the connection object or a character string naming the output file.
	# ---t--- is a vector of the time steps to write (in the range [1, number of pings]). If none of the elements of 't' are in [1, number of pings], only the header is written. If t == "all", all time points are written and if t == "none", none of the time points are written.
	# ---var--- is a vector of the variables to be written, either given as a character vector holding the names of the variables, as specified in the TSD-format, or as the number of the variables in 'labl', if 'labl' is known. If var == "all", all varialbes are written and if var == "none", none of the variables are written.
	# ---header--- is a list containing one or more of the following two elements ("nvar" and "lvar" are always extracted from the data as of 2011-12-06):
	#		"labl" (string vector of length 'nvar': labels of the variables)
	#		"dtyp" (string vector of length 'nvar': TSD data type of the variables). If missing, the header is generated from 'x'.
	# ---numt--- is the number of time points of the data 'x' overriding any existing information about the number of time steps, given in 'header' or 'x' (intended to only be specified if all time steps are arranged in the same vector for all variables).
	# ---dimension--- is an object specifying whether the dimensions of the variables at each time step are to be stored in the file. 'dimension' may have 4 values:
	#		1 - FALSE - no dimension data should be written to file.
	#		2 - TRUE - dimension data to be extracted from the data.
	#		3 - A vector of values specifying the dimensions of the data. These values must agree with the data. Given as c(narrays_1, ndims_1, indx_1, dims_1, narrays_2, ndims_2, indx_2, dims_2, ... , narrays_numt, ndims_numt, indx_numt, dims_numt), where 'narrays_p' is the number of variables at time step 'p' that have dimension (vectors are not regarded av having dimension, corresponding to the dim() function), 'ndims_p' is the number of dimensions for each variable at time step 'p', 'indx_p' is the index numbers for the variables that posess dimension at time step 'p', and 'dims_p' are the dimension values for the variables indexed by 'indx_p' at time step 'p', collapsed into a vector.
	#		4 - A list of 'nvar' lists of lengths 'numt', holding the dimensions of the variables at all time steps.
	# ---ts--- is a numeric vector specifying the rules for regarding the last dimensions of arrays as time: If the number of dimensions of an array is included in the set 'ts' the last dimension of that array is considered to be along time, so that if ts = 3 and we wish to write an array of dimension [3, 4, 2], this array will be written as two [3, 4] arrays. 'ts' is utilized through seq_along(dim(x))[ts], so that if ts = -2 matrices are not regarded as having time along the second dimension.
 	# - ---reserve' is the number of time steps to reserve for future appending to the file (in which case 'reserve' rows of zeros are appended in 'lvar' in the header).
 	# - ---keep.null' is FALSE if elements of length 0 should be discarded from the data, in which case a numerical 'var' would represent indices of 'x' AFTER empty elements have been removed.
 	# - ---dup' is TRUE to allow for duplicated variable label. If dup = FALSE (default) all variables with the same label as a previous variable in the input list 'x' are removed.
 	# - ---endian' is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	# ---append--- is TRUE if data is to be appended to the end of 'con' (only valid when 'con' is a character string), in which case the header is not written. No check made for compatibility to the existing file.
	# ---ow--- is TRUE if the user wish to overwrite existing file.
	# ---...--- is to allow variables passed on from other methods.
	

	##################################################
	##################################################
	##### Preparation #####
	split_char = "\u001F"
	### Connection: ###
	# Get the name of the file:
	if("file" %in% is(con)){
		conname = summary(con)$description
		close(con)
		}
	else if(is.character(con)){
		conname = con
		if(!ow && !append && file.exists(conname)){
			stop(paste("Cannot overwrite existing file ", conname, ". To allow owerwriting set the option 'ow' to TRUE", sep=""))
			}
		}
	else{
		stop("Invalid connection. Must either be a file object or a character string")
		}
	# 2014-02-12: It was discovered that tilde expansion is not accepted by the c++ funcitons UpdateHeaderTSD_long():
	conname = path.expand(conname)
	### End of Connection ###
	
	### Clean 'x': ###
	# If the input is not a list or is NULL, an error is returned:
	if(!is.list(x)){
		stop("The data 'x' must be organized in an R list object")
		}
	
	# Remove header elements and "info" from 'x' if present:
	headerind = na.omit(match(c("nvar", "lvar", "labl", "dtyp", "numt", "r000", "d000", "info"), names(x)))
	if(length(headerind)>0){
		x = x[-headerind]
		}
	# Remove NULL-elements from 'x' if specified:
	if(any(is.na(names(x)))){
		#warning("There were elements of 'x' with missing names. These elements were not written to file")
		x = x[!is.na(names(x))]
		}
	if(!keep.null){
		nullelements = unlist(lapply(x, length), use.names=FALSE) == 0
		x = x[!nullelements]
		}
	funs = sapply(x, is.function)
	
	# Transform funcitons to strings, adding the string "_FUNCTION_" at the start. This is then used at reading to convert back to function:
	if(any(funs)){
		x[funs] = lapply(x[funs], function(xx) if(is.list(xx)) lapply(xx, function2character, fname="FUNCTION") else function2character(xx, fname="FUNCTION"))
		}
	### End of Clean 'x' ###
	
		
	### Header: ###
	# Number of variables:
	header$nvar = as.double(length(x))
	# If not given or not of the same length as the non-header elements of 'x', the variable labels are read from the names of 'x' (only four character names written):
	if(length(header$labl)!= header$nvar){
		if(length(header$labl)>0){
			warning(paste("Variable labels specified in 'header' do not match the data (number of variables = ", header$nvar, ", length of header$labl = ", length(header$labl), ")", sep=""))
			}
		header$labl = names(x)
		}
	# Remove all variables with non-four-character labels:
	if(any(nchar(header$labl)!= 4)){
		fourcharind = nchar(header$labl) == 4
		#warning(paste("Only elements having 4 character names written to file: (\"", paste(header$labl[!fourcharind], collapse = "\", \""), "\")", sep = ""))
		x = x[fourcharind]
		header$labl = header$labl[fourcharind]
		header$nvar = as.double(length(x))
		}
	# If dup = FALSE, remove variables with duplicated labels:
	if(!dup){
		dup = duplicated(header$labl)
		x = x[!dup]
		header$labl = header$labl[!dup]
		header$nvar = as.double(length(x))
		}
	if(length(x)==0){
		# 0Write the TSD file format identifyer (magic number):
		writeChar("%TSD", con, 4, eos=NULL)
		return(4)
		}
	
	# Get the data types of the elements of 'x', if missing ('dtyp' can be given as a list in the input 'header'):
	if(length(header$dtyp)!= header$nvar || is.list(header$dtyp)){
		# Insert the data types specified in the header as a list with elements named by the corresponding variable lables:
		if(length(header$dtyp)>0 && is.list(header$dtyp)){
			# First remove variables that are not present in 'labl':
			header$dtypOld = header$dtyp[names(header$dtyp) %in% header$labl]
			header["dtyp"] = list(NULL)
			}
		if(length(header$dtyp)>0){
			warning(paste("Variable types specified in 'header' do not match the data (number of variables = ", header$nvar, ", length of header$dtyp = ", length(header$dtyp), ")", sep=""))
			}
		# 'timeind' are the indices of the time variables, 'charind' are the indices og the character variables and 'intind' are the indices og the integer variables:
		timeind = header$labl %in% c("mtim", "utim", "utmp", "utma", "ctim", "ftim", "imtm", "iutm", "ictm", "iftm", "u000")
		charind = unlist(lapply(x, is.charTSD_element), use.names=FALSE)
		doubind = logical(length(x))
		if(!keep.float){
			doubind = unlist(lapply(x, is.doubleTSD_element), use.names=FALSE) # TIME DEMANDING (0.2 sec for 1324*500*117 values)
			}
		intind = unlist(lapply(x, is.integerTSD_element), use.names=FALSE)
		cmpxind = unlist(lapply(x, is.complexTSD_element), use.names=FALSE)
		# Setting variale type from the indices above (defaulted to float for numerics):
		header$dtyp = rep("floa", length.out = length(x))
		header$dtyp[which(timeind | doubind)] = "doub"
		header$dtyp[which(charind)] = "char"
		header$dtyp[which(intind)] = "long"
		header$dtyp[which(cmpxind & !(timeind | doubind))] = "cpxf"
		header$dtyp[which(cmpxind & (timeind | doubind))] = "cpxd"
		# If appending, set 'dtyp' of all empty elements to NA (using 'charind', but any other could be used). See comment to 'rowIndexNA':
		if(append){
			header$dtyp[is.na(charind)] = NA
			}
		# ????
		if(length(header$dtypOld)>0 && is.list(header$dtypOld)){
			header$dtyp[match(names(header$dtypOld), header$labl)] = unlist(header$dtypOld, use.names=FALSE)
			}
		}
		
	# The input can be used to override header$numt, in which case the parameter 'ts' is set to NULL. This implies that character variables are written as one time step (unless the number of string elements equal the number of time steps), and other variables are assumed to have time along the last dimension:    	
	if(length(numt)>0){
		header$numt = numt
		ts = NULL
		#if(numt>1){
			#ts = NULL
		#	}
		}
	# The number of time steps is otherwise extracted from the data using write.TSD_fun_numt():
	else{
		# header$numt = max(unlist(lapply(x, function(y) write.TSD_fun_numt(y, ts))), na.rm=TRUE)
		header$numt = max(unlist(lapply(x, write.TSD_fun_numt, ts=ts), use.names=FALSE), na.rm=TRUE)
		}
	
	# Paste together all elelments of string vectors for each time step:
	charind = unlist(lapply(x, is.charTSD_element), use.names=FALSE)
	# x[which(charind)] = lapply(x[which(charind)], function(y) write.TSD_collapse_string(y, header$numt, split_char, ts))
	x[which(charind)] = lapply(x[which(charind)], write.TSD_collapse_string, numt=header$numt, split_char="\u001F", ts=ts)

	# Get the lengths of the variables:
	#header$lvar = matrix(unlist(lapply(x, function(y) write.TSD_fun_lvar(y, header$numt, ts)), use.names=FALSE), ncol=length(x))
	header$lvar = matrix(unlist(lapply(x, write.TSD_fun_lvar, numt=header$numt, ts=ts), use.names=FALSE), ncol=length(x))
	
	# If var == "all", all variables are written:
	if(identical(var, "all")){
		var = 1:header$nvar
		}
	# If var == "none", no variables are written:
	if(identical(var, "none")){
		var = 0
		}
	# If 'var' is given as a vector of four character variable names, the vector is matched to the lables of the input file variables:
	if(is.character(var)){
		var = which(header$labl %in% var)
		}
	# Crop the variables 'var' to the interval [1, header$nvar]:
	var = var[var>= 1 & var<= header$nvar]
	
	# If 'dimension' is TRUE or given as a list of lists or a vector (see description of 'dimension') dimension data are included in the file:
	if(header$numt>0 && !identical(dimension, FALSE)){
		if(is.list(dimension)){
			x$d000 = write.TSD_d000_lt2v(dimension, header$numt)
			}
		else if(!isTRUE(dimension) && is.numeric(dimension)){
			x$d000 = dimension
			}
		else if(isTRUE(dimension)){
			if(is.null(x$d000)){
				dimension = write.TSD_get_d000_lv(x, header$numt, ts)
				x$d000 = write.TSD_d000_lv2lt(dimension, header$numt)
				x$d000 = write.TSD_d000_lt2v(x$d000, header$numt)
				# Update the header with the specifications of the dimension data:
				header$nvar = header$nvar+1
				header$lvar = cbind(header$lvar, write.TSD_lvar_d000(x$d000, header$numt))
				header$labl = c(header$labl, "d000")
				header$dtyp = c(header$dtyp, "long")
				var = c(var, header$nvar)
				}
			}
		# Assure integer type of 'x$d000':
		if(length(x$d000)>0){
			x$d000 = as.integer(x$d000)
			}
		}
	
	# 'singleindex' is defined differently from 'index' used in read.TSD, and is only used for the elements of the data that are vectors or multidimensional arrays, to write the correct elements:
	singleindex = aperm(apply(header$lvar, 2, function(y) c(0, cumsum(y))))
	
	# If t == "all", all time points are written:
	if(identical(t, "all")){
		t = 1:header$numt
		}
	# If t == "none", no time points are written:
	if(identical(t, "none")){
		t = 0
		}
	# Crop the time points 't' to the interval [1, x$numt]:
	t = t[t>= 1 & t<= header$numt]
	
	# Subset 'lvar' by 't':
	header$lvar = header$lvar[t, , drop=FALSE]
	header$numt = length(t)
	if(length(header$lvar) == 0){
		header$lvar = matrix(0, nrow=1, ncol=length(x))
	}
	
	
	# Reserve space in the header for future appending:
	if(reserve>0 && !append){
		reservedim = c(reserve, header$nvar)
		reserve0 = double(prod(reservedim))
		dim(reserve0) = reservedim
		header$lvar = rbind(header$lvar, reserve0)
		}
	
	# Indicator for unequal lengths of variables for each time step:
	unequal = !all(apply(header$lvar, 2, function(y) mean(y) == y))
	
	# Get the number of rows of the 'lvar' which is written to the file:
	if(unequal){
		header$r000 = nrow(header$lvar)
		}
	else{
		header$r000 = 1
		}
	
	# Different method for writing if a variable is of list type or of array type:
	listtype = sapply(x, is.list)
	# Change lvar for complex variables:
	arecomplex = substr(header$dtyp,1,3) == "cpx"
	arecomplex[is.na(arecomplex)] = FALSE
	if(any(arecomplex)){
		header$lvar[,arecomplex] = header$lvar[,arecomplex]*2
		}
	### End of Header ###
	
	##### Execution #####
	if(append){
		# Read the header of the existing file:
		header_old = read.TSD(conname, var=NULL, header=TRUE, d000.out=TRUE)
		if(length(header_old)==0){
			return(FALSE)
		}
		
		# Get the number of reserved time steps of the existing file:
		reserved = header_old$r000 - header_old$numt
		
		# Match the variables in the data and the existing file, and remove variables that will not be written to file:
		matchlabl = match(header$labl, header_old$labl)
		# Create a two column matrix with which of the variables in 'x' that are present in the existing file in the first column, and the indices of these variables in the existing file in the second column:
		matchlablNA = is.na(matchlabl)
		lablPresent = cbind(which(!matchlablNA), matchlabl[!matchlablNA])
		# Intersect with 'var':
		lablPresent = lablPresent[lablPresent[, 1] %in% var, ]
		# Update 'var' to agree with the existing file:
		var = lablPresent[, 1][order(lablPresent[, 2])]
		# Change the 'lvar' to match the existing file:
		appenddim = c(nrow(header$lvar), ncol(header_old$lvar))
		lvarToAppend = double(prod(appenddim))
		dim(lvarToAppend) = appenddim
		lvarToAppend[, lablPresent[, 2]] = header$lvar[, lablPresent[, 1]]
		
		# Empty list elements in 'x' are identified by NA in header$dtyp, and are set to the 'dtyp' in the extisting file (if append = FALSE, they are set to "floa"):
		rowIndexNA = is.na(header$dtyp[lablPresent[, 1]])
		header$dtyp[lablPresent[, 1]][rowIndexNA] = header_old$dtyp[lablPresent[, 2]][rowIndexNA]
		
		# Check for consistancy between old and new dtyp:
		dtypUnequal = header_old$dtyp[lablPresent[, 2]] !=  header$dtyp[lablPresent[, 1]]
		if(any(dtypUnequal)){
			warning(paste0("The following variables were converted to the corresponding data types of the existing file: ", paste(which(dtypUnequal), collapse=", ")))
			header$dtyp[lablPresent[dtypUnequal, 1]] = header_old$dtyp[lablPresent[dtypUnequal, 2]]
			}
		
		# Also modify 'd000' t0 fit the file:
		x$d000 = write.TSD_modifyd000(x$d000, lablPresent)
				
		# Warning if variables do not match the existing file:
		if(any(is.na(matchlabl))){
			warning(paste0("The following variables were not written to file (not present in the file) ", conname, ":\n", paste(header$labl[matchlablNA], collapse=", ")))
			}
		# Warning if there is insufficient reserved space in the header for appending the variables (only when header_old$lvar is a matrix):
		if(prod(dim(header_old$lvar)) > header_old$nvar && header$numt>reserved){
			t = t[seq_len(reserved)]
			warning(paste0("Too few time steps reserved in the file \"", conname, "\". Not all time steps appended to the file (", paste(t, collapse=", "), ")"))
			}
		
		# No changes to the header if all variables have equal length (!unequal), AND the existing header has only one row (header_old$r000 == 1), AND the 'lvar' of the existing and current header aggree (all(header_old$lvar == header$lvar[1, ])):
		if(!unequal && header_old$r000 == 1 && all(header_old$lvar == header$lvar[1, ])){
			headerappend = FALSE
			}
		else if(header_old$r000-header_old$numt >=  header$numt){
			headerappend = TRUE
			}
		else{
			warning(paste0("Variables not match the existing file (", conname, ") . Use 'reserve' to reserve space for variables of unequal length over 'reserve time steps'. Old 'lvar':\n", paste(apply(header_old$lvar, 1, paste, collapse=", "), collapse="\n"), "\n for variables\n", paste(header_old$labl, collapse=", "), "\n\nCurrent 'lvar':\n", paste(apply(header$lvar, 1, paste, collapse=", "), collapse="\n")))
			return("Error:reserve")
			}
		endian = header_old$endian
		}
		
	# Open connection:
	if(length(conname)>1){
		warning(paste("More than one file path given. The first chosen: ", conname[1]))
		conname = conname[1]
		}
	if(append){
		#tryCatch(con<-file(conname, "ab"), error=function(err) paste("Cannot open the connection", conname, "(package:TSD)"))
		thisd <- try(con<-file(conname, "ab"), silent=TRUE)
		if(is(thisd) == "try-error"){
			warning(paste("Cannot open the connection", conname, "(package:TSD)"))
			}
		}
	else{
		#tryCatch(con<-file(conname, "wb"), error=function(err) paste("Cannot open the connection", conname, "(package:TSD)"))
		thisd <- try(con<-file(conname, "wb"), silent=TRUE)
		if(is(thisd) == "try-error"){
			warning(paste("Cannot open the connection", conname, "(package:TSD)"))
			}
		}
		
	# Write the header if data is not appended:
	if(!append){
		# 0: Write the TSD file format identifyer (magic number):
		writeChar("%TSD", con, 4, eos=NULL)
		# 1a: Writing the number of variables:
		writeBin(header$nvar, con, size=4, endian=endian)
		# 1b: Writing the number of time steps:
		writeBin(as.integer(header$numt), con, size=4, endian=endian)
		# 1c: Writing the number of rows of 'lvar':
		writeBin(as.integer(header$r000), con, size=4, endian=endian)
		# 2: Writing the lengths of variables:
		if(reserve == 0 && !unequal){
			# Write the lengths of variables as longs (new system since 2013-11-11):
			writeBin(as.integer(header$lvar[1, ]), con, size=4, endian=endian)
			}
		else{
			# Write the lengths of variables as longs (new system since 2013-11-11):
			writeBin(as.integer(c(t(header$lvar))), con, size=4, endian=endian)
			}
		# 3: Writing the names of variables:
		writeChar(header$labl, con, rep(4, header$nvar), eos=NULL)
		# 4: Writing the variable types:
		writeChar(header$dtyp, con, rep(4, header$nvar), eos=NULL)
		# 5: Writing the variables time step for time step:	
		}
	# Update header if !simpleappend:
	if(append){
		# Update 'numt'. The 8 bytes are the "%TSD" string and the 'nvar' float:
		UpdateHeaderTSD(conname, header_old$numt+header$numt, 8, endian)
		if(headerappend){
			# Update 'lvar'. The 16 bytes are the "%TSD" string, the 'nvar' float, the 'numt' long, and the 'r000' long:
			UpdateHeaderTSD(conname, c(t(lvarToAppend)), 16 + 4*header_old$numt*header_old$nvar, endian)
			}
		# The code below did not work, so a C++ function is called (see "UpdateHeaderTSD.R"):
		#seek(con, where = 4+4*header_old$numt*header_old$nvar, origin = "start")
		#writeBin(c(t(header$lvar)), con, size = 4, endian = endian)
		}
		
	# Determining the sizes of the output data types:	
	datasizes = c(1, 1, 2, 4, 4, 8, 4, 8)
	datasizes = datasizes[match(header$dtyp, c("char", "byte", "shrt", "long", "floa", "doub", "cpxf", "cpxd"))]
	
	# For loop through the time steps and variables:
	### Note that the header has been stripped to the time steps that are to be written, whereas all time steps exist in the original data 'x'. Thus the index is 'p' in 'header$lvar' and 'singleindex', and thist = t[p] in 'x': ###
	for(p in seq_along(t)){
		thist = t[p]
		for(i in var){
			if(header$lvar[p, i]!= 0){
				# Write characters:
				if(header$dtyp[i] == "char"){
					if(!is.character(x[[i]][thist])){
						x[[i]][thist] = as.character(x[[i]][thist])
						}
					writeChar(x[[i]][thist], con, nchars=header$lvar[p, i], eos = NULL)
					}
				# If a list is given for each variable:
				else if(listtype[i]){
					# If the variable is of integer type but not specified as such, convert to double:
					if(!header$dtyp[i] %in% c("shrt", "long") && is.integer(x[[i]][[thist]])){
						x[[i]][[thist]] = as.double(x[[i]][[thist]])
						}
					# If the variable is not of integer type but specified as such, convert to integer:
					else if(header$dtyp[i] %in% c("shrt", "long") && !is.integer(x[[i]][[thist]])){
						x[[i]][[thist]] = as.integer(x[[i]][[thist]])
						}
					# If complex, collapse to a vector with real first then imaginary:
					if(arecomplex[i]){
						writeBin(c(Re(x[[i]][[thist]]), Im(x[[i]][[thist]])), con, size = datasizes[i], endian = endian)
						}
					else{
						writeBin(c(x[[i]][[thist]]), con, size = datasizes[i], endian = endian)
						}
					}
				else{
					# If the variable is of integer type but not specified as such, convert to double:
					if(!header$dtyp[i] %in% c("shrt", "long") && is.integer(x[[i]])){
						x[[i]] = as.double(x[[i]])
						}
					# If the variable is not of integer type but specified as such, convert to integer:
					else if(header$dtyp[i] %in% c("shrt", "long") && !is.integer(x[[i]])){
						x[[i]] = as.integer(x[[i]])
						}
					# If complex, collapse to a vector with real first then imaginary:
					if(arecomplex[i]){
						writeBin(c(Re(x[[i]][(singleindex[i, thist]+1):singleindex[i, thist+1]]), Im(x[[i]][(singleindex[i, thist]+1):singleindex[i, thist+1]])), con, size = datasizes[i], endian = endian)
						}
					else{
						writeBin(c(x[[i]][(singleindex[i, thist]+1):singleindex[i, thist+1]]), con, size = datasizes[i], endian = endian)
						}
					}
				}
			}
		}
	close(con)
	
	##### Output #####
	# Return the number of bytes (may be lower than the actual number of bytes if blank characters are present in some of the character strings written):
	bytes = 4*sum(length(header$nvar), length(header$numt), length(header$r000), length(header$lvar), length(header$labl), length(header$dtyp)) + sum(header$lvar[, var]*datasizes[var])
	##################################################
	##################################################
	}
