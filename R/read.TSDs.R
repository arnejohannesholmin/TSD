#*********************************************
#*********************************************
#' Reads files using the Time Step Data format and outputs as an R-list object where the elements are named according to the four character names provided in the file 'con'. The output list is stripped of duplicate names if clean==TRUE. See the documentation on echoIBM for specification of the Time Step Data format (TSD). Header not returned as it may differ between files.
#'
#' @param files		is a vector of the TSD files or a directory containing TSD files (recursive).
#' @param t			is a vector of the time steps to read, or a list of time step vectors, if different time steps are to be read for each file. If t=="all", all time steps are read, and if t=="none" only the header is returned, (or if header==FALSE list() is returned).
#' @param var		is a vector of the variables to read, either given as a character vector holding the names of the variables to read, as specified in the TSD-format, or as the number of the variable in 'labl', if 'labl' is known. If var="all", all variables are read. If none of the elements of 'var' are in [1, nvar], or if var=="none", (or if header==FALSE list() is returned). See read.TSD() for legal variable names.
#' @param dimension	is TRUE to return data with dimension.
#' @param header	is TRUE if the number 'nvar', the lengths 'lvar', the labels 'labl', the types 'dtyp' of the variables, and the number 'numt' of time steps are to be returned (at the end of the output list). If the header (list of variables nvar, labl, lvar, dtyp, numt, endian) of the file is known or has already been read, time can be saved by setting header equal to the existing header, in which case the header will not be read.
#' @param clean		is is TRUE to delete duplicatedly named elements. If clean is NULL, data read from multiple files are returned in a list with one element per file.
#' @param merge		is is TRUE to merge all elements of equal names to one vector/matrix/list:.
#' @param indt		is TRUE if the time points specified in 't' are to be cross referenced to time indexes 'indt' in the data, if present.
#' @param recursive	used if a directory is given in 'files', determining whether listing of files should listing recurse into directories.
#' @param drop.out	is TRUE to drop empty dimensions of arrays and unlist lists of length 1.
#' @param keep.all	is TRUE to return all variables for all files, even though no data are read (useful when reading one variable, and which files contain that variable is important). Also used in read.TSD().
#' @param info		is TRUE if the variable 'info' returned from info.TSD() should be added to the output (applied in read.TSD()).
#' @param addNvar	is TRUE if a vector with the index number of the file from which each variable is read should be added to the output.
#' @param silent	is FALSE to print progress bar. If the 'silent' should be set to FALSE in general, use options(verbose=TRUE).
#' @param msg		is TRUE to print the time bar of the TSD-files.
#' @param use.raw	is TRUE as default to read files quicker by reading the information as raw, and converting to the appropriate data types aferwards, which olny applies if the number of time steps exceed 20 and the number of values of the file does not exceed 'use.raw'.
#' @param cores		is an integer specifying the number of cores to read the files over in parallel (should be lower than the number of cores in the computer).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom parallel detectCores makeCluster stopCluster clusterSplit
#' @importFrom pbapply pblapply pboptions
#'
#' @export
#' @rdname read.TSDs
#'
read.TSDs <- function(files, t=1, var="all", dimension=TRUE, header=FALSE, clean=TRUE, merge=FALSE, indt=FALSE, recursive=TRUE, drop.out=TRUE, keep.all=FALSE, info=FALSE, addNvar=FALSE, silent=!getOption("verbose"), msg=FALSE, use.raw=1e3, cores=1, reorderFiles=TRUE, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-16 - Clean version.
	# Update: 2010-09-02 - Added the options 'header', for returning header objects, and 'clean' to be set to TRUE if elements of the output having duplicated names are to be kept. Also 'files' changed to possibly being a directory.
	# Update: 2010-10-5 - Removed NULL elements (elements of length 0).
	# Update: 2010-11-15 - Added the option 'indt'.
	# Update: 2010-11-19 - Added the options 'all.list' and 'recursive'.
	# Update: 2010-11-19 - Removed the option 'recursive', which was used when merge=TRUE due to a misunderstanding. Also the method for merging lists was changed and improved to return arrays if all duplicated elements have the same dimension up to the last dimension, and lists if else.
	# Update: 2011-06-26 - Added the option 'recursive'.
	# Update: 2011-10-21 - Added the function merge_TSD().
	# Update: 2012-07-31 - Added the option 'r000' for specifying the number of time steps. This is used when reading 'lvar' and reduces CPU time.
	# Update: 2012-12-12 - Added info to the output.
	# Update: 2013-05-07 - Added the parameters 'info' and 'addNvar' controling, respectively, whether 'info' or a vector of the index number of the file from which each variable is read, should be added to the output.
	# Update: 2013-05-13 - Fixed bugs with nvarFile.
	# Update: 2013-12-04 - Added 'msg'.
	# Update: 2013-12-09 - Fixed bug with 'keep.null'.
	# Update: 2013-12-09 - Changed 'keep.null' to return a field in the list for each time each variable is attempted read.
	# Update: 2016-07-25 - Removed 'keep.null', using keep.all instead.
	# Last: 2017-05-03 - Added the option clean=NULL, which returns the data from multiple files in a list with one element per file.
	

	##################################################
	##################################################
	##### Preparation #####
	read.TSD_one <- function(i, files, t, var, dimension, header, indt, keep.all, drop.out, info, silent){
		# Keep empty dimensions if files are to be merged:
		this <- suppressWarnings(read.TSD(con=files[i], t=t[[i]], var=var, dimension=dimension, header=header, indt=indt, keep.all=keep.all, drop.out=if(merge && lfiles>1) FALSE else drop.out, info=info, silent=silent, use.raw=1e3))
		# Add empty list elements for unread variables if keep.all=TRUE:
		if(keep.all){
			thisfull = vector("list", length(var))
			names(thisfull) = var
			thisfull[names(this)] = this
			this = thisfull
		}
		this
	}
	
	
	# 'files' may be given as a directory:
	if(length(files)==0){
		return(list())
	}
	if(length(files)==1 && is.na(file.info(files)$isdir)){
		stop("Invalid file")
		}
	else if(length(files)==1 && file.info(files)$isdir){
		files = list.files(files, recursive=recursive, full.names=TRUE)
	}
	# Only read TSD files:
	#files = files[is.TSD(files)]
	lfiles = length(files)
	
	# The time steps need to be in a list:
	if(!is.list(t)){
		t = rep(list(t), lfiles)
	}
	# Read only the files for which t has positive length:
	nonempty = sapply(t, length)>0
	t = t[nonempty]
	files = files[nonempty]
	
	##### Execution and output #####
	nvarFile = NULL
	out = vector("list",lfiles)
	
	
	
	
	# Parallel processing using the pblapply() function in the pbapply package:
	l = seq_along(files)
	if(cores>1){
		# Detect the number of cores and use the minimum of this and the number of requested cores:	
		cores = min(cores, length(files), detectCores())
		
		# Order the files so that files are read as close to each other as possible (reading in the order 1:L for L files if all files are read equally long):
		if(reorderFiles){
			cc = lapply(parallel::splitIndices(length(l), cores), function(i) l[i])
			cc = unlist(lapply(seq_len(cores), function(xx) seq(xx, by=cores, l=length(cc[[xx]]))), use.names=FALSE)
			l = l[rank(cc)]
		}
	}
	
	# Progress bar parallel processing (if cores>1):
	if(!msg){
		pbo <- pboptions(type = "none")
		on.exit(pboptions(pbo))
	}
	else{
		if(cores>1){
			cat("Reading TSD files in parallel on", cores, "cores:\n")
		}
		else{
			cat("Reading TSD files:\n")
		}
	}
	out <- pblapply(l, read.TSD_one, files=files, t=t, var=var, dimension=dimension, header=header, indt=indt, keep.all=keep.all, drop.out=if(merge && lfiles>1) FALSE else drop.out, info=info, silent=silent, cl=cores)
	out = out[order(l)]
	
	
	
	
	### if(cores>1){
	### 	# Generate the clusters of time steps:
	### 	cores = min(cores, length(files))
	### 	cl<-makeCluster(cores, outfile="")
	### 	# Order the files so that files are read as close to each other as possible (reading in the order 1:L for L files if all files are read equally long):
	### 	seqFiles = seq_along(files)
	### 	# This produced errors, since the the order was fucked up (intentionally, to assure reading from positions in close proximity on a drive):
	### 	cc = clusterSplit(cl, seqFiles)
	### 	l = unlist(lapply(seq_len(cores), function(xx) seq(xx, by=cores, l=length(cc[[xx]]))), use.names=FALSE)
	### 	l = seqFiles[rank(l)]
	### 	# Read all the .pings files, and merge at each time step:
	### 	cat("Parallel reading on", cores, "cores:\n")
	### 	out = parLapply(cl, l, read.TSD_one, files=files, t=t, var=var, dimension=dimension, header=header, indt=indt, keep.all=keep.all, drop.out=if(merge && lfiles>1) FALSE else drop.out, info=info, silent=silent)
	### 	out = out[order(l)]
	### 	#out = parLapply(cl, seq_along(files), read.TSD_one, files=files, t=t, var=var, dimension=dimension, header=header, indt=indt, keep.all=keep.all, drop.out=if(merge && lfiles>1) FALSE else drop.out, info=info, silent=silent)
	### 	# End the parallel processing:
	### 	stopCluster(cl)
	### 	}
	### else{
	### 	for(i in seq_along(files)){
	### 		# Print a dot if the floor of the new value exceeds the old value:
	### 		if(msg){
	### 			thisvalue = floor(i*stepfact)
	### 			if(thisvalue > oldvalue){
	### 				cat(rep(".",thisvalue-oldvalue),if(i==totalsteps) "\n", sep="")
	### 				oldvalue = thisvalue
	### 				}
	### 			}
	### 		# Keep empty dimensions if files are to be merged:
	### 		this <- read.TSD_one(i, files, t, var, dimension, header, indt, keep.all, drop.out, info, silent)
	### 		#this<-suppressWarnings(read.TSD(con=files[i], t=t[[i]], var=var, dimension=dimension, header=header, indt=indt, keep.all=keep.all, drop.out=if(merge && lfiles>1) FALSE else drop.out, info=info, silent=silent, use.raw=1e3))
	### 		# Add empty list elements for unread variables if keep.all=TRUE:
	### 		#if(keep.all){
	### 		#	thisfull = vector("list", length(var))
	### 		#	names(thisfull) = var
	### 		#	thisfull[names(this)] = this
	### 		#	this = thisfull
	### 		#	}
	### 		# Add to the output
	### 		out[[i]] = this
	### 		}
	### 	}
	
	if(length(out)==0){
		return(list())
	}
	if(length(clean)==0){
		names(out) <- files
		return(out)
	}
	else{
		out = unlist(out, recursive=FALSE)
	}
	
	# Remove NULL elements:
	if(!keep.all){
		notnullelements = sapply(out, function(x) length(x)>0)
		if(length(notnullelements)>0){
			out = out[notnullelements]
		}
	}
	
	# Keep header variables if requested:
	if(isTRUE(header)){
		var = c(var,c("nvar","numt","r000","labl","lvar","dtyp","endian"))
		}
	# Remove irrelevant elements:
	#if(!any(c("all", "beams", "ctd", "vessel", "voxels", "school", "time") %in% var)){
	#	out[!names(out) %in% var] = NULL
	#	}
	groupvar = c("all", "pings", "beams", "ctd", "vessel", "voxels", "school", "time")
	if(any(groupvar %in% var)){
		out[names(out) %in% groupvar] = NULL
	}
	
	# If 'clean' is TRUE, duplicatedly named elements are removed, and if 'merge' is TRUE all elements of equal names are merged to one vector or matrix or list:
	if(isTRUE(merge)){
		# If 'indt' is present in the data, and indt = TRUE, collapse by removing duplicated time steps for variables with length 1 per time step:
		out = merge_TSD(out, indt=indt, ...)
	}
	if(isTRUE(clean)){
		out = out[!duplicated(names(out))]
	}
	out
	##################################################
	##################################################
	}
