#*********************************************
#*********************************************
#' Merges either (1) TSD files given by 'x', or (2) four character named TSD variables in the list 'x', as returned when reading mutiple TSD files. List elements of identical names are collapsed.
#'
#' @param x  is a list of TSD-data, or a directory or vector of TSD-files to be merged, in which case a new directory is created with the merged data.
#' @param dir  is the directory in which to put the merged file(s).
#' @param indt  is used then merging TSD-data in the list 'x', causing values for duplicated time steps to be discarded (the variable 'indt' must be present, one for each file/group of data).
#' @param reserve  is FALSE if time steps should not be reserved, requiring that the lengths and dimensions of the variables in the files to merge match exactly.
#' @param recursive  is used when listing the files to be merged, and if set to TRUE files in subfolders will be included as well.
#' @param test.TSD  is TRUE to discard files that are not TSD files (could be time consuming).
#' @param filesize  is the maximum size of the merged files.
#' @param chunksize  is the maximum size of the chunks of file read at the time.
#' @param clear_along  is TRUE to clear files that have been merged imediately after merging.
#' @param drop.out  is TRUE to clear files that have been merged imediately after merging.
#' @param adds  is TRUE to clear files that have been merged imediately after merging.
#' @param linked  is a list of file paths of the same length as 'x' (or the file list of 'x'), holding files that should be merged using the same file grouping as used for 'x'.
#' @param skipLast  is TRUE to discard the last file in the merging, and simply copy is to the merged-directory.
#' @param ...  are possible inputs to other functions.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom utils tail
#'
#' @export
#' @rdname merge_TSD
#'
merge_TSD<-function(x, dir=NULL, indt=FALSE, reserve=TRUE, recursive=FALSE, test.TSD=FALSE, filesize=3e8, chunksize=3e8, clear_along=FALSE, drop.out=FALSE, adds=NULL, msg=TRUE, linked=list(), skipLast=FALSE, cores=1, keep.lists=FALSE, pad=TRUE, ...){
	#merge_TSD<-function(x, dir=NULL, indt=FALSE, reserve=TRUE, recursive=FALSE, #test.TSD=FALSE, filesize=3e8, chunksize=1e8, clear_along=FALSE, drop.out=FALSE, adds=NULL, fileGroups=NULL, numt=NULL, ...){
	
	############### LOG: ###############
	# Start: 2011-10-21 - Clean version.
	# Update: 2011-11-29 - Fixed bu where names(x)[i] was used instead of unames[i] in the first line of the for loop. Also changed to regard one-dimensional vectors as column vectors.
	# Update: 2014-10-20 - Speeded up the function.
	# Update: 2014-12-11 - Added 'chunksize'.
	# Update: 2015-01-19 - Changed to print (or not print) dots after each step in the double for loop at the end.
	# Update: 2015-05-15 - Fixed bug with empty elements.
	# Update: 2015-10-05 - Removed option of keeping duplicated time steps, which is now assumed if indt=FALSE.
	# Last: 2016-07-25 - Added the parameter 'linked', which is a list of file paths of the same length as 'x' (or the file list of 'x'), holding files that should be merged using the same file grouping as used for 'x'.
	

	##################################################
	##################################################
	# Function that merges one file group:
	merge_one <- function(files, inputMergeFile, dir, fileGroups, i, j, indt, drop.out, adds, reserve, clear_along, skipLast, numt){
		thisfiles = files[fileGroups[[i]][[j]]]
		# Simply move the last file if specified (the last file is added to the output list of files):
		last = NULL
		if(skipLast && length(files) %in% fileGroups[[i]][[j]]){
			last = file.path(dir, basename(tail(thisfiles, 1)))
			file.rename(tail(thisfiles, 1), last)
			# The rest of the files are merged as usual:
			thisfiles = thisfiles[-length(thisfiles)]
		}
		
		# If only one file is given, simply copy it to the merged-directory:
		if(length(thisfiles)==0){
			return(last)
		}
		
		# Read file:
		thisdata = read.TSDs(thisfiles, t="all", info=FALSE, merge=TRUE, keep.all=TRUE, indt=indt, drop.out=drop.out, msg=FALSE, pad=pad)
		# This is done inside read.TSDs!!!!!!:
		#thisdata = lapply(thisdata, mergeListKeepDimensions, pad=pad)
		
		if(length(adds)>0){
			thisdata[names(adds)] = adds
		}
		# Write merged file:
		write.TSD(thisdata, inputMergeFile, numt=if("indt" %in% names(thisdata)) length(thisdata$indt) else NULL, append=j>1, reserve=if(reserve && j==1) sum(unlist(numt[[i]][-1], use.names=FALSE)) else 0)
		if(clear_along){
			unlink(files[fileGroups[[i]][[j]]])
		}
		c(inputMergeFile, last)
	}
	# Function for merging all files in a file group, using merge_one() and allowing the same merging of other linked files. This funciton allows for parallel merging, spreading the load of the file groups on multiple cores:
	#mergeOneFileGroup <- function(i, dir, fileGroups, files, indt, drop.out, adds, reserve, clear_along, skipLast, linked, msgfun){
	mergeOneFileGroup <- function(i, dir, fileGroups, files, indt, drop.out, adds, reserve, clear_along, skipLast, linked, filenames_out, numt){
		thesefiles <- files[unlist(fileGroups[[i]])]
		inputMergeFile <- file.path(dir, basename(files[fileGroups[[i]][[1]]][1]))
		if(length(thesefiles)==1){
			file.copy(thesefiles, inputMergeFile)
			if(clear_along){
				unlink(thesefiles)
			}
			# Print all dots of the current file group:
			#msgfun(cbind(i,seq_along(fileGroups[[i]])))
		}
		else{
			for(j in seq_along(fileGroups[[i]])){
				# Print the progress bar:
				#msgfun(c(i,j))
				
				# Merge the files of the current file group, for 'files' and 'linked' (which are files that are linked with 'files', and are merged by the same grouping):
				mergeFileInd = fileGroups[[i]][[1]][1]
				
				outputMergeFile = merge_one(files=files, inputMergeFile=inputMergeFile, dir=dir[1], fileGroups=fileGroups, i=i, j=j, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, skipLast=skipLast, numt=numt)
				
				filenames_out[[1]][mergeFileInd + seq(0, length(outputMergeFile)-1)] = outputMergeFile
				if(length(linked)){
					for(l in seq_along(linked)){
						inputMergeFile <- file.path(dir, basename(files[fileGroups[[i]][[1]]][1]))
						outputMergeFile = merge_one(files=linked[[l]], inputMergeFile=inputMergeFile, dir=dir[1], fileGroups=fileGroups, i=i, j=j, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, skipLast=skipLast, numt=numt)
						filenames_out[[1+l]][mergeFileInd + seq(0, length(outputMergeFile)-1)] = outputMergeFile
					}
				}
			}
		}
		filenames_out
	}
		
	if(is.list(x)){
		# Discard variable info:
		x = x[names(x)!= "info"]
		##### Preparation #####
		# Extract the unique names of the output, the number of elements of the unique names and a vector of which of the unique names there are more than one element:
		unames = unique(names(x))
		numnames = table(match(names(x), unames))
		dupnames = which(numnames>1)
		# Only apply the time step indices if 'indt' is present:
		indt = indt & length(x$indt)>0
		
		# Get the duplicated time steps, and set 'keepindt' to discard the duplicated time steps:
		len_indt = unlist(lapply(x[names(x) == "indt"], length), use.names=FALSE)
		numt = sum(len_indt)
		if(indt){
			keepindt = split(!duplicated(unlist(x[names(x) == "indt"], use.names=FALSE)), rep(seq_along(len_indt), len_indt))
			if(length(keepindt)){
				numt = sum(sapply(keepindt, sum))
			}
		}
		
		##### Execution and output #####
		# For loop through the duplicated names, merging the elements of the same name. This is done either by merging into an array along the last dimension, if all elements of the same name are arrays of equal dimension for all but the last dimension, or else by assigning list to all arrays if present, and merging together all the individual lists:
		for(i in dupnames){
			theseind = which(names(x) == unames[i])
			olddim = lapply(x[theseind], dim_all)
			# If several time steps are read from some of the TSD-files, and the length of the variable varies between time steps, the data are returned as a list for that variable:
			#arelists = sapply(olddim, is.list)
			arelists = sapply(x[theseind], is.list)
			numdim = unlist(lapply(olddim, length), use.names=FALSE)
			equalnumdim = all(numdim == numdim[1])
			allbutlast = lapply(olddim, function(xx) if(length(xx) == 1) xx else xx[-length(xx)])
			equaldim = lapply(allbutlast, function(xx) identical(xx, allbutlast[[1]]))
			equaldim = all(unlist(equaldim, use.names=FALSE))
			
			# ATTEMPT TO DISCARD DUPLICATED TIME STEPS:
			if(indt && equalnumdim){
				# Discard duplicated time steps:
				for(j in seq_along(theseind)){
					if(sum(keepindt[[j]])>0 && mean(keepindt[[j]])<1){
						x[[theseind[j]]] = extractIndSubset(x[[theseind[j]]], ind=list(which(keepindt[[j]])), pad="start")
					}
					else if(sum(keepindt[[j]]) == 0){
						x[theseind[j]] = list(NULL)
					}
				}
			}
			
			# If all elements are arrays of equal dimension up to the last dimension, merge into an array:
			if(!any(arelists)){
				# This is the old method, kept for reference, and in case there are unexpected problems occuring due to the new method using mergeListKeepDimensions():
				if(keep.lists){
					if(equaldim){
						# Unlist into a vector:
						merged = unlist(x[theseind], use.names=FALSE, recursive=TRUE)
						if(length(merged)){
							x[[theseind[1]]] = merged
						}
						# Remove the rest of the fields:
						x[theseind[-1]] = list(NULL)
						if(length(x[[theseind[1]]])>0){
							dim(x[[theseind[1]]]) = c(allbutlast[[1]], length(x[[theseind[1]]])/prod(allbutlast[[1]]))
						}
						# Drop dimensions of variables with only one value per ping
						if(prod(allbutlast[[1]]) == 1){
							dim(x[[theseind[1]]]) = NULL
						}
					}
					else{
						# Get the new lengths of the variable:
						newdim = lapply(x[theseind], dim_all)
						newlength = sum(unlist(lapply(newdim, prod), use.names=FALSE))
						# If there is one value per time step, simply unlist the data:
						if(newlength == numt || numt==0 && length(newdim[[1]])==1){
							# Unlist into a vector:
							merged = unlist(x[theseind], use.names=FALSE, recursive=TRUE)
							if(length(merged)){
								x[[theseind[1]]] = merged
							}
							# Remove the rest of the fields:
							x[theseind[-1]] = list(NULL)
						}
						# Otherwise, drop empty dimensions
						else{
							if(drop.out){
								x[[theseind[1]]] = lapply(x[theseind], drop)
								}
							names(x[[theseind[1]]]) = NULL
						}
					}
				}
				else{
					x[[theseind[1]]] = mergeListKeepDimensions(x[theseind], pad=pad, add1=FALSE, split=TRUE)
					# Remove the rest of the fields:
					x[theseind[-1]] = list(NULL)
				}
			}
			# Else make lists of the non-list elements, and merge the lists together:
			else{
				if(!all(arelists)){
					x[theseind] = lapply(x[theseind], function(x) if(is.list(x)) x else list(x))
				}
				x[[theseind[1]]] = unlist(x[theseind], use.names=FALSE, recursive=FALSE)
			}
		}
		# Return only the collapsed x:
		if(length(adds)>0){
			x[names(adds)] = adds
		}
		x[unames]
	}
	
	# If 'x' is given as a directory or a vector of files, read the files in batches, and write merged files:
	else{
		# 'x' may be given as a directory:
		if(length(x) == 1 && is.na(file.info(x)$isdir)){
			stop("Invalid file")
		}
		else if(length(x) == 1 && file.info(x)$isdir){
			x = list.files(x, recursive=recursive, full.names=TRUE)
		}
		# Discard non-existing files:
		x = x[file.exists(x)]
		# Only read TSD files:
		if(test.TSD){
			x = x[is.TSD(x)]
		}
		lx = length(x)
		
		# Place new files in a different directory:
		if(length(dir) == 0){
			dir = dirname(x[1])
			if(dir=="."){
				dir=""
			}
			dir = paste(dir, "_merged", sep = "")
		}
		# If the merged directory is the same as the original, set clear_along to FALSE:
		if(dir == dirname(x[1])){
			clear_along = FALSE
		}
		suppressWarnings(dir.create(dir[1]))
		
		#x_merged = file.path(dir[1], basename(x))
		
		# Get file sizes:
		s = file.info(x)$size
		cs = cumsum(s)
		
		# Merge the time steps:
		infostring = paste("Merging files:")
		cat(infostring, "\n", sep = "")
		totalsteps = length(x)
		totalsize = tail(cs, 1)
		#stepfact = nchar(infostring)/totalsteps
		stepfact = nchar(infostring)/totalsize
		oldvalue = 0
		
		# Scan through the files and get the number of time steps of each file:
		numt = unlist(read.TSDs(x, var="numt", clean=FALSE, merge=FALSE, msg=FALSE), use.names=FALSE)
		# Group the files by file size:
		fileGroups = ceiling(cs/filesize)
		numt = split(numt, fileGroups)
		fileGroups = split(seq_along(x), fileGroups)
		# For each file group, group files in chunks of at least one file:
		for(i in seq_along(fileGroups)){
			theseFileChunks = ceiling(cumsum(s[fileGroups[[i]]])/chunksize)
			fileGroups[[i]] = split(fileGroups[[i]], theseFileChunks)
			numt[[i]] = split(numt[[i]], theseFileChunks)
		}
		
		##### Run through the time steps, and merge the files: #####
		filenames_out = rep(list(NAs(length(x))), 1 + length(linked))
		
		#msgfun <- getMSG(list(msg=paste0("Merging ", length(x), " files:"), len=c(length(fileGroups), )))
		
		#out <- papply(seq_along(fileGroups), mergeOneFileGroup, dir=dir, fileGroups=fileGroups, files=x, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, skipLast=skipLast, linked=linked, msgfun=msgfun, cores=cores)
		### filenames_out <- papply(seq_along(fileGroups), mergeOneFileGroup, dir=dir, fileGroups=fileGroups, files=x, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, skipLast=skipLast, linked=linked, filenames_out=filenames_out, cores=cores)
		
		
		
		# Parallel processing using the pblapply() function in the pbapply package:
		if(cores>1){
			# Detect the number of cores and use the minimum of this and the number of requested cores:	
			cores = min(cores, length(fileGroups), detectCores())
		}
		# Progress bar parallel processing (if cores>1):	
		cat("Merging TSD files:\n")
		out <- pblapply(seq_along(fileGroups), mergeOneFileGroup, dir=dir, fileGroups=fileGroups, files=x, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, skipLast=skipLast, linked=linked, filenames_out=filenames_out, numt=numt, cl=cores)	
		
		
		
		
		
		#if(cores==1 || length(fileGroups)==1){
		#	mergeOneFileGroup(i=i, dir=dir, fileGroups=fileGroups, x=x, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, cores=cores, skipLast=skipLast, linked=linked, msgfun=msgfun)
		#}
		#else{
		#	cores <- min(parallel::detectCores(), cores, length(fileGroups))
		#	cl <- parallel::makeCluster(cores, outfile="")
		#	out <- parallel::parLapply(cl, seq_along(fileGroups), mergeOneFileGroup, dir=dir, fileGroups=fileGroups, x=x, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, cores=cores, skipLast=skipLast, linked=linked, msgfun=msgfun)
		#	parallel::stopCluster(cl)
		#}
		
		#if(length(fileGroups) == 1 && length(fileGroups[[1]]) == 1){
		#	msg = FALSE
		#	}
		#for(i in seq_along(fileGroups)){
		#	# Print a dot if the floor of the new value exceeds the old value:
		#	#thisvalue = floor(cumNumFilesInGroups[i]*stepfact)
		#	#if(thisvalue > oldvalue){
		#	#	cat(rep(".", thisvalue-oldvalue), if(i == totalsteps) "\n", sep="")
		#	#	oldvalue = thisvalue
		#	#	}
		#		
		#	thesefiles <- unlist(fileGroups[[i]])		
		#	mergedfile <- file.path(dir, basename(x[fileGroups[[i]][[1]]][1]))
		#	if(length(thesefiles)==1){
		#		file.copy(thisfiles, mergedfile)
		#	}
		#	else{
		#		for(j in seq_along(fileGroups[[i]])){
		#			# Print a dot if the floor of the new value exceeds the old value, but not if msg == TRUE, which implies that the dots should be plotted inside read.TSDs() instead:
		#			if(msg){
		#				thisvalue = floor(tail(cs[fileGroups[[i]][[j]]], 1)*stepfact)
		#				if(thisvalue > oldvalue){
		#					cat(rep(".", thisvalue-oldvalue), if(i == totalsteps) "\n", sep="")
		#					oldvalue = thisvalue
		#					}
		#				}
		#			# Merge the files of the current file group, for 'x' and 'linked' (which are files that are linked with 'x', and are merged by the same grouping):
		#			mergeFileInd = fileGroups[[i]][[1]][1]
		#			
		#			theseMergedFiles = merge_one(files=x, mergedfile=mergedfile, dir=dir[1], fileGroups=fileGroups, i=i, j=j, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, cores=cores, skipLast=skipLast)
		#			filenames_out[[1]][mergeFileInd + seq(0, length(theseMergedFiles)-1)] = theseMergedFiles
		#			if(length(linked)){
		#				for(l in seq_along(linked)){
		#					theseMergedFiles = merge_one(files=linked[[l]], dir=dir[1], fileGroups=fileGroups, i=i, j=j, indt=indt, drop.out=drop.out, adds=adds, reserve=reserve, clear_along=clear_along, cores=cores, skipLast=skipLast)
		#					filenames_out[[1+l]][mergeFileInd + seq(0, length(theseMergedFiles)-1)] = theseMergedFiles
		#					}
		#				}
		#			}
		#		}
		#	}
			
			
			
		
				
				
				
				
		
		cat("\n")
		
		out <- do.call(rbind, lapply(out, as.data.frame, stringsAsFactors=FALSE))
		names(out) <- c("Merged", if(length(linked)) paste0("Linked", seq_along(linked)))
		out = lapply(out, rm.na)
		
		
		### filenames_out = lapply(filenames_out, rm.na)
		### filenames_out = lapply(out, rm.na)
		#fileGroups = lapply(fileGroups, unlist)
		#fileGroupsN = sapply(fileGroups, length)
		#x = split(x, rep(seq_along(fileGroupsN), fileGroupsN))
		#x_merged = x_merged[sapply(fileGroups, function(xx) head(unlist(xx), 1))]
		#invisible(list(x=x, x_merged=x_merged, fileGroups=fileGroups, numt=numt))
		#x_merged
		if(length(linked)){
			out
		}
		else{
			out[[1]]
		}
	}
	##################################################
	##################################################
}
