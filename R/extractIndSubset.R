#*********************************************
#*********************************************
#' Extracts a subset of 'x' defined by the index list or vector 'ind' and/or the logical vector 'subset'.
#'
#' @param x  is the list or array to be extracted from.
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting. Can be given as a list with subsetting indices for each time step.
#' @param drop  is TRUE if dimensions of only one level is to be removed from the output.
#' @param ind.out  is TRUE if the one dimensinal vector indexes used in the extraction is to be returned.
#' @param insert.NA  is TRUE if the discarded data are to be kept as NA.
#' @param only.match  is TRUE if only the arrays of length equal to the length of 'subset' are to be subsetted using 'subset'.
#' @param return.all  is FALSE to return empty data and indices when 'ind' and 'subset' are enpty.
#' @param pad  determines at which end of the dimensions to pad with zeros if the length of 'ind' is shorter than the number of dimensions. Using pad="start" applies 'ind' on the last dimensions.	
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail
#'
#' @export
#' @rdname extractIndSubset
#'
extractIndSubset <- function(x, ind=list(), subset=NULL, drop=TRUE, ind.out=FALSE, insert.NA=FALSE, only.match=FALSE, return.all=TRUE, pad=c("end","start")){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-27 - Finished.
	# Update: 2009-05-13 - Changed from having input subscript 'ind' to '...'. A problem with extractIndSubset(x,), which gives an error as '...' is empty, is unsolved.
	# Update: 2009-05-13 - Changed in method from assigning indexes to 'x' to using arr.ind2ind().
	# Update: 2011-05-20 - Added the option 'only.match' for subsetting only the arrays of the same length as 'subset'.
	# Update: 2011-09-24 - Changed to separate into 4 cases, depending on whither 'ind' and 'subset' are given. Also re-applied the old fashioned but fast method of a ten-fold conditional expression for the extraction using only 'ind'.
	# Update: 2012-06-21 - Added the function ind.expand() to treat the indexes given in 'ind'.
	# Update: 2012-07-30 - Changed to be an applicationd for extractIndSubset(), which supports lists.
	# Update: 2012-08-02 - Radical change in the code: The entire function is hardcoded without use of sub-funcrions like extract_array() and extract_list() Also support is given for lists, and list elements in the list.
	# Update: 2012-08-19 - Speeded up the funciton when multiple elements of the list input 'x' have equal dimension, by the variable 'dup'.
	# Update: 2012-08-21 - Fixed bug when inserting NAs.
	# Update: 2012-09-05 - Added support for list input of 'subset', where each list element corresponds to one time step organized along the last dimension of 'x'.
	# Update: 2012-09-20 - Added the option return.all, which should be set to FALSE to return empty data if both 'ind' and 'subset' are empty. Otherwise the unaltered data are returned (default).
	# Last: 2014-10-22 - Added 'pad'.
	########### DESCRIPTION: ###########
	# Extracts a subset of 'x' defined by the index list or vector 'ind' and/or the logical vector 'subset'.
	########## DEPENDENCIES: ###########
	# dim_all(), arr.ind2ind()
	############ VARIABLES: ############
	# ---x--- is the list or array to be extracted from.
	# ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
	# ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting. Can be given as a list with subsetting indices for each time step.
	# ---drop--- is TRUE if dimensions of only one level is to be removed from the output.
	# ---ind.out--- is TRUE if the one dimensinal vector indexes used in the extraction is to be returned.
	# ---insert.NA--- is TRUE if the discarded data are to be kept as NA.
	# ---only.match--- is TRUE if only the arrays of length equal to the length of 'subset' are to be subsetted using 'subset'.
	# ---return.all--- is FALSE to return empty data and indices when 'ind' and 'subset' are enpty.
	# ---pad--- determines at which end of the dimensions to pad with zeros if the length of 'ind' is shorter than the number of dimensions. Using pad="start" applies 'ind' on the last dimensions.	
	
	
	##################################################
	##################################################
	##### Preparation #####
	# If 'insert.NA' is given as a single numeric, this is inserted in stead of NA:
	if(identical(insert.NA,FALSE)){
		insert.NA=NULL
		}
	else if(isTRUE(insert.NA)){
		insert.NA=NA
		}
	
	# 'dup' is a list of indices used to reduce CPU time by copying the indices calculated using arr.ind2ind(). If dup[[i]] != i, the indices from position dup[[i]] are copied to the position 'i'. Differ between array and list elements of the list 'x':
	# Get the dimensions of the data:
	dim_allx=dim_all(x)
	lists=sapply(dim_allx,is.list)
	arrays=!lists
	# Identify equal dimensions:
	dup=dim_allx
	dup[arrays]=as.list(match(dim_allx[arrays],dim_allx[arrays]))
	dup[lists]=lapply(dim_allx[lists],function(y) match(y,y))
			
	# Run through the list elements and extract without subfunctions, in order to avoid unnessecary transfering of data:
	if(is.list(x)){
		# Store the names of the list:
		namesx=names(x)
		# The length of the list:
		lx=length(x)
		# Define the index output list, with indexes for each list element of 'x':
		indout=vector("list",lx)
		
		# For loop through the elements of the list 'x', which are assumed to be different variables:
		for(i in seq_len(lx)){
			
			########## If the element is a list: ##########
			if(is.list(x[[i]])){
				
				# Pass if any of the elements of x[[i]] are lists, which is not supported:
				if(!any(unlist(lapply(x[[i]],is.list)))){
					# Length of the current element in the list x[[i]]:
					lxi=length(x[[i]])
					# Apply the last element of 'ind' to the list itself, and apply the rest of 'ind' to each element of the list:
					array_ind=ind[-length(ind)]
					list_ind=ind[length(ind)]
					list_ind=unlist(ind.expand(list_ind,lxi,pad=pad))
					
					indout[[i]]=vector("list",lxi)
					
					# Apply on each element of the list, which are assumed to be time steps:
					if(length(list_ind)>0){
						for(j in list_ind){
							
							##### Preparation #####
							# Dimensions of 'x':
							dimx=dim_all(x[[i]][[j]])
							l=length(dimx)
							thisind=ind.expand(array_ind,dimx,pad=pad)
							
							##### Execution and output #####
							### 1 ###
							# If only 'ind' is given:
							if(length(thisind)>0 && (identical(subset,0) || length(subset)==0)){
								# Expand the 'ind' to fit the dimension of the data:
								if(length(insert.NA)>0){
									if(l==1){
										x[[i]][[j]][-thisind[[1]]]=insert.NA
										}
									if(l==2){
										x[[i]][[j]][-thisind[[1]],]=insert.NA
										x[[i]][[j]][,-thisind[[2]]]=insert.NA
										}
									if(l==3){
										x[[i]][[j]][-thisind[[1]],,]=insert.NA
										x[[i]][[j]][,-thisind[[2]],]=insert.NA
										x[[i]][[j]][,,-thisind[[3]]]=insert.NA
										}
									if(l==4){
										x[[i]][[j]][-thisind[[1]],,,]=insert.NA
										x[[i]][[j]][,-thisind[[2]],,]=insert.NA
										x[[i]][[j]][,,-thisind[[3]],]=insert.NA
										x[[i]][[j]][,,,-thisind[[4]]]=insert.NA
										}
									if(l==5){
										x[[i]][[j]][-thisind[[1]],,,,]=insert.NA
										x[[i]][[j]][,-thisind[[2]],,,]=insert.NA
										x[[i]][[j]][,,-thisind[[3]],,]=insert.NA
										x[[i]][[j]][,,,-thisind[[4]],]=insert.NA
										x[[i]][[j]][,,,,-thisind[[5]]]=insert.NA
										}
									}
								else{
									if(l==1){
										x[[i]][[j]]=x[[i]][[j]][thisind[[1]],drop=drop]
										}
									if(l==2){
										x[[i]][[j]]=x[[i]][[j]][thisind[[1]],thisind[[2]],drop=drop]
										}
									if(l==3){
										x[[i]][[j]]=x[[i]][[j]][thisind[[1]],thisind[[2]],thisind[[3]],drop=drop]
										}
									if(l==4){
										x[[i]][[j]]=x[[i]][[j]][thisind[[1]],thisind[[2]],thisind[[3]],thisind[[4]],drop=drop]
										}
									if(l==5){
										x[[i]][[j]]=x[[i]][[j]][thisind[[1]],thisind[[2]],thisind[[3]],thisind[[4]],thisind[[5]],drop=drop]
										}
									}
								if(ind.out){
									if(dup[[i]][[j]]!=j){
										indout[[i]][[j]]=indout[[i]][[ dup[[i]][[j]] ]]
										}
									else{
										indout[[i]][[j]]=arr.ind2ind(thisind,dimx)
										}
									}
								}
								
							### 2 ###
							# If only 'subset' is given:
							else if(length(thisind)==0 && !(identical(subset,0) || length(subset)==0)){
								
								# Selet the current subset if 'subset' is a list, where each element of the list refers to one time step, and where the time steps are asumed to be organized along the last dimension of 'x[[i]][[j]]':
								if(is.list(subset)){
									# Select the current subset:
									if((only.match && length(subset)==length(list_ind)) || !only.match){
										thissubset= subset[[j]]
										}
									}
								else{
									thissubset=subset
									}
								
								# Apply the current subset:
								if(is.logical(thissubset) && only.match){
									if(length(thissubset)==length(x[[i]][[j]])){
										indout[[i]][[j]]= thissubset
										}
									# If ind.out=TRUE, return the endite sequence along the data:
									else if(ind.out){
										indout[[i]][[j]]=seq_along(x[[i]][[j]])
										}
									}
								else{
									indout[[i]][[j]]=seq_along(x[[i]][[j]])[thissubset]
									}
								# Extract and reset the dimension:
								if(length(insert.NA)>0){
									x[[i]][[j]][-indout[[i]][[j]]]=insert.NA
									}
								else{
									x[[i]][[j]]=x[[i]][[j]][indout[[i]][[j]]]
									}
								}
							
							### 3 ###
							# If both 'ind' and 'subset' are given, transform 'ind' to indexes, and combine with 'subset':
							else if(length(ind)>0 && !(identical(subset,0) || length(subset)==0)){
								# Expand the 'ind' to fit the dimension of the data:
								thisind=ind.expand(array_ind,dimx,pad=pad)
								# Transform 'subset' to postitive numeric indexes:
								subset=seq_len(prod(dimx))[subset]
								# Dimension of 'ind':
								d=sapply(thisind,length)
								if(drop){
									d=d[d!=1]
									}
								# Get the vector indexes of 'ind':
								if(dup[[i]][[j]]!=j){
									indout[[i]][[j]]=indout[[i]][[ dup[[i]][[j]] ]]
									}
								else{
									indout[[i]][[j]]=arr.ind2ind(thisind,dimx)
									}
								# Intersect the indexes of 'ind' and 'subset':
								if(!all(only.match,length(subset)!=length(x[[i]][[j]]))){
									indout[[i]][[j]]=intersect(indout[[i]][[j]],subset)
									}
								# Extract and reset the dimension:
								if(length(insert.NA)>0){
									x[[i]][[j]][-indout[[i]][[j]]]=insert.NA
									}
								else{
									x[[i]][[j]]=x[[i]][[j]][indout[[i]][[j]]]
									# If 'subset' did not make a subset of the subset selected by 'ind':
									if(length(d)!=0 && prod(d)==length(indout[[i]][[j]])){
										dim(x[[i]][[j]])=d
										}
									}
								}
							
							### 4 ###
							# Else return the unaltered input (along with the full indexes if requested):
							else if(ind.out){
								if(return.all){
									indout[[i]][[j]]=seq_along(x[[i]][[j]])
									}
								else{
									x[[i]][j]=list(NULL)
									}
								}
							}
						}
					# Remove the list elements not included in 'list_ind'
					x[[i]]=x[[i]][list_ind]	
					}
				}
			
			########## If the element is an array: ##########
			else{
				##### Preparation #####
				# Dimensions of 'x':
				dimx=dim_all(x[[i]])
				l=length(dimx)
				thisind=ind.expand(ind,dimx,pad=pad)
				
				##### Execution and output #####
				### 1 ###
				# If only 'ind' is given:
				if(length(thisind)>0 && (identical(subset,0) || length(subset)==0)){
					if(length(insert.NA)>0){
						if(l==1){
							x[[i]][-thisind[[1]]]=insert.NA
							}
						if(l==2){
							x[[i]][-thisind[[1]],]=insert.NA
							x[[i]][,-thisind[[2]]]=insert.NA
							}
						if(l==3){
							x[[i]][-thisind[[1]],,]=insert.NA
							x[[i]][,-thisind[[2]],]=insert.NA
							x[[i]][,,-thisind[[3]]]=insert.NA
							}
						if(l==4){
							x[[i]][-thisind[[1]],,,]=insert.NA
							x[[i]][,-thisind[[2]],,]=insert.NA
							x[[i]][,,-thisind[[3]],]=insert.NA
							x[[i]][,,,-thisind[[4]]]=insert.NA
							}
						if(l==5){
							x[[i]][-thisind[[1]],,,,]=insert.NA
							x[[i]][,-thisind[[2]],,,]=insert.NA
							x[[i]][,,-thisind[[3]],,]=insert.NA
							x[[i]][,,,-thisind[[4]],]=insert.NA
							x[[i]][,,,,-thisind[[5]]]=insert.NA
							}
						}
					else{
						if(l==1){
							x[[i]]=x[[i]][thisind[[1]],drop=drop]
							}
						if(l==2){
							x[[i]]=x[[i]][thisind[[1]],thisind[[2]],drop=drop]
							}
						if(l==3){
							x[[i]]=x[[i]][thisind[[1]],thisind[[2]],thisind[[3]],drop=drop]
							}
						if(l==4){
							x[[i]]=x[[i]][thisind[[1]],thisind[[2]],thisind[[3]],thisind[[4]],drop=drop]
							}
						if(l==5){
							x[[i]]=x[[i]][thisind[[1]],thisind[[2]],thisind[[3]],thisind[[4]],thisind[[5]],drop=drop]
							}
						}
					if(ind.out){
						if(dup[[i]]!=i){
							indout[[i]]=indout[[dup[[i]]]]
							}
						else{
							indout[[i]]=arr.ind2ind(thisind,dimx)
							}
						}
					}
					
				### 2 ###
				# If only 'subset' is given:
				else if(length(ind)==0 && !(identical(subset,0) || length(subset)==0)){
					
					# Selet the current subset if 'subset' is a list, where each element of the list refers to one time step, and where the time steps are asumed to be organized along the last dimension of 'x[[i]]':
					if(is.list(subset)){
						if((only.match && length(subset)==tail(dimx,1)) || !only.match){
							# Get the length of a time step in x[[i]]:
							firstdims=max(1,prod(dimx[seq_len(length(dimx)-1)]))
							# Crop the subsets so that none are longer than or have higher indices than 'firstdims':
							thissubset=lapply(subset,function(subs) if(is.logical(subs)) subs[seq_len(firstdims)] else subs[subs<=firstdims])
							# Get the lengths of the subsets for each time step:
							lenthissubset=sapply(thissubset,length)
							# Add the lengths of the time steps to the subsets, collapsing all time steps into one vector of subsets fitting x[[i]]: 
							thissubset=unlist(thissubset) + rep(seq(0,length(thissubset)-1)*firstdims, lenthissubset)
							}
						}
					else{
						thissubset=subset
						}
					
					# Apply the current subset:
					if(is.logical(thissubset) && only.match){
						if(length(thissubset)==length(x[[i]])){
							indout[[i]]= thissubset
							}
						# If ind.out=TRUE, return the endite sequence along the data:
						else if(ind.out){
							indout[[i]]=seq_along(x[[i]])
							}
						}
					else{
						indout[[i]]=seq_along(x[[i]])[thissubset]
						}
					# Extract and reset the dimension:
					if(length(insert.NA)>0){
						x[[i]][-indout[[i]]]=insert.NA
						}
					else{
						x[[i]]=x[[i]][indout[[i]]]
						}
					}
				
				### 3 ###
				# If both 'ind' and 'subset' are given, transform 'ind' to indexes, and combine with 'subset':
				else if(length(ind)>0 && !(identical(subset,0) || length(subset)==0)){
					# Transform 'subset' to postitive numeric indexes:
					subset=seq_len(prod(dimx))[subset]
								
					# Dimension of 'ind':
					d=sapply(thisind,length)
					if(drop){
						d=d[d!=1]
						}
					# Get the vector indexes of 'ind':
					indout[[i]]=arr.ind2ind(thisind,dimx)
					# Intersect the indexes of 'ind' and 'subset':
					if(!all(only.match,length(subset)!=length(x[[i]]))){
						indout[[i]]=intersect(indout[[i]],subset)
						}
					# Extract and reset the dimension:
					if(length(insert.NA)>0){
						x[[i]][-indout[[i]]]=insert.NA
						}
					else{
						x[[i]]=x[[i]][indout[[i]]]
						# If 'subset' did not make a subset of the subset selected by 'ind':
						if(length(d)!=0 && prod(d)==length(indout[[i]])){
							dim(x[[i]])=d
							}
						}
					}
				
				### 4 ###
				# Else return the unaltered input (along with the full indexes if requested):
				else if(ind.out){
					if(return.all){
						indout[[i]]=seq_along(x[[i]])
						}
					else{
						x[i]=list(NULL)
						}
					}
				}
			}
		
		# Output:
		if(ind.out){
			names(x)=namesx
			names(indout)=namesx
			list(x=x,ind=indout)
			}
		else{
			names(x)=namesx
			x
			}
		}
	
	
	# If 'x' is an array:
	else{
		##### Preparation #####
		# Dimensions of 'x':
		dimx=dim_all(x)
		l=length(dimx)
		
		##### Execution and output #####
		### 1 ###
		# If only 'ind' is given:
		if(length(ind)>0 && (identical(subset,0) || length(subset)==0)){
			# Expand the 'ind' to fit the dimension of the data:
			ind=ind.expand(ind,dimx,pad=pad)
			if(length(insert.NA)>0){
				if(l==1){
					x[-ind[[1]]]=insert.NA
					}
				if(l==2){
					x[-ind[[1]],-ind[[2]]]=insert.NA
					}
				if(l==3){
					x[-ind[[1]],-ind[[2]],-ind[[3]]]=insert.NA
					}
				if(l==4){
					x[-ind[[1]],-ind[[2]],-ind[[3]],-ind[[4]]]=insert.NA
					}
				if(l==5){
					x[-ind[[1]],-ind[[2]],-ind[[3]],-ind[[4]],-ind[[5]]]=insert.NA
					}
				}
			else{
				if(l==1){
					x=x[ind[[1]],drop=drop]
					}
				if(l==2){
					x=x[ind[[1]],ind[[2]],drop=drop]
					}
				if(l==3){
					x=x[ind[[1]],ind[[2]],ind[[3]],drop=drop]
					}
				if(l==4){
					x=x[ind[[1]],ind[[2]],ind[[3]],ind[[4]],drop=drop]
					}
				if(l==5){
					x=x[ind[[1]],ind[[2]],ind[[3]],ind[[4]],ind[[5]],drop=drop]
					}
				}
			# Output:
			if(ind.out){
				list(x=x,ind=arr.ind2ind(ind,dimx))
				}
			else{
				x
				}
			}
			
		### 2 ###
		# If only 'subset' is given:
		else if(length(ind)==0 && !(identical(subset,0) || length(subset)==0)){
			# Selet the current subset if 'subset' is a list, where each element of the list refers to one time step, and where the time steps are asumed to be organized along the last dimension of 'x[[i]]':
			if(is.list(subset)){
				if((only.match && length(subset)==tail(dimx,1)) || !only.match){
					# Get the length of a time step in x[[i]]:
					firstdims=max(1,prod(dimx[seq_len(length(dimx)-1)]))
					# Crop the subsets so that none are longer than or have higher indices than 'firstdims':
					thissubset=lapply(subset,function(subs) if(is.logical(subs)) subs[seq_len(firstdims)] else subs[subs<=firstdims])
					# Get the lengths of the subsets for each time step:
					lenthissubset=sapply(thissubset,length)
					# Add or subtract the lengths of the time steps to the subsets, collapsing all time steps into one vector of subsets fitting x[[i]]: 
					thissubset=do.call(c("+","-")[1 + (max(sapply(thissubset,max))<=0)], list(unlist(thissubset),rep(seq(0,length(thissubset)-1)*firstdims, lenthissubset)) )
					}
				}
			else{
				thissubset=subset
				}
			
			# Apply the current subset:
			if(is.logical(thissubset) && only.match){
				if(length(thissubset)==length(x)){
					indout=thissubset
					}
				# If ind.out=TRUE, return the entire sequence along the data:
				else if(ind.out){
					indout=seq_along(x)
					}
				}
			else{
				indout=seq_along(x)[thissubset]
				}
			# Extract and reset the dimension:
			if(length(insert.NA)>0){
				x[-indout]=insert.NA
				}
			else{
				x=x[indout]
				}
			# Output:
			if(ind.out){
				list(x=x,ind=indout)
				}
			else{
				x
				}
			}
		
		### 3 ###
		# If both 'ind' and 'subset' are given, transform 'ind' to indexes, and combine with 'subset':
		else if(length(ind)>0 && !(identical(subset,0) || length(subset)==0)){
			# Expand the 'ind' to fit the dimension of the data:
			ind=ind.expand(ind,dimx,pad=pad)
			# Transform 'subset' to postitive numeric indexes:
			subset=seq_len(prod(dimx))[subset]
			
			# Dimension of 'ind':
			d=sapply(ind,length)
			if(drop){
				d=d[d!=1]
				}
			# Get the vector indexes of 'ind':
			ind=arr.ind2ind(ind,dimx)
			# Intersect the indexes of 'ind' and 'subset':
			if(!all(only.match,length(subset)!=length(x))){
				ind=intersect(ind,subset)
				}
			# Extract and reset the dimension:
			if(length(insert.NA)>0){
				x[-ind]=insert.NA
				}
			else{
				x=x[ind]
				# If 'subset' did not make a subset of the subset selected by 'ind':
				if(length(d)!=0 && prod(d)==length(ind)){
					dim(x)=d
					}
				}
			# Output:
			if(ind.out){
				list(x=x,ind=ind)
				}
			else{
				x
				}
			}
		
		### 4 ###
		# Else return the unaltered input (along with the full indexes if requested):
		else{
			if(return.all){
				if(ind.out){
					list(x=x,ind=seq_along(x))
					}
				else{
					x
					}
				}
			else{
				if(ind.out){
					list(x=NULL,ind=NULL)
					}
				else{
					NULL
					}
				}
			}
		}
	##################################################
	##################################################
	}
