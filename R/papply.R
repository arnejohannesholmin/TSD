#*********************************************
#*********************************************
#' Parallel version of lapply
#'
#' Detects and opens cores for simpler parallel lapply.
#'
#' @param X,FUN,...  See lapply.
#' @param cores  An integer giving the number of cores to run the function FUN over.
#' @param outfile  Set this to FALSE to suppress printing from the cores.
#'
#' @return
#'
#' @examples
#' f <- function(i){
#' 	for(j in 1:3){
#' 		for(k in 1:500){
#' 			Sys.sleep(0.001)
#' 			d <- runif(100)
#' 		}
#' 	}	
#' }
#' system.time(p <- papply(1:4, f, cores=1))
#' system.time(p <- papply(1:4, f, cores=2))
#'
#' @importFrom parallel makeCluster parLapply stopCluster detectCores
#' @importFrom pbapply pblapply pboptions
#' @export
#' @rdname papply
#'
papply <- function(X, FUN, ..., cores=1, outfile="", text="Processing... ", msg=TRUE){
	
	if(!msg){
		pbo <- pbapply::pboptions(type = "none")
		on.exit(pboptions(pbo))
	}
	
	availableCores <- parallel::detectCores()
	# If memory runs out, a system call to determine number of cores might fail, thus detectCores() could return NA
	# defaulting to single core if this is the case
	if(is.na(availableCores)){
		availableCores <- 1
	}
	if(cores > availableCores){
		warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
	}
	nruns <- length(X)
	cores <- min(cores, nruns, availableCores)
	
	# Generate the clusters of time steps:
	if(cores>1){
		cat(paste0(text, "(", nruns, " runs using ", cores, " cores in parallel):\n"))
		cl <- parallel::makeCluster(cores)
		# Bootstrap:
		out <- pbapply::pblapply(X, FUN, ..., cl=cl)
		# End the parallel bootstrapping:
		parallel::stopCluster(cl)
	}
	else{
		cat(paste0(text, "(", nruns, " runs):\n"))
		out <- pbapply::pblapply(X, FUN, ...)
	}
	return(out)
}
