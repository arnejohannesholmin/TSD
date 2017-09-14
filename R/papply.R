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
#' @export
#' @rdname papply
#'
papply <- function(X, FUN, ..., cores=1, outfile=""){
	cores <- min(parallel::detectCores(), cores, length(X))
	#msgfun <- getMSG(msg)
	if(cores==1){
		#if(length(formals(msgfun))){
		#	out <- lapply(X, FUN, ..., msgfun=msgfun)
		#}
		#else{
			out <- lapply(X, FUN, ...)
		#}
	}
	else{
		cl <- parallel::makeCluster(cores, outfile=outfile)
		#if(length(formals(msgfun))){
		#	out <- parallel::parLapply(cl, X, FUN, ..., msgfun=msgfun)
		#}
		#else{
			out <- parallel::parLapply(cl, X, FUN, ...)
		#}
		parallel::stopCluster(cl)
	}
	#cat("\n")
	out
}
### #'
### #' @export
### #' @rdname papply
### #'
### getMSG_old <- function(msg=list()){
### 	if(length(msg) && all(c("len", "msg") %in% names(msg))){
### 		N <- rev(cumprod(rev(msg$len)))
### 		allN <- max(N)
### 		cat(msg$msg,"\n",sep="")
### 		nch <- if(length(msg$w) && startsWith(msg$w[1], "c")) options()$width else nchar(msg$msg)
### 		stepfact = nch / allN
### 	
### 		s <- floor(seq_len(allN) * stepfact)
### 		diffs <- c(0, diff(s))
### 		step <- diffs==1
### 		fact <- c(N[-1], 1)
### 	
### 		fun <- function(ind){
### 			if(length(dim(ind))==2){
### 				flatind <- 1 + rowSums(matrix(fact, byrow=TRUE, ncol=length(fact), nrow=nrow(ind)) * (ind - 1))
### 			}
### 			else{
### 				flatind <- 1 + sum(fact * (ind - 1))
### 			}
### 			cat(rep(".", sum(step[flatind])), sep="")
### 			if(flatind==allN){
### 				cat("\n")
### 			}
### 		}
### 	}
### 	else{
### 		fun <- function(...) NULL
### 	}
### 	fun
### }
### #'
### #' @export
### #' @rdname papply
### #'
### getMSG <- function(msg="Processing:", len=NULL, ind=NULL, width=c("message", "console")){
### 	nch <- if(startsWith(width[1], "c")) options()$width else nchar(msg)
### 	
### 	if(length(len)){
### 		N <- rev(cumprod(rev(len)))
### 		allN <- max(N)
### 		stepfact = nch / allN
### 	
### 		s <- floor(seq_len(allN) * stepfact)
### 		diffs <- c(0, diff(s))
### 		step <- diffs==1
### 		fact <- c(N[-1], 1)
### 	
### 		fun <- function(ind){
### 			flatind <- 1 + sum(fact * (ind - 1))
### 			
### 			cat(rep(".", sum(step[flatind])), sep="")
### 			if(flatind==allN){
### 				cat("\n")
### 			}
### 		}
### 	}
### 	else if(length(ind)){
### 		N <- rapply(ind, length)
### 		allN <- sum(N)
### 		stepfact = nch / allN
### 	
### 		
### 		#ind <- rapply(ind, function(xx) {oldflatind <- flatind; flatind <<- flatind + length(xx); oldflatind + seq_along(xx)}, how="replace")
### 		
### 	}
### 	else{
### 		fun <- function(...) NULL
### 	}
### 	
### 	cat(msg, "\n", sep="")
### 	fun
### }
### 