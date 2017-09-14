#*********************************************
#*********************************************
#' Generate arrays with the given dimenstions.
#'
#' @param ... the elements specifying the dimension of the array.
#' @param type specifies the return data type as either "int" or "double" (default), used in zeros() and ones().
#'
#' @return An array of zeros, ones, NAs, NaNs, empry characters or FALSEs, with the given dimension.
#'
#' @examples
#' \dontrun{
#' zeros(2:3)
#' ones(2:3, 4)
#' chars(2:3)
#' FALSEs(2:3)
#' NAs(2:3)
#' NaNs(2:3)}
#'
#' @export
#' @rdname zeros
#'
chars<-function(...){
	# Start: 2008-08-31 - Bad version.
	# Update: 2009-02-17 - Finished.
	# Update: 2009-02-23 - Use of double() to speed the function up to half the time as array().
	# Update: 2009-02-03 - Support for list objects in input.
	# Last: 2009-04-06 - Inherited from zeros.
	##### Preparation #####
	dimension=unlist(list(...))
	if(is.null(dimension)){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	out=character(l)
	dim(out)=dimension
	out
}
#'
#' @export
#' @rdname zeros
#'
FALSEs<-function(...){
	# Start: 2009-03-08 - Finished.
	##### Preparation #####
	dimension=unlist(list(...))
	if(is.null(dimension)){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	array(logical(l),dim=dimension)
}
#'
#' @export
#' @rdname zeros
#'
NaNs<-function(...){
	# Start: 2008-08-31 - Bad version.
	# Update: 2009-02-17 - Finished.
	# Last: 2009-02-03 - Support for list objects in input.
	
	##### Preparation #####
	dimension=unlist(list(...))
	if(is.null(dimension)){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	out=rep(NaN,l)
	dim(out)=dimension
	out
}
#'
#' @export
#' @rdname zeros
#'
NAs<-function(...){
	# Start: 2008-08-31 - Bad version.
	# Update: 2009-02-17 - Finished.
	# Last: 2009-02-03 - Support for list objects in input.

	##### Preparation #####
	dimension=unlist(list(...))
	if(is.null(dimension)){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	out=rep(NA,l)
	dim(out)=dimension
	out
}
#'
#' @export
#' @rdname zeros
#'
ones<-function(...,type="double"){
	# Start: 2008-08-31 - Bad version.
	# Update: 2009-02-17 - Finished.
	# Update: 2009-02-23 - Use of double() to speed the function up to half the time as array().
	# Update: 2009-02-03 - Support for list objects in input.
	# Last: 2009-07-06 - Support for NULL input and 'type'. Method changed to using dim() on output, instead of using the array() function, reducing system time to 50%.

	##### Preparation #####
	dimension=unlist(list(...))
	if(is.null(dimension)){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	if(type=="int"){
		output=integer(l)+as.integer(1)
	}
	else{
		output=double(l)+1
	}
	dim(output)=dimension
	output
}
#'
#' @export
#' @rdname zeros
#' 
zeros<-function(..., type="double"){
	# Start: 2008-08-31 - Bad version.
	# Update: 2009-02-17 - Finished.
	# Update: 2009-02-23 - Use of double() to speed the function up to half the time as array().
	# Update: 2009-02-03 - Support for list objects in input.
	# Last: 2009-07-06 - Support for NULL input and 'type'. Method changed to using dim() on output, instead of using the array() function, reducing system time to 30%.
	##### Preparation #####
	dimension=unlist(list(...))
	if(length(dimension)==0){
		dimension=0
	}
	l=prod(dimension)
	if(length(dimension)==1){
		dimension=NULL
	}
	
	##### Execution and output #####
	if(type=="int"){
		output=integer(l)
	}
	else{
		output=double(l)
	}
	if(length(dimension)>0){
		dim(output)=dimension
	}
	output
}
