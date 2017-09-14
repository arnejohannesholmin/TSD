#*********************************************
#*********************************************
#' Merges text files given a maximum size
#'
#' @param files		A vector of file paths.
#' @param con		The file path of the merged file, appended by integers before the dot if more than one file is written.
#' @param maxsize	Maximum size of the merged files.
#'
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
#'
mergeTextFiles <- function(files, con=NULL, maxsize=1e6){
	if(length(con)==0){
		con <- paste0(tools::file_path_sans_ext(files[1]), "_merge", ".", tools::file_ext(files[1]))
	}
	sizes <- file.info(files)$size
	# Group the files:
	files <- split(files, ceiling(cumsum(sizes)/maxsize))
	if(length(files)>1){
		con <- paste0(tools::file_path_sans_ext(con), seq_along(files), ".", tools::file_ext(con))
	}
	for(group in seq_along(files)){
		for(element in seq_along(files[[group]])){
			thisReadDump = readLines(files[[group]][element])
			# Add the dumps to the dumpfile:
			write(thisReadDump, con[group], append=element>1)
		}
	}
	list(files=files, con=con)
}