#*********************************************
#*********************************************
#' Splits a vector into groups based on the size of one element and a size limit.
#'
#' @param t  is a vector
#' @param size1  the size of each element.
#' @param size  the maximum size each block.
#' @param blocks  (optional) if the number of blocks is lower than 'blocks', distribute evenly.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail
#'
#' @export
#' @rdname splitSeqIntoBlocks
#'
splitSeqIntoBlocks <- function(t, size1, size, blocks=1){	
	# Get file sizes:
	tseq = seq_along(t)
	cumsize1 = size1 * tseq
	allsize = tail(cumsize1,1)
	if(allsize < blocks*size){
		size = allsize/blocks
	}
	# Group the time steps by size:
	tGroups = ceiling(cumsize1/size)
	split(t, tGroups)
	}
