# TODO: Add comment
# 
# Author: ecor
###############################################################################
#'
#' This function uses  \code{\link{writeRaster}} to create .asc maps which can be read by GEOtop
#' 
#' @param x a Raster object, see \code{\link{writeRaster}}
#' @param filename see \code{\link{writeRaster}}
#' @param overwrite logical. Default is \code{TRUE}, see \code{\link{writeRaster}}. 
#' @param NAflag numeric. Dafauli is -9999, see \code{\link{writeRaster}}.
#' @param ... further aruments of \code{\link{writeRaster}}
#' 
#' @export
#' @note It makes use of \code{\link{system}} functons 
#' 


writeRasterxGEOtop <- function(x,filename,overwrite=TRUE,NAflag=-9999,...) {

 writeRaster(x=x,filename=filename,overwrite=overwrite,NAflag=NAflag,...)

# CORRECT THE HEADER 

 name <- filename
 nlast=5
 namen <- paste(filename,".temp",1:nlast,sep="")
 system(paste("sed -e 's/NCOLS/ncols/g'",name,">",namen[1]))
 system(paste("sed -e 's/NROWS/nrows/g'",namen[1],">",namen[2]))
 system(paste("sed -e 's/XLLCORNER/xllcorner/g'",namen[2],">",namen[3]))
 system(paste("sed -e 's/YLLCORNER/yllcorner/g'",namen[3],">",namen[4]))
 system(paste("sed -e 's/CELLSIZE/cellsize/g'",namen[4],">",namen[5]))

 file.copy(from=namen[nlast],to=name,overwrite=TRUE)

 for (i in 1:nlast) file.remove(namen[i])


}