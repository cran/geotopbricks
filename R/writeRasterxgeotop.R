# TODO: Add comment
# 
# Author: ecor
###############################################################################
#'
#' This function uses  \code{\link{writeRaster}} to create .asc maps which can be read by GEOtop
#' 
#' @param x a Raster object, see \code{\link{writeRaster}}. It can be also a \code{\link{RasterBrick-class}} object.
#' @param filename see \code{\link{writeRaster}}. It is a vector of string or one string containing a decimal formatter (see \code{\link{brick.decimal.formatter}}) in case \code{x} is a \code{\link{RasterBrick-class}} object.
#' @param overwrite logical. Default is \code{TRUE}, see \code{\link{writeRaster}}. 
#' @param NAflag numeric. Dafauli is -9999, see \code{\link{writeRaster}}.
#' @param use.decimal.formatter logical value. Default is \code{FALSE}. If it is \code{TRUE} or \code{x} is a \code{\link{RasterBrick-class}} object with \code{nlayers(x)!=length(filename)} , \code{filename} is considered as one string containing a decimal formatter (e.g. \code{"\%04d"}, see \code{\link{brick.decimal.formatter}}). Otherwise, if \code{filename} is considered as a vector string. 
#' @param start.from.zero logical value. Default is \code{FALSE}. If \code{TRUE} the formatter starts from \code{0000}, otherwise it starts from \code{0001}. 
#' @param ... further aruments of \code{\link{writeRaster}}
#' 
#' @export
#' @note It makes use of \code{\link{system}} functons 
#' 


writeRasterxGEOtop <- function(x,filename,overwrite=TRUE,NAflag=-9999.0,use.decimal.formatter=FALSE,start.from.zero=FALSE,...) {

options(scipen=99999) # It remove cientific notation	
 ## add write "brick" modality. 
if (class(x)=="RasterBrick") {

	if ((length(filename)!=nlayers(x)) | (use.decimal.formatter) ) {
		
		first <- 1 
		if (start.from.zero) first <- 0 # if star.from.zero==TRUE the formatter writing starts from "L0000","L0001",....
		filename <- array(filename[1],nlayers(x))
	
		for (i in 1:nlayers(x)) {
		
			filename[i] <- sprintf(filename[i],i+first-1) 
			
		}
		
	}
	
	if (nlayers(x)==1) x <- stack(x) # This is because x=subset(x,i) does not return a "RasterLayer" if x has only one layer.
	
	for (i in 1:nlayers(x)) {
		
		writeRasterxGEOtop(x=subset(x,i),filename=filename[i],overwrite=overwrite,NAflag=NAflag,...)
		
	}
	return()
	
} 
 
 
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