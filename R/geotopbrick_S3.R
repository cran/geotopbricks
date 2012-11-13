# TODO: Add comment
# 
# Author: ecor

### CAMBIRE IN S3!!!!! 

###############################################################################
NULL

#' @rdname geotopbrick
#' @export


geotopbrick <- function(x=NULL,...) {
	
	return(UseMethod("geotopbrick",x))
	
}


NULL

#' 
#' @rdname geotopbrick
#' 
#' @method geotopbrick default
#' @S3method geotopbrick default
#' 
#' 
#' @export

geotopbrick.default <- function (x,...) {
	
	out <- new("GeotopRasterBrick")
	print("default")
	return(out)
	
}

#' @title geotopbrick
#' @name geotopbrick
#' @aliases geotopbrick.zoo 
#'  
#' @param x a 'zoo' object returned by function \code{\link{pointer.to.maps.xyz.time}} or \code{\link{pointer.to.maps.xy.time}} or a \code{\link{GeotopRasterBrick-class}} object
#' @param layer layer at which raster maps are imported. If is \code{NULL}, maps ara no-zlayer distributed and \code{zoo} must be returend by \code{\link{pointer.to.maps.xy.time}}
#' @param timerange two-elememts vector containing the time range at which geotop maps are imported
#' @param time vector of time instants at which geotop maps are imported
# @param rows rows of \code{zoo} correspondig to the geotop maps that are imported. By default all rows of \code{zoo} are considered. It is calculated by \code{time} or \code{timerange} if they are not set as \code{NULL}. 
#' @param crs coordinate system see \code{\link{RasterBrick-class}}
#' @param ... further arguments. 
#' 
#' 
#' 
#' @return a \code{\link{GeotopRasterBrick-class}}
#' 
#' 
#' @rdname geotopbrick
#' 
#' @method geotopbrick zoo
#' @S3method geotopbrick zoo
#' 
#' 
#' @export

geotopbrick.zoo <- function(x,layer=1,timerange=NULL,time=index(x),crs=NULL,...) { 
		
			if (!is.null(timerange)) time <- index(x)[index(x)>=timerange[1] & index(x)<=timerange[2]] 
			
			out <- new("GeotopRasterBrick")
			
			out@ascpath <- x
			out@layer <- as.character(layer)
		
			out@brick <- brick(x,layer=layer,time=time,crs=crs,...) 
			out@index <- time		
	
			return(out)
		}





