# TODO: Add comment
# 
# Author: ecor
###############################################################################
#'
#' This function uses  \code{\link[raster]{writeRaster}} to create .asc maps which can be read by GEOtop
#' 
#' @param x a Raster object, see \code{\link[raster]{writeRaster}}. It can be also a \code{\link[raster]{RasterBrick-class}} object.
#' @param filename see \code{\link[raster]{writeRaster}}. It is a vector of string or one string containing a decimal formatter (see \code{\link{brick.decimal.formatter}}) in case \code{x} is a \code{\link[raster]{RasterBrick-class}} object.
#' @param overwrite logical. Default is \code{TRUE}, see \code{\link[raster]{writeRaster}}. 
#' @param NAflag numeric. Dafauli is -9999, see \code{\link[raster]{writeRaster}}.
#' @param use.decimal.formatter logical value. Default is \code{FALSE}. If it is \code{TRUE} or \code{x} is a \code{\link[raster]{RasterBrick-class}} object with \code{nlayers(x)!=length(filename)} , \code{filename} is considered as one string containing a decimal formatter (e.g. \code{"\%04d"}, see \code{\link{brick.decimal.formatter}}). Otherwise, if \code{filename} is considered as a vector string. 
#' @param start.from.zero logical value. Default is \code{FALSE}. If \code{TRUE} the formatter starts from \code{0000}, otherwise it starts from \code{0001}. 
#' @param keyword geotop keyword to be used to extract the raster file name from \code{geotop.inpts} file. This is enabled if \code{filename} is equal to \code{NULL}.
#' @param wpath simulation folder containing \code{geotop.inpts} file.
#' @param suffix.ext charachter string to be added to the \code{keyword} value,e.g. possible suffix and extension of the raster file name. Default is \code{".asc"}.
#' @param ... further arguments of \code{\link{get.geotop.inpts.keyword.value}} or  \code{\link[raster]{writeRaster}}
#'  
#' @export
#' @note It makes use of \code{\link{system}} functons. It uses \code{*.asc} format for raster files. 
#'  In case the file name \code{filename} is missing and then \code{NULL}, it must be imported by the simulation \code{geotop.inpts} file.
#' 
#' 
# @importFrom rgdal showWKT
#' @importFrom terra rast
#' @examples 
#' 
#' 
#' file <- system.file("ex/elev.tif", package="terra")
#' elev <- raster(file)
#' 
#' 
#' elevfile <- rasterTmpFile()
#' extension(elevfile) <- ".asc"
#' 
#' writeRasterxGEOtop(x=elev,file=elevfile)
#' 
#' 
#' 





writeRasterxGEOtop <- function(x,filename=NULL,overwrite=TRUE,NAflag=-9999.0,use.decimal.formatter=FALSE,start.from.zero=FALSE,keyword,wpath,suffix.ext=".asc",...) {

	
options(scipen=99999) # It remove scientific notation	
 ## add write "brick" modality. 
 
 if (is.null(filename)) {
	 
	 filename <- geotopbricks::get.geotop.inpts.keyword.value(keyword,wpath=wpath,add_wpath=TRUE,...)
	 filename <- paste(filename,suffix.ext,sep="")
	 
 }
 
if (is(x,"RasterBrick")) {

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
		
		geotopbricks::writeRasterxGEOtop(x=subset(x,i),filename=filename[i],overwrite=overwrite,NAflag=NAflag,...)
		
	}
	return()
	
} 
 
 
 terra::writeRaster(x=rast(x),filename=filename,overwrite=overwrite,NAflag=NAflag,...) ## EC2023040

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

## set prj 
		## fileprj <- str_replace(filename,".asc",".prj")
		# ec 20150412

	fileprj <- filename
		extension(fileprj) <- ".prj"

	### EC 20230414 COMMENT HERE 
		
	# if (fileprj!=filename) {
	# 	###	print(fileprj)
	# 	##	crs <- proj4string(x)
	# if (is.null(crs)) crs <- NA 
	# 		if (!is.na(crs)) { 
	# 	##		require("rgdal")
	# 			wkt <- rgdal::showWKT(crs)
	# 			writeLines(text=wkt,con=fileprj)
	# 		}
	# 	}
  
    ### EC 20230414 COMMENT HERE 
		



}
