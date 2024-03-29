# TODO: Add comment
# 
# Author: ecor
###############################################################################

NULL
#'
#' Creates geotop meteo files from (a list of) 'zoo' objects
#' 
#'
#' 
#' @param x 'zoo' object or a list of 'zoo' object representing the meteorological station 
#' @param format string format representing the date, see \code{\link{as.POSIXlt}}. Default is \code{"\%d/\%m/\%Y \%H:\%M"} (which is the same format used in \code{geotop.inpts} keyword \code{InitDateDDMMYYYYhhmm})
#' @param file_prefix string containing file prefix (full path). It correspos to the value of in \code{geotop.inpts} keyword \code{MeteoFile})  
#' @param file_extension string containing the extensions of final files. Default is \code{c(".txt")}
#' @param formatter string value. It is the decimal formatter contained in the file name and used in case the tabular data are referred at several points. Default is \code{"\%04d"} . See \code{\link{sprintf}} .
#' @param na NA value indicator. Default is \code{"-9999"}. See \code{\link{write.table}}.
#' @param row.names logical parameter. Default is \code{FALSE}. See \code{\link{write.table}}.
#' @param col.names logical parameter. Default is \code{TRUE}. See \code{\link{write.table}}.
#' @param date_field string value. Default is "Date", otherwise defined by the value of \code{HeaderDateDDMMYYYYhhmmMeteo} geotop keyword. 
#' @param sep string value. Default is \code{","}. See \code{\link{write.table}}.
#' @param quote logical parameter. Default is \code{TRUE}. See \code{\link{write.table}}.
#' @param level integer argument. See \code{\link{get.geotop.inpts.keyword.value}} for major details. Default is \code{NULL} and is ignored.  
#' @param ... further arguments for \code{\link{write.table}}
#' 
#' 
#' @export
#' @examples 
#' 
#' library(geotopbricks)
#' data(bondone) ## It contains a "meteo" zoo object.
#' 
#'
#' set.seed(12)
#' 
#' file_prefix <- paste(tempdir(),"meteo",sep="/") 
#' level=2
#' out <- create.geotop.meteo.files(x=meteo,file_prefix=file_prefix,level=level) 
#' ## It exports the "meteo" zoo object into a ASCII file for GEOtop
#' head(readLines(out))
#' out
#'
#' 
#' 
#' 
#' @seealso \code{\link{write.table}},\code{\link{get.geotop.inpts.keyword.value}}
#' 
#' 






create.geotop.meteo.files <- function(x,format="%d/%m/%Y %H:%M",file_prefix="meteo",file_extension=".txt",formatter="%04d",na="-9999",
		col.names=TRUE,row.names=FALSE,date_field="Date",sep=",",level=NULL,quote=FALSE,...) {
		
		
		

	if (!is.list(x)) x <- list(x)
	if (is.null(level)) { level <- 1:length(x)}
	if (length(level)<1) { level <- 1:length(x)}
	if (is.na(level)) { level <- 1:length(x)}
	
	filename <- paste(file_prefix,formatter,sep="")
	
	if (str_sub(file_extension,1,1)==".")  {
		filepath <- paste(filename,file_extension,sep="") 
	} else { 	
		filepath <- paste(filename,file_extension,sep=".") 
	}
	filename <- filepath
	out <- as.character(NA)
	for (i in 1:length(x)) { 
				
#		x[[i]] is a 'zoo' object 
		filenamex <- sprintf(filename,level[i]) 
		y <- x[[i]]
		names <- names(y)
		y <- cbind(index(y),as.data.frame(y))
		names(y) <- c(date_field,names)
		
		y[,date_field] <- format(y[,date_field],format=format)

		
		
		utils::write.table(x=y,file=filenamex,quote=quote,sep=sep,na=na,row.names=row.names,col.names=col.names,...)
		out[i] <- filenamex
	} 
	
		
		
			
	
	
	return(out)
}