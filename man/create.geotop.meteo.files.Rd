% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.geotop.meteo.file.R
\name{create.geotop.meteo.files}
\alias{create.geotop.meteo.files}
\title{Creates geotop meteo files from (a list of) 'zoo' objects}
\usage{
create.geotop.meteo.files(
  x,
  format = "\%d/\%m/\%Y \%H:\%M",
  file_prefix = "meteo",
  file_extension = ".txt",
  formatter = "\%04d",
  na = "-9999",
  col.names = TRUE,
  row.names = FALSE,
  date_field = "Date",
  sep = ",",
  level = NULL,
  quote = FALSE,
  ...
)
}
\arguments{
\item{x}{'zoo' object or a list of 'zoo' object representing the meteorological station}

\item{format}{string format representing the date, see \code{\link{as.POSIXlt}}. Default is \code{"\%d/\%m/\%Y \%H:\%M"} (which is the same format used in \code{geotop.inpts} keyword \code{InitDateDDMMYYYYhhmm})}

\item{file_prefix}{string containing file prefix (full path). It correspos to the value of in \code{geotop.inpts} keyword \code{MeteoFile})}

\item{file_extension}{string containing the extensions of final files. Default is \code{c(".txt")}}

\item{formatter}{string value. It is the decimal formatter contained in the file name and used in case the tabular data are referred at several points. Default is \code{"\%04d"} . See \code{\link{sprintf}} .}

\item{na}{NA value indicator. Default is \code{"-9999"}. See \code{\link{write.table}}.}

\item{col.names}{logical parameter. Default is \code{TRUE}. See \code{\link{write.table}}.}

\item{row.names}{logical parameter. Default is \code{FALSE}. See \code{\link{write.table}}.}

\item{date_field}{string value. Default is "Date", otherwise defined by the value of \code{HeaderDateDDMMYYYYhhmmMeteo} geotop keyword.}

\item{sep}{string value. Default is \code{","}. See \code{\link{write.table}}.}

\item{level}{integer argument. See \code{\link{get.geotop.inpts.keyword.value}} for major details. Default is \code{NULL} and is ignored.}

\item{quote}{logical parameter. Default is \code{TRUE}. See \code{\link{write.table}}.}

\item{...}{further arguments for \code{\link{write.table}}}
}
\description{
Creates geotop meteo files from (a list of) 'zoo' objects
}
\examples{

library(geotopbricks)
data(bondone) ## It contains a "meteo" zoo object.


set.seed(12)

file_prefix <- paste(tempdir(),"meteo",sep="/") 
level=2
out <- create.geotop.meteo.files(x=meteo,file_prefix=file_prefix,level=level) 
## It exports the "meteo" zoo object into a ASCII file for GEOtop
head(readLines(out))
out




}
\seealso{
\code{\link{write.table}},\code{\link{get.geotop.inpts.keyword.value}}
}
