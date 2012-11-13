\name{get.geotop.inpts.keyword.value}
\alias{get.geotop.inpts.keyword.value}
\title{Returns the values of a keyword of "geotop.inpts" file or data frame with the suitable format}
\usage{
  get.geotop.inpts.keyword.value(keyword,
    inpts.frame = NULL, vector_sep = NULL, numeric = FALSE,
    format = "\%d/\%m/\%Y \%H:\%M", date = FALSE, tz = "A",
    raster = FALSE, file_extension = ".asc",
    add_wpath = FALSE, wpath = NULL,
    use.read.raster.from.url = TRUE, ...)
}
\arguments{
  \item{keyword}{keyword name}

  \item{inpts.frame}{data frame returned by
  \code{\link{declared.geotop.inpts.keywords}} or
  \code{NULL}. Default is \code{NULL}.}

  \item{vector_sep}{character value for the separator
  chacter if Keyword Value must be returned as a vector,
  otherwise it is \code{NULL}. Default is \code{NULL}, but
  if \code{numeric} or \code{date} are \code{FALSE},
  \code{vactor_sep} is set \code{","} by default.}

  \item{numeric}{logical value. If \code{TRUE} the Value
  has numeric type, otherwise it is a string or string
  vector. Default is \code{FALSE}.}

  \item{date}{logical value. If \code{TRUE} the Value is
  retured as \code{\link{POSIXlt}} date, otherwise it is a
  string or string vector. Default is \code{FALSE}.}

  \item{format}{string format representing the date, see
  \code{\link{as.POSIXlt}}, used if \code{date} is
  \code{TRUE}. Default is \code{"\%d/\%m/\%Y \%H:\%M"}
  (which is the format used in \code{geotop.inpts})}

  \item{tz}{format string representing the time zone, see
  \code{\link{as.POSIXlt}}, used if \code{date} is
  \code{TRUE}. Default is \code{"A"}.}

  \item{raster}{logical value. Default is \code{FALSE}. If
  \code{TRUE} function returns direclty the raster map as
  \code{\link{Raster-class}} object built with
  \code{\link{raster}} method.}

  \item{file_extension}{Extension to be added to the
  keyword if keyword is a file name. Default is
  \code{".asc"}}

  \item{wpath}{working directory containing GEOtop files
  (included the inpts file), see
  \code{\link{declared.geotop.inpts.keywords}}. It is
  mandatory if \code{raster} is \code{TRUE}.}

  \item{add_wpath}{logical value. Default is \code{FALSE}.
  If \code{TRUE}, the \code{wpath} string is attached to
  the keyword string value. It is automatically set
  \code{TRUE} if \code{raster} is \code{TRUE}.}

  \item{use.read.raster.from.url}{logical value. Default is
  \code{TRUE}. If \code{TRUE} the RasterLayer are read with
  \code{\link{read.raster.from.url}}, istead of
  \code{\link{raster}} (otherwise). It is recomended in
  case the files whose paths are contained in \code{x} are
  remote and are 'http' addresses. In this cases the
  stand-alone method \code{raster(x)} does not always work
  and \code{use.read.raster.from.url} is necessary.}

  \item{...}{further arguments of
  \code{\link{declared.geotop.inpts.keywords}}}
}
\value{
  the keyword value
}
\description{
  Returns the values of a keyword of "geotop.inpts" file or
  data frame with the suitable format
}
\note{
  If \code{inpts.frame} is \code{NULL}, \code{inpts.frame}
  will be obtained by calling the function
  \code{\link{declared.geotop.inpts.keywords}} with
  \code{...} arguments.
}
\examples{
library(geotopbricks)

#Simulation working path
wpath <- 'http://meteogis.fmach.it/idroclima/panola13_run2xC_test3'
prefix <- get.geotop.inpts.keyword.value("SoilLiqWaterPressTensorFile",wpath=wpath)

slope <- get.geotop.inpts.keyword.value("SlopeMapFile",raster=TRUE,wpath=wpath)
bedrock_depth <- get.geotop.inpts.keyword.value("BedrockDepthMapFile",raster=TRUE,wpath=wpath)

layers <- get.geotop.inpts.keyword.value("SoilLayerThicknesses",numeric=TRUE,wpath=wpath)
names(layers) <- paste("L",1:length(layers),sep="")

# set van genuchten parameters to estimate water volume
theta_sat <- get.geotop.inpts.keyword.value("ThetaSat",numeric=TRUE,wpath=wpath)
theta_res <- get.geotop.inpts.keyword.value("ThetaRes",numeric=TRUE,wpath=wpath)
alphaVG <-  get.geotop.inpts.keyword.value("AlphaVanGenuchten",numeric=TRUE,wpath=wpath) # expressed in mm^-1
nVG <-  get.geotop.inpts.keyword.value("NVanGenuchten",numeric=TRUE,wpath=wpath)


# end set van genuchten parameters to estimate water volume


# set time during wich GEEOtop simulation provided maps (the script is written for daily frequency")

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")

# end set time during wich GEEOtop simulation provided maps (the script is written for daily frequency")
}

