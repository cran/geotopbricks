\name{writeRasterxGEOtop}
\alias{writeRasterxGEOtop}
\title{This function uses  \code{\link{writeRaster}} to create .asc maps which can be read by GEOtop}
\usage{
  writeRasterxGEOtop(x, filename, overwrite = TRUE,
    NAflag = -9999, use.decimal.formatter = FALSE, ...)
}
\arguments{
  \item{x}{a Raster object, see \code{\link{writeRaster}}.
  It can be also a \code{\link{RasterBrick-class}} object.}

  \item{filename}{see \code{\link{writeRaster}}. It is a
  vector of string or one string containing a decimal
  formatter (see \code{\link{brick.decimal.formatter}}) in
  case \code{x} is a \code{\link{RasterBrick-class}}
  object.}

  \item{overwrite}{logical. Default is \code{TRUE}, see
  \code{\link{writeRaster}}.}

  \item{NAflag}{numeric. Dafauli is -9999, see
  \code{\link{writeRaster}}.}

  \item{use.decimal.formatter}{logical value. Default is
  \code{FALSE}. If it is \code{TRUE} or \code{x} is a
  \code{\link{RasterBrick-class}} object with
  \code{nlayers(x)!=length(filename)} , \code{filename} is
  considered as one string containing a decimal formatter
  (e.g. \code{"\%04d"}, see
  \code{\link{brick.decimal.formatter}}). Otherwise, if
  \code{filename} is considered as a vector string.}

  \item{...}{further aruments of \code{\link{writeRaster}}}
}
\description{
  This function uses \code{\link{writeRaster}} to create
  .asc maps which can be read by GEOtop
}
\note{
  It makes use of \code{\link{system}} functons
}

