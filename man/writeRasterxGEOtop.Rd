\name{writeRasterxGEOtop}
\alias{writeRasterxGEOtop}
\title{This function uses  \code{\link{writeRaster}} to create .asc maps which can be read by GEOtop}
\usage{
  writeRasterxGEOtop(x, filename, overwrite = TRUE,
    NAflag = -9999, ...)
}
\arguments{
  \item{x}{a Raster object, see \code{\link{writeRaster}}}

  \item{filename}{see \code{\link{writeRaster}}}

  \item{overwrite}{logical. Default is \code{TRUE}, see
  \code{\link{writeRaster}}.}

  \item{NAflag}{numeric. Dafauli is -9999, see
  \code{\link{writeRaster}}.}

  \item{...}{further aruments of \code{\link{writeRaster}}}
}
\description{
  This function uses \code{\link{writeRaster}} to create
  .asc maps which can be read by GEOtop
}
\note{
  It makes use of \code{\link{system}} functons
}

