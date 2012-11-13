\name{geotopbrick}
\alias{geotopbrick}
\alias{geotopbrick.default}
\alias{geotopbrick.zoo}
\title{geotopbrick}
\usage{
  geotopbrick(x = NULL, ...)

  \method{geotopbrick}{default} (x, ...)

  \method{geotopbrick}{zoo} (x, layer = 1,
    timerange = NULL, time = index(x), crs = NULL, ...)
}
\arguments{
  \item{x}{a 'zoo' object returned by function
  \code{\link{pointer.to.maps.xyz.time}} or
  \code{\link{pointer.to.maps.xy.time}} or a
  \code{\link{GeotopRasterBrick-class}} object}

  \item{layer}{layer at which raster maps are imported. If
  is \code{NULL}, maps ara no-zlayer distributed and
  \code{zoo} must be returend by
  \code{\link{pointer.to.maps.xy.time}}}

  \item{timerange}{two-elememts vector containing the time
  range at which geotop maps are imported}

  \item{time}{vector of time instants at which geotop maps
  are imported}

  \item{crs}{coordinate system see
  \code{\link{RasterBrick-class}}}

  \item{...}{further arguments.}
}
\value{
  a \code{\link{GeotopRasterBrick-class}}
}
\description{
  geotopbrick
}

