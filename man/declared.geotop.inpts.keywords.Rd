\name{declared.geotop.inpts.keywords}
\alias{declared.geotop.inpts.keywords}
\title{Collects all keywords contained in the 'getop.inpts' configuration files and their values in a data frame object.}
\usage{
  declared.geotop.inpts.keywords(wpath = "/Users/ecor/LAVORI_IN_CORSO/2012/BOUSSINESQ_wrr/URGENTE_Cordano_Rigon_2012/manuscript_under_development/panola_simulation/simulation_geotop/panola13_run2xC_test3",
    inpts.file = "geotop.inpts", comment = "!",
    exceptions = "Date", warn = FALSE, ...)
}
\arguments{
  \item{wpath}{working directory containing GEOtop files}

  \item{inpts.file}{name of the GEOtop configuration file.
  Default is \code{"geotop.inpts"}}

  \item{comment}{comment indicator charcater. Default is
  \code{"!"}}

  \item{exceptions}{string vector. If keywords contain an
  element of this vector, the blank spaces in Value \code{"
  "} will not be removed.}

  \item{warn}{logical argument of \code{\link{readLines}}.
  Default is \code{FALSE}.}

  \item{...}{further arguments of \code{\link{readLines}}}
}
\value{
  a data frame with two columns: \code{Keyword} and
  \code{Value}
}
\description{
  Collects all keywords contained in the 'getop.inpts'
  configuration files and their values in a data frame
  object.
}

