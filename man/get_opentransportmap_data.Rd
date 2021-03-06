% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_opentransportmap_data.R
\name{get_opentransportmap_data}
\alias{get_opentransportmap_data}
\title{get_opentransportmap_data}
\usage{
get_opentransportmap_data(plz, path, trafficvol_treshold = 1,
  mc.cores = parallel::detectCores() - 1)
}
\arguments{
\item{plz}{a vector a German postcode for that the traffic data will be}

\item{path}{filepath where the informations are stored}

\item{trafficvol_treshold}{A threshold, which roads should be ignored, default
is 1, due to the logarithmical transformation of the data}

\item{mc.cores}{how much cores should be used for parallelization, default is
one core less your maximum number of detected cores.}
}
\value{
a sfc object of the roads
}
\description{
get_opentransportmap_data
}
\examples{
get_opentransportmap_data('80993')
}
