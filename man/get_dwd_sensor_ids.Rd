% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dwd_sensor_ids.R
\name{get_dwd_sensor_ids}
\alias{get_dwd_sensor_ids}
\title{get_dwd_sensor_ids}
\usage{
get_dwd_sensor_ids(plz, path)
}
\arguments{
\item{plz}{numeric vector of German post codes}

\item{path}{filepath where the informations are stored}
}
\value{
all DWD stations inside the plz areas
}
\description{
get_dwd_sensor_ids
}
\examples{
get_dwd_sensor_ids(80993, './')
}
