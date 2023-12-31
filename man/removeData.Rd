% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataManipulationFunctions.R
\name{removeData}
\alias{removeData}
\title{Remove Data from MicrobiomeData Object}
\usage{
removeData(object, data_type)
}
\arguments{
\item{object}{MicrobiomeData object to remove data from.}

\item{data_type}{Type of the data to remove ('rRNA16S', 'Metagenomic', 'Metatranscriptomic').}
}
\value{
Updated MicrobiomeData object.
}
\description{
Removes a specific type of data (e.g., rRNA16S, Metagenomic, or Metatranscriptomic)
from a MicrobiomeData object.
}
