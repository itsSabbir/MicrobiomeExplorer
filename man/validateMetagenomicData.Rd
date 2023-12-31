% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataValidationFunctions.R
\name{validateMetagenomicData}
\alias{validateMetagenomicData}
\title{Validate Metagenomic Data}
\usage{
validateMetagenomicData(metagenomicData)
}
\arguments{
\item{metagenomicData}{A matrix or data frame containing metagenomic data.
Expected format: rows as samples, columns as genes/features.}
}
\value{
A logical value indicating whether the data is valid.
}
\description{
Performs thorough checks on metagenomic data for format integrity and data quality.
This includes checks for data structure, absence of invalid values, and other
metagenomic-specific criteria.
}
