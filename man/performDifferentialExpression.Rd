% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performDEAnalysis.R
\name{performDifferentialExpression}
\alias{performDifferentialExpression}
\title{Advanced Differential Expression Analysis}
\usage{
performDifferentialExpression(
  microbiomeData,
  conditions,
  analysisType = "DESeq2",
  countThreshold = 5,
  minSamples = 2
)
}
\arguments{
\item{microbiomeData}{Matrix or dataframe with microbiome data.}

\item{conditions}{Factor vector specifying the condition for each sample.}

\item{analysisType}{Type of analysis to perform, default "DESeq2".}

\item{countThreshold}{The minimum count threshold for a gene to be kept for analysis.}

\item{minSamples}{The minimum number of samples that must meet the countThreshold.}
}
\value{
Results of the differential expression analysis.
}
\description{
Performs differential expression analysis using specified statistical methods.
}
\examples{
microbiomeData <- matrix(rpois(400, lambda = 20), nrow = 100, ncol = 4)
conditions <- factor(rep(c("Condition1", "Condition2"), each = 2))

results <- performDifferentialExpression(microbiomeData, conditions, analysisType = "DESeq2")

}
