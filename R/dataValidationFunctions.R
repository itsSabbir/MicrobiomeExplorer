# dataValidationFunctions.R

#' Validate 16S rRNA Data
#'
#' Performs comprehensive checks on 16S rRNA gene sequencing data, including structure,
#' quality, and content checks to ensure its suitability for analysis.
#'
#' @param rRNA16SData A matrix or data frame with rows as samples and columns as OTUs/taxa.
#' @param minColumns Minimum number of OTUs/taxa columns expected.
#' @param minNonZeroEntries Minimum number of non-zero entries required per sample.
#' @return A logical value indicating whether the data is valid.
#' @export
validate16SRNAData <- function(rRNA16SData, minColumns = 10, minNonZeroEntries = 5) {
  # Check if data is a matrix or data frame
  if (!is.matrix(rRNA16SData) && !is.data.frame(rRNA16SData)) {
    stop("16S rRNA data must be a matrix or data frame.")
  }

  # Check for missing values
  if (any(is.na(rRNA16SData))) {
    stop("16S rRNA data contains missing values. Please impute or remove them.")
  }

  # Check for non-numeric values
  if (!all(sapply(rRNA16SData, is.numeric))) {
    stop("16S rRNA data must contain only numeric values.")
  }

  # Check for negative values
  if (any(rRNA16SData < 0)) {
    stop("16S rRNA data contains negative values, which are not valid.")
  }

  # Check for a minimum number of columns (OTUs/taxa)
  if (ncol(rRNA16SData) < minColumns) {
    stop(paste("16S rRNA data must have at least", minColumns, "OTUs/taxa columns."))
  }

  # Check for a minimum number of non-zero entries per sample
  if (any(rowSums(rRNA16SData > 0) < minNonZeroEntries)) {
    stop(paste("Each sample in the 16S rRNA data must have at least", minNonZeroEntries,
               "non-zero entries."))
  }

  # If all checks pass
  return(TRUE)
}



#' Validate Metagenomic Data
#'
#' Performs thorough checks on metagenomic data for format integrity and data quality.
#' This includes checks for data structure, absence of invalid values, and other
#' metagenomic-specific criteria.
#'
#' @param metagenomicData A matrix or data frame containing metagenomic data.
#'                        Expected format: rows as samples, columns as genes/features.
#' @return A logical value indicating whether the data is valid.
#' @export
validateMetagenomicData <- function(metagenomicData) {
  # Check if data is a matrix or data frame
  if (!is.matrix(metagenomicData) && !is.data.frame(metagenomicData)) {
    stop("Metagenomic data must be a matrix or data frame.")
  }

  # Check for appropriate dimensions (at least one row and one column)
  if (nrow(metagenomicData) < 1 || ncol(metagenomicData) < 1) {
    stop("Metagenomic data must have at least one sample (row) and one gene/feature (column).")
  }

  # Check for non-numeric values
  if (!all(sapply(metagenomicData, is.numeric))) {
    stop("Metagenomic data must contain only numeric values.")
  }

  # Check for negative values, which are not typical in count data
  if (any(metagenomicData < 0)) {
    stop("Metagenomic data contains negative values, which are not valid.")
  }

  # Check for extremely high values that might indicate data entry errors
  threshold <- 1e6  # Setting a threshold for maximum expected count
  if (any(metagenomicData > threshold)) {
    stop("Metagenomic data contains values exceeding the typical range, indicating possible data entry errors.")
  }

  # Additional metagenomic-specific checks can be added here...

  # If all checks pass
  return(TRUE)
}



#' Validate Metatranscriptomic Data
#'
#' Performs comprehensive validation of metatranscriptomic data, ensuring correct format,
#' data integrity, and adherence to expected properties of RNA sequencing data.
#'
#' @param metatranscriptomicData A matrix or data frame containing metatranscriptomic data.
#'                               Expected format: rows as samples, columns as gene expression features.
#' @return A logical value indicating whether the data is valid.
#' @export
validateMetatranscriptomicData <- function(metatranscriptomicData) {
  # Ensure data is a matrix or data frame
  if (!is.matrix(metatranscriptomicData) && !is.data.frame(metatranscriptomicData)) {
    stop("Metatranscriptomic data must be a matrix or data frame.")
  }

  # Validate dimensions: minimum one sample (row) and one feature (column)
  if (nrow(metatranscriptomicData) < 1 || ncol(metatranscriptomicData) < 1) {
    stop("Metatranscriptomic data must have at least one sample (row) and one gene expression feature (column).")
  }

  # Ensure all entries are numeric
  if (!all(sapply(metatranscriptomicData, is.numeric))) {
    stop("Metatranscriptomic data must contain only numeric values.")
  }

  # Check for negative values - typically not expected
  if (any(metatranscriptomicData < 0)) {
    stop("Metatranscriptomic data contains negative values, which are typically not valid.")
  }

  # Check for potential outliers indicating data errors
  threshold <- 1e5  # Adjust the threshold based on expected range in your data
  if (any(metatranscriptomicData > threshold)) {
    stop("Metatranscriptomic data contains unusually high values, possibly indicating data entry errors.")
  }

  # Checking for consistency in sequencing depth across samples
  sequencingDepth <- rowSums(metatranscriptomicData)
  if (sd(sequencingDepth)/mean(sequencingDepth) > 0.5) {
    warning("There is high variability in sequencing depth across samples, which might affect downstream analysis.")
  }

  # Additional checks can be implemented as needed

  # If all checks pass
  return(TRUE)
}



#' Validate Sample Information
#'
#' Verifies if the sample information is correctly formatted and consistent with the provided dataset.
#' Ensures that sample identifiers are unique and match those in the dataset.
#'
#' @param sampleInfo A data frame containing sample information, expected to have rows as samples and columns as metadata fields.
#' @param data A matrix or data frame of the corresponding data (16S rRNA, metagenomic, or metatranscriptomic).
#' @return A logical value indicating whether the sample information is valid.
#' @export
validateSampleInfo <- function(sampleInfo, data) {
  # Ensure sampleInfo is a data frame
  if (!is.data.frame(sampleInfo)) {
    stop("Sample information must be a data frame.")
  }

  # Validate matching number of samples
  if (nrow(sampleInfo) != nrow(data)) {
    stop("The number of samples in the sample information does not match the data.")
  }

  # Check for unique sample identifiers
  sampleIDs <- rownames(sampleInfo)
  if (length(unique(sampleIDs)) != length(sampleIDs)) {
    stop("Sample identifiers in sample information must be unique.")
  }

  # Verify sample identifiers match those in the data
  if (!all(rownames(data) %in% sampleIDs)) {
    stop("Sample identifiers in the dataset do not match those in the sample information.")
  }

  # Optional: Additional checks on metadata fields (e.g., checking for missing values, data types of fields)
  # Example: Check for missing values in metadata
  if (any(is.na(sampleInfo))) {
    warning("There are missing values in the sample information.")
  }

  # If all checks pass
  return(TRUE)
}
