# dataManipulationFunctions.R

#' Add Data to MicrobiomeData Object
#'
#' Merges new data into an existing MicrobiomeData object with advanced handling
#' of overlapping samples and consistency checks.
#'
#' @param object MicrobiomeData object to add data to.
#' @param new_data New data to add (matrix or data frame).
#' @param data_type Type of the new data ('rRNA16S', 'Metagenomic', or 'Metatranscriptomic').
#' @return Updated MicrobiomeData object.
#' @export
addData <- function(object, new_data, data_type) {
  if (!inherits(object, "MicrobiomeData")) {
    stop("Object must be of class 'MicrobiomeData'")
  }

  if (!is.matrix(new_data) && !is.data.frame(new_data)) {
    stop("New data must be a matrix or data frame")
  }

  valid_types <- c("rRNA16S", "Metagenomic", "Metatranscriptomic")
  if (!data_type %in% valid_types) {
    stop("Invalid data type specified. Valid types are: ", paste(valid_types, collapse = ", "))
  }

  # Additional validation based on data_type
  if (data_type == "rRNA16S") {
    if (!validate16SRNAData(new_data)) {
      stop("Validation failed for 16S rRNA data.")
    }
  } else if (data_type == "Metagenomic") {
    if (!validateMetagenomicData(new_data)) {
      stop("Validation failed for Metagenomic data.")
    }
  } else if (data_type == "Metatranscriptomic") {
    if (!validateMetatranscriptomicData(new_data)) {
      stop("Validation failed for Metatranscriptomic data.")
    }
  }

  existing_data <- slot(object, data_type)
  if (is.null(existing_data)) {
    slot(object, data_type) <- new_data
  } else {
    # Merge existing and new data
    combined_data <- mergeData(existing_data, new_data)
    slot(object, data_type) <- combined_data
  }

  return(object)
}

# Helper function to merge data matrices
# Advanced Merging Strategy with Options for Overlapping Data
mergeData <- function(existing_data, new_data, merge_strategy = "average") {
  existing_df <- as.data.frame(existing_data)
  new_df <- as.data.frame(new_data)

  common_samples <- intersect(rownames(existing_df), rownames(new_df))
  common_features <- intersect(colnames(existing_df), colnames(new_df))

  if (length(common_samples) > 0 && length(common_features) > 0) {
    for (sample in common_samples) {
      for (feature in common_features) {
        existing_value <- existing_df[sample, feature]
        new_value <- new_df[sample, feature]

        # Handling NA values
        if (is.na(existing_value)) existing_value <- 0
        if (is.na(new_value)) new_value <- 0

        # Merging based on the selected strategy
        updated_value <- switch(merge_strategy,
                                "average" = mean(c(existing_value, new_value)),
                                "prioritize_new" = new_value,
                                "prioritize_existing" = existing_value,
                                stop("Invalid merge strategy specified.")
        )

        existing_df[sample, feature] <- updated_value
      }
    }

    # Merging non-overlapping new data
    non_overlapping_samples <- setdiff(rownames(new_df), common_samples)
    non_overlapping_data <- new_df[non_overlapping_samples, , drop = FALSE]
    existing_df <- rbind(existing_df, non_overlapping_data)
  } else {
    # Simple combination for no overlapping data
    existing_df <- rbind(existing_df, new_df)
  }

  # Convert back to original data type (matrix or data frame)
  combined_data <- as.matrix(existing_df)
  return(combined_data)
}



#' Remove Data from MicrobiomeData Object
#'
#' Removes a specific type of data (e.g., rRNA16S, Metagenomic, or Metatranscriptomic)
#' from a MicrobiomeData object.
#'
#' @param object MicrobiomeData object to remove data from.
#' @param data_type Type of the data to remove ('rRNA16S', 'Metagenomic', 'Metatranscriptomic').
#' @return Updated MicrobiomeData object.
#' @export
removeData <- function(object, data_type) {
  if (!inherits(object, "MicrobiomeData")) {
    stop("Object must be of class 'MicrobiomeData'")
  }

  valid_types <- c("rRNA16S", "Metagenomic", "Metatranscriptomic")
  if (!data_type %in% valid_types) {
    stop("Invalid data type specified. Valid types are: ", paste(valid_types, collapse = ", "))
  }

  # Directly remove the specified data type
  slot(object, data_type) <- NULL

  return(object)
}



#' Update Sample Information in MicrobiomeData Object
#'
#' @description Updates the sample information in a MicrobiomeData object.
#' @param object MicrobiomeData object to update sample information.
#' @param new_sample_info New sample information (data frame).
#' @return Updated MicrobiomeData object.
#' @export
updateSampleInfo <- function(object, new_sample_info) {
  if (!inherits(object, "MicrobiomeData")) {
    stop("Object must be of class 'MicrobiomeData'")
  }

  if (!is.data.frame(new_sample_info)) {
    stop("New sample information must be a data frame")
  }

  # Validate new sample information against existing data
  existing_data_types <- c("rRNA16S", "Metagenomic", "Metatranscriptomic")
  for (data_type in existing_data_types) {
    data <- slot(object, data_type)
    if (!is.null(data)) {
      # Using the validateSampleInfo function to check consistency
      if (!validateSampleInfo(new_sample_info, data)) {
        stop(paste("Sample information validation failed for", data_type, "data."))
      }
    }
  }

  # Update sample information
  slot(object, "SampleInfo") <- new_sample_info
  return(object)
}

# Example of the validateSampleInfo function for reference
validateSampleInfo <- function(sampleInfo, data) {
  # Ensuring sampleInfo is a data frame
  if (!is.data.frame(sampleInfo)) {
    stop("Sample information must be a data frame.")
  }

  # Validating matching number of samples
  if (nrow(sampleInfo) != nrow(data)) {
    stop("The number of samples in the sample information does not match the data.")
  }

  # Checking for unique sample identifiers
  sampleIDs <- rownames(sampleInfo)
  if (length(unique(sampleIDs)) != length(sampleIDs)) {
    stop("Sample identifiers in sample information must be unique.")
  }

  # Verifying sample identifiers match those in the data
  if (!all(rownames(data) %in% sampleIDs)) {
    stop("Sample identifiers in the dataset do not match those in the sample information.")
  }

  # Checking for missing values in metadata
  if (any(is.na(sampleInfo))) {
    warning("There are missing values in the sample information.")
  }

  return(TRUE)
}


