# MicrobiomeData S4 Class Definition

#' MicrobiomeData S4 Class
#'
#' @description
#' An S4 class for storing and managing microbiome data. It encompasses various
#' types of microbiome-related data including 16S rRNA gene sequencing data,
#' metagenomic, and metatranscriptomic data, along with associated sample information.
#'
#' @slot rRNA16S 16S rRNA gene sequencing data stored as a matrix or dataframe.
#' @slot Metagenomic Metagenomic data stored as a matrix or dataframe.
#' @slot Metatranscriptomic Metatranscriptomic data stored as a matrix or dataframe.
#' @slot SampleInfo A dataframe containing information about the samples.
#'
#' @export
setClass(
  "MicrobiomeData",
  slots = list(
    rRNA16S = "ANY",
    Metagenomic = "ANY",
    Metatranscriptomic = "ANY",
    SampleInfo = "list"
  ),
  prototype = prototype(
    rRNA16S = NULL,
    Metagenomic = NULL,
    Metatranscriptomic = NULL,
    SampleInfo = list()
  )
)




#' Create a new MicrobiomeData object
#'
#' @description
#' This function initializes a MicrobiomeData object with the provided data.
#'
#' @param rRNA16S A matrix or dataframe with 16S rRNA gene sequencing data.
#' @param Metagenomic A matrix or dataframe with metagenomic data.
#' @param Metatranscriptomic A matrix or dataframe with metatranscriptomic data.
#' @param SampleInfo A data frame with sample information.
#'
#' @return A `MicrobiomeData` object.
#'
#' @importFrom methods new
#' @importFrom stats model.matrix
#' @examples
#' # Example usage of createMicrobiomeDataObject
#'
#' # Creating mock data for the example
#' myRNA16SData <- matrix(runif(50, min = 0, max = 10), nrow = 5, ncol = 10)
#' colnames(myRNA16SData) <- paste0("OTU", 1:10)
#' rownames(myRNA16SData) <- paste0("Sample", 1:5)
#'
#' myMetagenomicData <- matrix(rnorm(20, mean = 10), nrow = 5, ncol = 4)
#' colnames(myMetagenomicData) <- paste0("Gene", 1:4)
#'
#' myMetatranscriptomicData <- matrix(rnorm(20, mean = 5), nrow = 5, ncol = 4)
#' colnames(myMetatranscriptomicData) <- paste0("Expression", 1:4)
#'
#' mySampleInfo <- data.frame(
#'   SampleID = paste0("Sample", 1:5),
#'   Condition = rep(c("Control", "Treatment"), length.out = 5)
#' )
#' rownames(mySampleInfo) <- mySampleInfo$SampleID
#'
#' # Using the function with the mock data
#' myData <- createMicrobiomeDataObject(
#'   rRNA16S = myRNA16SData,
#'   Metagenomic = myMetagenomicData,
#'   Metatranscriptomic = myMetatranscriptomicData,
#'   SampleInfo = mySampleInfo
#' )
#'
#' @export
#'
setGeneric("createMicrobiomeDataObject",
           function(rRNA16S, Metagenomic, Metatranscriptomic, SampleInfo) {
             standardGeneric("createMicrobiomeDataObject")
           }
)

#' @rdname createMicrobiomeDataObject
setMethod("createMicrobiomeDataObject",
          signature(rRNA16S = "ANY", Metagenomic = "ANY", Metatranscriptomic = "ANY", SampleInfo = "list"),
          function(rRNA16S, Metagenomic, Metatranscriptomic, SampleInfo) {
            # Validation Checks
            message("Validating input data...")
            if (!is.null(rRNA16S)) {
              if (!validate16SRNAData(rRNA16S)) stop("Invalid 16S rRNA data.")
            }
            if (!is.null(Metagenomic)) {
              if (!validateMetagenomicData(Metagenomic)) stop("Invalid Metagenomic data.")
            }
            if (!is.null(Metatranscriptomic)) {
              if (!validateMetatranscriptomicData(Metatranscriptomic)) stop("Invalid Metatranscriptomic data.")
            }
            if (!is.null(SampleInfo)) {
              if (!is.data.frame(SampleInfo)) stop("SampleInfo must be a data frame.")
              if (!validateSampleInfo(SampleInfo, rRNA16S)) stop("Invalid SampleInfo.")
            }

            # Create new MicrobiomeData object
            message("Creating MicrobiomeData object...")
            newObject <- new("MicrobiomeData",
                             rRNA16S = rRNA16S,
                             Metagenomic = Metagenomic,
                             Metatranscriptomic = Metatranscriptomic,
                             SampleInfo = SampleInfo)
            return(newObject)
          }
)



#' Retrieve Data from a MicrobiomeData Object
#'
#' @description
#' Extracts specified data from a MicrobiomeData object. This can be used to access
#' different types of stored data within the object.
#'
#' @param object A MicrobiomeData object.
#' @param dataType A character string specifying the type of data to retrieve.
#'                 Valid types are 'rRNA16S', 'Metagenomic', 'Metatranscriptomic', 'SampleInfo'.
#' @return The requested data stored in the MicrobiomeData object.
#' @export
#'
setGeneric("getData", function(object, dataType) standardGeneric("getData"))

#' @rdname getData
setMethod("getData",
          signature(object = "MicrobiomeData", dataType = "character"),
          function(object, dataType) {
            data <- slot(object, dataType)
            if (is.null(data)) {
              stop(paste("No data available for", dataType))
            }
            return(data)
          }
)



#' Update Data in a MicrobiomeData Object
#'
#' @description
#' Updates specific data within a MicrobiomeData object. This function allows modification
#' of the existing data slots in the MicrobiomeData object.
#'
#' @param object A `MicrobiomeData` object to be updated.G
#' @param dataType The type of data to update. Valid types include 'rRNA16S', 'Metagenomic',
#'                 'Metatranscriptomic', and 'SampleInfo'.
#' @param newData The new data to be inserted into the object.
#' @return The updated `MicrobiomeData` object.
#' @export
#'
setGeneric("updateData", function(object, dataType, newData) standardGeneric("updateData"))

#' @rdname updateData
setMethod("updateData",
          signature(object = "MicrobiomeData", dataType = "character", newData = "ANY"),
          function(object, dataType, newData) {
            # Check if validation function exists
            validationFunctionName <- paste0("validate", dataType, "Data")
            if (exists(validationFunctionName, mode = "function")) {
              validationFunction <- get(validationFunctionName)
              validationFunction(newData)
            }

            # Update data
            slot(object, dataType) <- newData
            return(object)
          }
)





# [END]
