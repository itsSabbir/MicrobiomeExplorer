#' MicrobiomeData S4 Class
#'
#' A class for storing and managing microbiome data including 16S rRNA,
#' metagenomic, and metatranscriptomic data along with sample information.
#'
#' @slot rRNA16S 16S rRNA gene sequencing data
#' @slot Metagenomic Metagenomic data
#' @slot Metatranscriptomic Metatranscriptomic data
#' @slot SampleInfo Information about the samples
#' @export

# Define the MicrobiomeData S4 class
setClass(
  "MicrobiomeData",
  slots = list(
    rRNA16S = "ANY",  # You can replace "ANY" with a more specific class if needed
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
