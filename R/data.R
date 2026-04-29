#' Microbiome Example Dataset with Whole Numbers
#'
#' A synthetic dataset representing microbiome count data with whole numbers.
#' This dataset is specifically structured with integer counts to demonstrate
#' functions like advancedRarefactionPlot in scenarios where decimalization is not
#' present. It can be used to showcase analysis techniques where data precision
#' does not include decimals.
#'
#' @format A data frame with 30 rows and 10 variables (taxa). Each variable
#' represents a taxa count in each sample. The counts are whole numbers.
#'
#' @source Generated using createMicrobiomeExample function, simulating
#' microbiome count data without decimalization.
#'
#'
"microbiomeExample"


#' Microbiome Example Dataset with Decimal Numbers
#'
#' A synthetic dataset representing microbiome count data with decimal values.
#' This dataset includes decimalized counts, making it suitable for functions
#' and analysis techniques where finer precision is required. It illustrates
#' how decimalization impacts the analysis and visualization in microbiome studies.
#'
#' @format A data frame with 30 rows and 10 variables (taxa). Each variable
#' represents a taxa count in each sample. The counts include decimal values.
#'
#' @source Generated using generateMicrobiomeData function, simulating
#' microbiome count data with decimalization.
#'
#'
"microbiome_example"


#' Sample Microbiome Dataset with Metadata
#'
#' A bundled list containing a synthetic OTU count matrix and matching sample
#' metadata, designed for demonstrating all package features including ML
#' classification and biomarker discovery. The data simulates two groups
#' (Healthy vs Disease) with distinct abundance profiles.
#'
#' @format A list with two elements:
#' \describe{
#'   \item{counts}{Integer matrix (30 samples x 25 OTUs). OTUs 1-10 are
#'     enriched in Healthy samples, OTUs 11-20 in Disease, OTUs 21-25 shared.}
#'   \item{metadata}{Data frame with 30 rows and 3 columns: Group
#'     (Healthy/Disease), Site (Gut/Oral/Skin), Age (integer).}
#' }
#'
#' @source Synthetically generated with \code{set.seed(42)}.
"sampleDataset"
