#' Microbiome Example Dataset with Whole Numbers
#'
#' A synthetic dataset representing microbiome count data with whole numbers.
#' This dataset is specifically structured with integer counts to demonstrate
#' functions like AdvancedRarefactionPlot in scenarios where decimalization is not
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
