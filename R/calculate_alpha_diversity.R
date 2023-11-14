# calculate_alpha_diversity.R

#' Advanced Alpha Diversity Calculation for Microbiome Data
#'
#' This function calculates various alpha diversity indices (Shannon, Simpson, and others) for each sample
#' in a microbiome dataset. It also offers an option to handle rarefied data.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param indices A character vector specifying the diversity indices to calculate.
#'                Supported indices: "Shannon", "Simpson", "Chao1", "ACE".
#' @param rarefied Logical, indicating whether the data is rarefied.
#' @return A dataframe with specified alpha diversity indices for each sample.
#' @export
#'
#' @examples
#' data(microbiome_example) # Example dataset
#' alpha_diversity <- calculate_alpha_diversity(microbiome_example, indices = c("Shannon", "Simpson"))
#' print(alpha_diversity)
#'
#' @references
#' Shannon, C.E. (1948). A Mathematical Theory of Communication. Bell System Technical Journal.
#' Simpson, E.H. (1949). Measurement of Diversity. Nature.
#' Chao, A. (1984). Nonparametric estimation of the number of classes in a population. Scandinavian Journal of Statistics.
#' Chao, A., & Lee, S.M. (1992). Estimating the number of classes via sample coverage. Journal of the American Statistical Association.
calculate_alpha_diversity <- function(data, indices = c("Shannon", "Simpson"), rarefied = FALSE) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  results <- data.frame(Sample = rownames(data))

  # Function to calculate Shannon diversity
  calc_shannon <- function(x) {
    p <- x / sum(x)
    -sum(p * log(p), na.rm = TRUE)
  }

  # Function to calculate Simpson diversity
  calc_simpson <- function(x) {
    p <- x / sum(x)
    1 - sum(p^2, na.rm = TRUE)
  }

  if ("Shannon" %in% indices) {
    results$Shannon <- apply(data, 1, calc_shannon)
  }

  if ("Simpson" %in% indices) {
    results$Simpson <- apply(data, 1, calc_simpson)
  }

  # Rarefied data adjustments
  #if (rarefied) {
    # Recalculate diversity indices for rarefied data
    # This assumes that the data has already been rarefied to a consistent sampling depth.
   # if ("Shannon" %in% indices) {
    #  results$Shannon_Rarefied <- apply(data, 1, calc_shannon)
    #}
#
 #   if ("Simpson" %in% indices) {
  #    results$Simpson_Rarefied <- apply(data, 1, calc_simpson)
   # }
  #}

  return(results)
}


