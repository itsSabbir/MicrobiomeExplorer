#' Advanced Alpha Diversity Calculation for Microbiome Data
#'
#' This function calculates various alpha diversity indices (Shannon, Simpson, Chao1, and ACE) for each sample
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
#' sample_data <- matrix(rnorm(45), nrow = 9, ncol = 5)
#' colnames(sample_data) <- paste0("Taxa_", 1:5)
#' rownames(sample_data) <- paste0("Sample_", 1:9)
#' sample_data_df <- as.data.frame(sample_data)
#' diversity_results <- calculate_alpha_diversity(sample_data_df, indices = c("Shannon", "Simpson", "Chao1", "ACE"))
#'
#' @references
#' Shannon, C.E. (1948). A Mathematical Theory of Communication. Bell System Technical Journal.
#' Simpson, E.H. (1949). Measurement of Diversity. Nature.
#' Chao, A. (1984). Nonparametric estimation of the number of classes in a population. Scandinavian Journal of Statistics.
#' Chao, A., & Lee, S.M. (1992). Estimating the number of classes via sample coverage. Journal of the American Statistical Association.
calculate_alpha_diversity <- function(data, indices = c("Shannon", "Simpson", "Chao1", "ACE"), rarefied = FALSE) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  data <- data[, sapply(data, is.numeric)]

  results <- data.frame(Sample = rownames(data))

  calc_shannon <- function(x) {
    p <- x / sum(x)
    -sum(p * log(p), na.rm = TRUE)
  }

  calc_simpson <- function(x) {
    p <- x / sum(x)
    1 - sum(p^2, na.rm = TRUE)
  }

  calc_chao1 <- function(x) {
    S_obs <- sum(x > 0)
    n <- sum(x)
    f1 <- sum(x == 1)
    S_chao1 <- S_obs + f1^2 / (2 * (sum(x == 2) + 1))
    return(S_chao1)
  }

  calc_ace <- function(x) {
    S_abund <- sum(x > 10)
    S_rare <- sum(x <= 10)
    n_rare <- sum(x[x <= 10])
    f1 <- sum(x == 1)
    f2 <- sum(x == 2)
    ACE <- S_abund + S_rare / (1 - f1 / n_rare) + (f1 * (f1 - 1)) / (2 * (f2 + 1))
    return(ACE)
  }

  if ("Shannon" %in% indices) {
    results$Shannon <- apply(data, 1, calc_shannon)
  }

  if ("Simpson" %in% indices) {
    results$Simpson <- apply(data, 1, calc_simpson)
  }

  if ("Chao1" %in% indices) {
    results$Chao1 <- apply(data, 1, calc_chao1)
  }

  if ("ACE" %in% indices) {
    results$ACE <- apply(data, 1, calc_ace)
  }

  return(results)
}
