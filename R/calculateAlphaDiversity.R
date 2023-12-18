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
#'@importFrom stats rmultinom
#'
#' @examples
#' sample_data <- matrix(rnorm(45), nrow = 9, ncol = 5)
#' colnames(sample_data) <- paste0("Taxa_", 1:5)
#' rownames(sample_data) <- paste0("Sample_", 1:9)
#' sample_data_df <- as.data.frame(sample_data)
#' diversity_alphaResults <- calculate_alpha_diversity(sample_data_df,
#' indices = c("Shannon", "Simpson", "Chao1", "ACE"))
#'
#' @references
#' Shannon, C.E. (1948). A Mathematical Theory of Communication. Bell System
#' Technical Journal.
#' Simpson, E.H. (1949). Measurement of Diversity. Nature.
#' Chao, A. (1984). Nonparametric estimation of the number of classes in a population. Scandinavian Journal of Statistics.
#' Chao, A., & Lee, S.M. (1992). Estimating the number of classes via sample coverage. Journal of the American Statistical Association.
calculate_alpha_diversity <- function(data, indices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"), rarefied = FALSE) {
  # Validate input data
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  data <- data[, sapply(data, is.numeric)] # Ensure all columns are numeric

  alphaResults <- data.frame(Sample = rownames(data))

  # Define diversity calculation functions
  diversity_functions <- list(
    Shannon = function(x) -sum((x / sum(x)) * log(x / sum(x)), na.rm = TRUE),
    Simpson = function(x) 1 - sum((x / sum(x))^2, na.rm = TRUE),
    Chao1 = function(x) sum(x > 0) + sum(x == 1)^2 / (2 * (sum(x == 2) + 1)),
    ACE = function(x) {
      S_abund <- sum(x > 10)
      S_rare <- sum(x <= 10)
      n_rare <- sum(x[x <= 10])
      f1 <- sum(x == 1)
      f2 <- sum(x == 2)
      S_abund + S_rare / (1 - f1 / n_rare) + (f1 * (f1 - 1)) / (2 * (f2 + 1))
    },
    Fisher = function(x) {
      s <- sum(x)
      a <- sum(log(1:x))
      b <- sum(x * log(x))
      return(s * log(s) - a - b)
    }
  )

  # Calculate indices
  for (index in indices) {
    if (!index %in% names(diversity_functions)) {
      warning(paste("Index", index, "is not supported. Skipping."))
      next
    }
    func <- diversity_functions[[index]]
    alphaResults[[index]] <- apply(data, 1, func)
  }

  # Handle rarefied data if applicable
  # Rarefying data if requested
  if (rarefied) {
    # Calculate minimum sample size across all samples
    minSampleSize <- min(rowSums(data))
    if (minSampleSize == 0) {
      stop("Cannot rarefy data: one or more samples have zero total count.")
    }

    # Rarefaction process
    rarefyFunction <- function(sample) {
      if (sum(sample) == 0) return(sample) # Skip rarefying empty samples
      probs <- sample / sum(sample)
      rarefiedSample <- rmultinom(1, minSampleSize, prob = probs)
      return(rarefiedSample)
    }
    dataRarefied <- t(apply(data, 1, rarefyFunction))

    # Recalculate diversity indices for rarefied data
    for (index in indices) {
      if (!index %in% names(diversity_functions)) {
        next # Skip unsupported indices
      }
      func <- diversity_functions[[index]]
      alphaResults[[paste0(index, "_rarefied")]] <- apply(dataRarefied, 1, func)
    }
  }

  return(alphaResults)
}
