#' Advanced Alpha Diversity Calculation for Microbiome Data
#'
#' This function calculates various alpha diversity indices (Shannon, Simpson, Chao1, and ACE) for each sample
#' in a microbiome dataset. It also offers an option to handle rarefied data.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param indices A character vector specifying the diversity indices to calculate.
#'                Supported indices: "Shannon", "Simpson", "Chao1", "ACE".
#' @param rarefied Logical, indicating whether the data is rarefied.
#' @param ace_threshold Numeric. Abundance threshold for ACE index; taxa with
#'   counts above this are considered "abundant". Default: \code{10}.
#' @return A dataframe with specified alpha diversity indices for each sample.
#' @export
#'@importFrom stats rmultinom
#'
#' @examples
#' set.seed(42)
#' sample_data <- matrix(rpois(45, lambda = 10), nrow = 9, ncol = 5)
#' colnames(sample_data) <- paste0("Taxa_", 1:5)
#' rownames(sample_data) <- paste0("Sample_", 1:9)
#' sample_data_df <- as.data.frame(sample_data)
#' diversity_alphaResults <- calculateAlphaDiversity(sample_data_df,
#' indices = c("Shannon", "Simpson", "Chao1", "ACE"))
#'
#' @references
#' Shannon, C.E. (1948). A Mathematical Theory of Communication. Bell System
#' Technical Journal.
#' Simpson, E.H. (1949). Measurement of Diversity. Nature.
#' Chao, A. (1984). Nonparametric estimation of the number of classes in a population. Scandinavian Journal of Statistics.
#' Chao, A., & Lee, S.M. (1992). Estimating the number of classes via sample coverage. Journal of the American Statistical Association.
calculateAlphaDiversity <- function(data, indices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"), rarefied = FALSE, ace_threshold = 10) {
  # Validate input data
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  # Ensure all columns are numeric
  if (is.matrix(data)) {
    if (!is.numeric(data)) stop("Matrix must be numeric.")
  } else {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    data <- data[, numeric_cols, drop = FALSE]
  }
  data <- as.matrix(data)

  alphaResults <- data.frame(Sample = rownames(data))

  # Define diversity calculation functions
  diversity_functions <- list(
    Shannon = function(x) {
      x <- x[x > 0]
      if (length(x) == 0) return(NA_real_)
      p <- x / sum(x)
      -sum(p * log(p))
    },
    Simpson = function(x) {
      x <- x[x > 0]
      if (length(x) == 0) return(NA_real_)
      p <- x / sum(x)
      1 - sum(p^2)
    },
    Chao1 = function(x) {
      x <- x[x > 0]
      if (length(x) == 0) return(NA_real_)
      S_obs <- length(x)
      f1 <- sum(x == 1)
      f2 <- sum(x == 2)
      S_obs + f1^2 / (2 * (f2 + 1))
    },
    ACE = function(x) {
      x <- x[x > 0]
      if (length(x) == 0) return(NA_real_)
      S_abund <- sum(x > ace_threshold)
      S_rare <- sum(x <= ace_threshold)
      n_rare <- sum(x[x <= ace_threshold])
      if (n_rare == 0) return(S_abund)
      f1 <- sum(x == 1)
      f2 <- sum(x == 2)
      C_ace <- 1 - f1 / n_rare
      if (C_ace == 0) return(NA_real_)
      S_abund + S_rare / C_ace + (f1 * (f1 - 1)) / (2 * (f2 + 1))
    },
    Fisher = function(x) {
      # Fisher's alpha: S = a * ln(1 + n/a), solved via Newton-Raphson
      x <- x[x > 0]
      if (length(x) == 0) return(NA_real_)
      n <- sum(x)
      s <- length(x)
      if (n == 0 || s <= 1) return(NA_real_)
      alpha <- s / log(n)
      for (i in seq_len(50)) {
        f_val <- alpha * log(1 + n / alpha) - s
        f_deriv <- log(1 + n / alpha) - n / (alpha + n)
        if (abs(f_deriv) < .Machine$double.eps) break
        alpha_new <- alpha - f_val / f_deriv
        if (alpha_new <= 0) alpha_new <- alpha / 2
        if (abs(alpha_new - alpha) < 1e-8) break
        alpha <- alpha_new
      }
      alpha
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
