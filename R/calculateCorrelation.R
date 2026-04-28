#' Calculate Pairwise Taxon Correlation Matrix
#'
#' Computes a pairwise correlation matrix between taxa (columns) in a
#' microbiome dataset, with optional prevalence-based filtering.
#'
#' @param data A matrix or data.frame with rows as samples and columns as taxa.
#' @param method Character string: \code{"spearman"} (recommended for count
#'   data) or \code{"pearson"}. Default: \code{"spearman"}.
#' @param min_prevalence Numeric in [0, 1]. Taxa present in fewer than this
#'   proportion of samples are excluded. Default: \code{0.1}.
#' @return A list with:
#'   \describe{
#'     \item{correlation}{Symmetric numeric matrix of correlation coefficients.}
#'     \item{pvalue}{Symmetric numeric matrix of raw p-values.}
#'     \item{method}{Character string naming the method used.}
#'     \item{n_taxa}{Integer. Number of taxa retained after filtering.}
#'   }
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
#' rownames(counts) <- paste0("S", 1:10)
#' colnames(counts) <- paste0("OTU", 1:10)
#' corr <- calculateCorrelation(counts, method = "spearman")
#' dim(corr$correlation)
#'
#' @importFrom stats cor cor.test
#' @export
calculateCorrelation <- function(data, method = "spearman",
                                  min_prevalence = 0.1) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame.")
  }
  if (!method %in% c("spearman", "pearson")) {
    stop("method must be 'spearman' or 'pearson'.")
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
  }
  if (!is.numeric(data)) {
    stop("Data must be numeric.")
  }

  # Prevalence filtering
  prevalence <- colMeans(data > 0, na.rm = TRUE)
  keep <- prevalence >= min_prevalence
  if (sum(keep) < 2) {
    stop(paste("Fewer than 2 taxa pass the min_prevalence threshold of",
               min_prevalence, ". Lower the threshold."))
  }
  data <- data[, keep, drop = FALSE]

  cor_mat <- stats::cor(data, method = method, use = "pairwise.complete.obs")

  # P-values via t-distribution (avoids external package dependency)
  n <- nrow(data)
  t_stat <- cor_mat * sqrt((n - 2) / (1 - cor_mat^2))
  p_mat  <- 2 * stats::pt(-abs(t_stat), df = n - 2)
  diag(p_mat) <- 0

  list(correlation = cor_mat, pvalue = p_mat,
       method = method, n_taxa = ncol(data))
}
