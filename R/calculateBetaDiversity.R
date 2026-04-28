#' Calculate Beta Diversity Distance Matrix
#'
#' Computes pairwise dissimilarity/distance matrices between samples using
#' standard ecological distance metrics.
#'
#' @param data A matrix or data.frame with rows as samples and columns as taxa.
#'   Values must be non-negative counts or relative abundances.
#' @param method Character string specifying the distance metric. One of
#'   \code{"bray"} (Bray-Curtis), \code{"jaccard"} (Jaccard), or
#'   \code{"euclidean"} (Euclidean). Default: \code{"bray"}.
#' @param binary Logical. If \code{TRUE}, data are converted to
#'   presence/absence before computing distances. Default: \code{FALSE}.
#' @return A \code{dist} object containing the pairwise distance matrix.
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Sample", 1:10)
#' colnames(counts) <- paste0("OTU", 1:6)
#' dist_mat <- calculateBetaDiversity(counts, method = "bray")
#'
#' @references
#' Bray, J.R. and Curtis, J.T. (1957). An ordination of the upland forest
#' communities of southern Wisconsin. Ecological Monographs, 27(4), 325-349.
#'
#' @importFrom vegan vegdist
#' @export
calculateBetaDiversity <- function(data, method = "bray", binary = FALSE) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame.")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }
  valid_methods <- c("bray", "jaccard", "euclidean")
  if (!method %in% valid_methods) {
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))
  }
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    data <- as.matrix(data[, numeric_cols, drop = FALSE])
  }
  if (any(data < 0, na.rm = TRUE)) {
    stop("Data must contain only non-negative values.")
  }
  vegan::vegdist(data, method = method, binary = binary)
}
