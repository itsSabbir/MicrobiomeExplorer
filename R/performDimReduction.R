#' Dimensionality Reduction via t-SNE or UMAP
#'
#' Computes 2D embeddings using t-SNE or UMAP, returning coordinates in the
#' same format as \code{\link{performOrdination}} so that
#' \code{\link{plotOrdinationBiplot}} works directly.
#'
#' @param data A matrix or data.frame (rows = samples, cols = taxa).
#' @param method Character string: \code{"tSNE"} or \code{"UMAP"}.
#'   Default: \code{"tSNE"}.
#' @param perplexity Numeric. t-SNE perplexity; auto-capped at
#'   \code{floor((nrow(data) - 1) / 3)} for small datasets. Default: \code{30}.
#' @param n_neighbors Integer. Number of UMAP neighbours. Default: \code{15}.
#' @param min_dist Numeric. UMAP minimum distance. Default: \code{0.1}.
#' @param dist_method Character string. Distance metric for the input distance
#'   matrix (passed to \code{\link{calculateBetaDiversity}}).
#'   Default: \code{"bray"}.
#' @param scale_data Logical. Centre and scale columns before computing.
#'   Default: \code{TRUE}.
#' @param seed Integer. Random seed for reproducibility. Default: \code{42}.
#' @return A list compatible with \code{\link{plotOrdinationBiplot}}:
#'   \describe{
#'     \item{coordinates}{data.frame with \code{Axis1}, \code{Axis2}.}
#'     \item{method}{Character string (\code{"tSNE"} or \code{"UMAP"}).}
#'     \item{variance_explained}{\code{NULL} (not applicable).}
#'     \item{stress}{\code{NULL}.}
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' counts <- matrix(rpois(100, 15), nrow = 20, ncol = 5)
#' rownames(counts) <- paste0("S", 1:20)
#' res <- performDimReduction(counts, method = "tSNE", perplexity = 5)
#' plotOrdinationBiplot(res)
#' }
#'
#' @export
performDimReduction <- function(data, method = "tSNE", perplexity = 30,
                                 n_neighbors = 15, min_dist = 0.1,
                                 dist_method = "bray", scale_data = TRUE,
                                 seed = 42) {
  if (!method %in% c("tSNE", "UMAP")) {
    stop("method must be 'tSNE' or 'UMAP'.")
  }
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
  }
  if (nrow(data) < 4) {
    stop("At least 4 samples are required for dimensionality reduction.")
  }

  sample_names <- rownames(data)
  set.seed(seed)

  if (method == "tSNE") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      stop("Package 'Rtsne' is required for t-SNE. Install with: install.packages('Rtsne')")
    }
    dist_mat <- as.matrix(calculateBetaDiversity(data, method = dist_method))
    perp <- min(perplexity, floor((nrow(data) - 1) / 3))
    tsne_res <- Rtsne::Rtsne(dist_mat, is_distance = TRUE, dims = 2,
                              perplexity = perp, check_duplicates = FALSE)
    coords <- as.data.frame(tsne_res$Y)
  } else {
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop("Package 'umap' is required for UMAP. Install with: install.packages('umap')")
    }
    input <- if (scale_data) scale(data) else data
    umap_cfg <- umap::umap.defaults
    umap_cfg$n_neighbors <- min(n_neighbors, nrow(data) - 1)
    umap_cfg$min_dist <- min_dist
    umap_res <- umap::umap(input, config = umap_cfg)
    coords <- as.data.frame(umap_res$layout)
  }

  colnames(coords) <- c("Axis1", "Axis2")
  rownames(coords) <- sample_names

  list(coordinates = coords, method = method,
       variance_explained = NULL, stress = NULL)
}
