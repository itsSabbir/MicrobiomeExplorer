#' Unsupervised Clustering of Microbiome Samples
#'
#' Clusters samples using k-means, hierarchical, or DBSCAN methods. When
#' \code{k = NULL}, the optimal cluster count is selected via silhouette width.
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param method Character string: \code{"kmeans"}, \code{"hierarchical"}, or
#'   \code{"dbscan"}. Default: \code{"kmeans"}.
#' @param k Integer or \code{NULL}. Number of clusters; \code{NULL} triggers
#'   automatic selection. Default: \code{NULL}.
#' @param max_k Integer. Maximum k to evaluate during auto-selection.
#'   Default: \code{10}.
#' @param dist_method Character string for distance calculation (passed to
#'   \code{\link{calculateBetaDiversity}}). Default: \code{"bray"}.
#' @param dbscan_eps Numeric. DBSCAN epsilon. Default: \code{NULL} (auto).
#' @param dbscan_minPts Integer. DBSCAN minimum points. Default: \code{5}.
#' @param scale_data Logical. Centre and scale before clustering.
#'   Default: \code{TRUE}.
#' @return A list with:
#'   \describe{
#'     \item{cluster_assignments}{Named integer vector of cluster labels.}
#'     \item{method}{Character string.}
#'     \item{k}{Final number of clusters.}
#'     \item{avg_silhouette}{Mean silhouette width.}
#'     \item{silhouette_scores}{Per-sample silhouette widths (when
#'       \code{cluster} package is available).}
#'   }
#'
#' @examples
#' \dontrun{
#' counts <- matrix(rpois(100, 15), nrow = 20, ncol = 5)
#' rownames(counts) <- paste0("S", 1:20)
#' cl <- performClustering(counts, method = "kmeans", k = 3)
#' table(cl$cluster_assignments)
#' }
#'
#' @export
performClustering <- function(data, method = "kmeans", k = NULL, max_k = 10,
                                dist_method = "bray", dbscan_eps = NULL,
                                dbscan_minPts = 5, scale_data = TRUE) {
  if (!method %in% c("kmeans", "hierarchical", "dbscan")) {
    stop("method must be 'kmeans', 'hierarchical', or 'dbscan'.")
  }
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
  }
  if (nrow(data) < 4) stop("At least 4 samples are needed for clustering.")

  sample_names <- rownames(data)
  input <- if (scale_data) scale(data) else data
  dist_mat <- calculateBetaDiversity(data, method = dist_method)

  # Auto-select k via silhouette if k is NULL
  if (is.null(k) && method != "dbscan") {
    max_k <- min(max_k, nrow(data) - 1)
    if (max_k < 2) stop("Not enough samples for automatic k selection.")
    has_cluster <- requireNamespace("cluster", quietly = TRUE)
    best_k <- 2
    best_sil <- -1
    for (ki in 2:max_k) {
      cl <- stats::kmeans(input, centers = ki, nstart = 25)$cluster
      if (has_cluster) {
        sil <- mean(cluster::silhouette(cl, dist_mat)[, "sil_width"])
      } else {
        sil <- 0
      }
      if (sil > best_sil) {
        best_sil <- sil
        best_k <- ki
      }
    }
    k <- best_k
  }

  if (method == "kmeans") {
    km <- stats::kmeans(input, centers = k, nstart = 25)
    clusters <- km$cluster
  } else if (method == "hierarchical") {
    hc <- stats::hclust(dist_mat, method = "ward.D2")
    clusters <- stats::cutree(hc, k = k)
  } else {
    if (!requireNamespace("dbscan", quietly = TRUE)) {
      stop("Package 'dbscan' is required. Install with: install.packages('dbscan')")
    }
    if (is.null(dbscan_eps)) {
      knn_dists <- sort(dbscan::kNNdist(as.matrix(dist_mat), k = dbscan_minPts))
      dbscan_eps <- knn_dists[ceiling(length(knn_dists) * 0.9)]
    }
    db_res <- dbscan::dbscan(as.matrix(dist_mat), eps = dbscan_eps,
                              minPts = dbscan_minPts)
    clusters <- db_res$cluster
    k <- length(unique(clusters[clusters > 0]))
  }

  names(clusters) <- sample_names

  avg_sil <- NA_real_
  sil_scores <- NULL
  if (requireNamespace("cluster", quietly = TRUE) && length(unique(clusters)) > 1) {
    sil <- cluster::silhouette(clusters, dist_mat)
    sil_scores <- sil[, "sil_width"]
    avg_sil <- mean(sil_scores)
  }

  list(
    cluster_assignments = clusters,
    method = method,
    k = k,
    avg_silhouette = avg_sil,
    silhouette_scores = sil_scores
  )
}
