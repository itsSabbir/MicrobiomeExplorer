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
#'   \code{\link{calculateBetaDiversity}}) for hierarchical and DBSCAN methods.
#'   K-means silhouettes use Euclidean \code{dist(input)} to match the data used
#'   by \code{stats::kmeans}. Default: \code{"bray"}.
#' @param dbscan_eps Numeric. DBSCAN epsilon. Default: \code{NULL} (auto).
#' @param dbscan_minPts Integer. DBSCAN minimum points. Default: \code{5}.
#' @param scale_data Logical. Centre and scale before clustering.
#'   Default: \code{TRUE}.
#' @return A list with:
#'   \describe{
#'     \item{cluster_assignments}{Named integer vector of cluster labels.}
#'     \item{method}{Character string.}
#'     \item{k}{Final number of clusters.}
#'     \item{dist_method_used}{Distance metric used for silhouette scoring.}
#'     \item{avg_silhouette}{Mean silhouette width.}
#'     \item{silhouette_scores}{Per-sample silhouette widths (when
#'       \code{cluster} package is available).}
#'   }
#'
#' @examples
#' \donttest{
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
  params <- list(method = method, k = k, max_k = max_k,
                 dist_method = dist_method, dbscan_eps = dbscan_eps,
                 dbscan_minPts = dbscan_minPts, scale_data = scale_data)
  data <- .prepare_clustering_data(data, method)
  sample_names <- rownames(data)
  distances <- .prepare_clustering_distances(data, params)

  if (is.null(params$k) && method != "dbscan") {
    params$k <- .select_cluster_count(distances, params)
  }

  clusters <- .assign_clusters(distances, params)
  names(clusters) <- sample_names
  final_k <- .final_cluster_count(clusters, params)
  silhouette <- .summarize_silhouette(clusters, distances$silhouette_dist)

  list(
    cluster_assignments = clusters,
    method = method,
    k = final_k,
    dist_method_used = distances$dist_method_used,
    avg_silhouette = silhouette$avg,
    silhouette_scores = silhouette$scores
  )
}

.min_clustering_samples <- 4L
.min_auto_k <- 2L
.kmeans_nstart <- 25L
# Preserve the previous high-percentile auto-eps heuristic for compatibility.
.dbscan_eps_quantile <- 0.9

.prepare_clustering_data <- function(data, method) {
  if (!method %in% c("kmeans", "hierarchical", "dbscan")) {
    stop("method must be 'kmeans', 'hierarchical', or 'dbscan'.")
  }
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
  }
  if (!is.numeric(data)) {
    stop("data must be numeric.")
  }
  if (any(data < 0, na.rm = TRUE)) {
    stop("data must contain only non-negative values.")
  }
  if (nrow(data) < .min_clustering_samples) {
    stop("At least 4 samples are needed for clustering.")
  }
  data
}

.prepare_clustering_distances <- function(data, params) {
  input <- if (params$scale_data) scale(data) else data

  if (params$method == "kmeans") {
    message("[cluster] using euclidean distance for kmeans silhouette")
    return(list(input = input, cluster_dist = NULL,
                silhouette_dist = stats::dist(input),
                dist_method_used = "euclidean"))
  }

  message("[cluster] using ", params$dist_method, " distance for ",
          params$method)
  dist_mat <- calculateBetaDiversity(data, method = params$dist_method)
  list(input = input, cluster_dist = dist_mat, silhouette_dist = dist_mat,
       dist_method_used = params$dist_method)
}

.select_cluster_count <- function(distances, params) {
  max_k <- min(params$max_k, attr(distances$silhouette_dist, "Size") - 1L)
  if (max_k < .min_auto_k) {
    stop("Not enough samples for automatic k selection.")
  }
  if (!requireNamespace("cluster", quietly = TRUE)) {
    message("[cluster] cluster package unavailable; using k=", .min_auto_k)
    return(.min_auto_k)
  }

  candidate_k <- seq.int(.min_auto_k, max_k)
  scores <- vapply(candidate_k, function(ki) {
    clusters <- .clusters_for_k(distances, params$method, ki)
    mean(cluster::silhouette(clusters, distances$silhouette_dist)[, "sil_width"])
  }, numeric(1))
  selected <- candidate_k[which.max(scores)]
  message("[cluster] selected k=", selected)
  selected
}

.clusters_for_k <- function(distances, method, k) {
  if (method == "hierarchical") {
    hc <- stats::hclust(distances$cluster_dist, method = "ward.D2")
    return(stats::cutree(hc, k = k))
  }
  stats::kmeans(distances$input, centers = k, nstart = .kmeans_nstart)$cluster
}

.assign_clusters <- function(distances, params) {
  if (params$method != "dbscan") {
    return(.clusters_for_k(distances, params$method, params$k))
  }
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Install with: install.packages('dbscan')")
  }
  dbscan_eps <- params$dbscan_eps
  if (is.null(dbscan_eps)) {
    knn_dists <- sort(dbscan::kNNdist(as.matrix(distances$cluster_dist),
                                      k = params$dbscan_minPts))
    dbscan_eps <- knn_dists[ceiling(length(knn_dists) * .dbscan_eps_quantile)]
    message("[cluster] selected dbscan eps=", signif(dbscan_eps, 4))
  }
  dbscan::dbscan(as.matrix(distances$cluster_dist), eps = dbscan_eps,
                 minPts = params$dbscan_minPts)$cluster
}

.final_cluster_count <- function(clusters, params) {
  if (params$method == "dbscan") {
    return(length(unique(clusters[clusters > 0])))
  }
  params$k
}

.summarize_silhouette <- function(clusters, silhouette_dist) {
  if (!requireNamespace("cluster", quietly = TRUE) ||
      length(unique(clusters)) <= 1) {
    return(list(avg = NA_real_, scores = NULL))
  }
  sil <- cluster::silhouette(clusters, silhouette_dist)
  sil_scores <- sil[, "sil_width"]
  list(avg = mean(sil_scores), scores = sil_scores)
}
