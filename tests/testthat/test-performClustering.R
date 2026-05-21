# Interpretation: k-means reports "euclidean" because its silhouette uses dist(input);
# distance-based methods report the ecological distance passed via dist_method.
make_clustering_counts <- function() {
  counts <- rbind(
    c(10, 9, 0, 0),
    c(9, 11, 0, 0),
    c(11, 10, 0, 1),
    c(10, 10, 1, 0),
    c(0, 0, 10, 9),
    c(0, 1, 9, 11),
    c(1, 0, 11, 10),
    c(0, 0, 10, 10)
  )
  rownames(counts) <- paste0("S", seq_len(nrow(counts)))
  colnames(counts) <- paste0("OTU", seq_len(ncol(counts)))
  counts
}

test_that("performClustering returns expected kmeans structure", {
  skip_if_not_installed("cluster")
  set.seed(42)
  counts <- make_clustering_counts()

  result <- performClustering(counts, method = "kmeans", k = 2,
                              dist_method = "bray")
  expected_sil <- cluster::silhouette(
    result$cluster_assignments,
    stats::dist(scale(counts))
  )[, "sil_width"]

  expect_named(
    result,
    c("cluster_assignments", "method", "k", "dist_method_used",
      "avg_silhouette", "silhouette_scores")
  )
  expect_equal(result$method, "kmeans")
  expect_equal(result$k, 2)
  expect_equal(result$dist_method_used, "euclidean")
  expect_equal(unname(result$silhouette_scores), unname(expected_sil))
  expect_equal(result$avg_silhouette, mean(expected_sil))
})

test_that("performClustering separates well-separated kmeans clusters", {
  set.seed(42)
  counts <- make_clustering_counts()

  result <- performClustering(counts, method = "kmeans", k = 2)
  assignments <- result$cluster_assignments

  expect_equal(length(unique(assignments[1:4])), 1)
  expect_equal(length(unique(assignments[5:8])), 1)
  expect_false(assignments[1] == assignments[5])
})

test_that("performClustering auto-k keeps kmeans silhouette metric consistent", {
  skip_if_not_installed("cluster")
  set.seed(42)
  counts <- make_clustering_counts()

  result <- performClustering(counts, method = "kmeans", k = NULL,
                              max_k = 3, dist_method = "bray")
  expected_sil <- cluster::silhouette(
    result$cluster_assignments,
    stats::dist(scale(counts))
  )[, "sil_width"]

  expect_equal(result$k, 2)
  expect_equal(result$dist_method_used, "euclidean")
  expect_equal(unname(result$silhouette_scores), unname(expected_sil))
})

test_that("performClustering supports hierarchical clustering", {
  counts <- make_clustering_counts()

  result <- performClustering(counts, method = "hierarchical", k = 2,
                              dist_method = "bray")

  expect_equal(result$method, "hierarchical")
  expect_equal(result$k, 2)
  expect_equal(result$dist_method_used, "bray")
  expect_length(result$cluster_assignments, nrow(counts))
})

test_that("performClustering supports DBSCAN clustering", {
  skip_if_not_installed("dbscan")
  counts <- make_clustering_counts()

  result <- performClustering(counts, method = "dbscan", dbscan_eps = 0.3,
                              dbscan_minPts = 2)

  expect_equal(result$method, "dbscan")
  expect_equal(result$dist_method_used, "bray")
  expect_length(result$cluster_assignments, nrow(counts))
  expect_gte(result$k, 1)
})

test_that("performClustering errors when fewer than four samples are supplied", {
  counts <- matrix(1:9, nrow = 3)

  expect_error(performClustering(counts, method = "kmeans", k = 2),
               "At least 4 samples")
})
