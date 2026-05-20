make_network_counts <- function() {
  counts <- cbind(
    TaxonA = seq_len(10),
    TaxonB = seq_len(10) * 2,
    TaxonC = c(1, 3, 2, 5, 4, 7, 6, 9, 8, 10),
    TaxonD = c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8)
  )
  rownames(counts) <- paste0("S", seq_len(nrow(counts)))
  counts
}

test_that("buildCooccurrenceNetwork returns expected structure", {
  skip_if_not_installed("igraph")
  counts <- make_network_counts()

  result <- buildCooccurrenceNetwork(counts, method = "pearson",
                                     cor_threshold = 0.99,
                                     pval_threshold = 0.05,
                                     min_prevalence = 0)

  expect_named(result, c("graph", "node_metrics", "edge_list", "n_nodes",
                         "n_edges", "modularity"))
  expect_s3_class(result$graph, "igraph")
  expect_s3_class(result$node_metrics, "data.frame")
  expect_s3_class(result$edge_list, "data.frame")
  expect_equal(result$n_edges, nrow(result$edge_list))
})

test_that("buildCooccurrenceNetwork includes a correlated pair edge", {
  skip_if_not_installed("igraph")
  counts <- make_network_counts()

  result <- buildCooccurrenceNetwork(counts, method = "pearson",
                                     cor_threshold = 0.99,
                                     pval_threshold = 0.05,
                                     min_prevalence = 0)

  expect_equal(result$n_edges, 1)
  expect_setequal(c(result$edge_list$from[1], result$edge_list$to[1]),
                  c("TaxonA", "TaxonB"))
  expect_equal(result$edge_list$correlation[1], 1)
})

test_that("buildCooccurrenceNetwork filters edges by correlation threshold", {
  skip_if_not_installed("igraph")
  counts <- make_network_counts()

  loose <- buildCooccurrenceNetwork(counts, method = "pearson",
                                    cor_threshold = 0.8,
                                    pval_threshold = 0.05,
                                    min_prevalence = 0)
  strict <- buildCooccurrenceNetwork(counts, method = "pearson",
                                     cor_threshold = 0.99,
                                     pval_threshold = 0.05,
                                     min_prevalence = 0)

  expect_gt(loose$n_edges, strict$n_edges)
  expect_equal(strict$n_edges, 1)
})

test_that("buildCooccurrenceNetwork errors on non-numeric data", {
  skip_if_not_installed("igraph")
  data <- data.frame(
    TaxonA = letters[1:5],
    TaxonB = letters[6:10]
  )

  expect_error(buildCooccurrenceNetwork(data), "numeric")
})
