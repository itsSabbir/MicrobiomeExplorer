make_unifrac_fixture <- function() {
  counts <- matrix(
    c(
      10, 0, 5, 0,
      1, 0, 20, 0,
      0, 12, 0, 8
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(counts) <- paste0("S", 1:3)
  colnames(counts) <- paste0("OTU", 1:4)

  list(
    counts = counts,
    tree = ape::read.tree(text = "((OTU1:0.2,OTU2:0.2):0.3,(OTU3:0.2,OTU4:0.2):0.3);")
  )
}

test_that("calculateBetaDiversity returns a dist object", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  result <- calculateBetaDiversity(counts, method = "bray")
  expect_s3_class(result, "dist")
})

test_that("calculateBetaDiversity works for all three methods", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  expect_s3_class(calculateBetaDiversity(counts, method = "bray"),      "dist")
  expect_s3_class(calculateBetaDiversity(counts, method = "jaccard"),   "dist")
  expect_s3_class(calculateBetaDiversity(counts, method = "euclidean"), "dist")
})

test_that("calculateBetaDiversity binary flag changes output", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  d_count  <- calculateBetaDiversity(counts, method = "jaccard", binary = FALSE)
  d_binary <- calculateBetaDiversity(counts, method = "jaccard", binary = TRUE)
  expect_false(all(as.numeric(d_count) == as.numeric(d_binary)))
})

test_that("calculateBetaDiversity preserves sample names", {
  set.seed(42)
  counts <- matrix(rpois(30, lambda = 10), nrow = 5, ncol = 6)
  rownames(counts) <- paste0("S", 1:5)
  result <- calculateBetaDiversity(counts)
  expect_equal(attr(result, "Labels"), paste0("S", 1:5))
})

test_that("calculateBetaDiversity errors on invalid method", {
  counts <- matrix(rpois(30, 10), nrow = 5, ncol = 6)
  expect_error(calculateBetaDiversity(counts, method = "manhattan"), "method must be one of")
})

test_that("calculateBetaDiversity errors on negative values", {
  counts <- matrix(c(-1, 1, 2, 3, 4, 5), nrow = 2)
  expect_error(calculateBetaDiversity(counts), "non-negative")
})

test_that("calculateBetaDiversity accepts data.frame input", {
  set.seed(42)
  df <- as.data.frame(matrix(rpois(30, 10), nrow = 5, ncol = 6))
  rownames(df) <- paste0("S", 1:5)
  expect_s3_class(calculateBetaDiversity(df), "dist")
})

test_that("calculateBetaDiversity errors on UniFrac without a tree", {
  fixture <- make_unifrac_fixture()
  expect_error(
    calculateBetaDiversity(fixture$counts, method = "unifrac"),
    "requires a phylogenetic tree"
  )
})

test_that("calculateBetaDiversity returns UniFrac distances with a tree", {
  fixture <- make_unifrac_fixture()
  result <- calculateBetaDiversity(fixture$counts, method = "unifrac", tree = fixture$tree)
  expect_s3_class(result, "dist")
  expect_equal(attr(result, "Labels"), rownames(fixture$counts))
})

test_that("calculateBetaDiversity uses MicrobiomeData tree slot for UniFrac", {
  fixture <- make_unifrac_fixture()
  data_object <- new(
    "MicrobiomeData",
    rRNA16S = fixture$counts,
    PhylogeneticTree = fixture$tree
  )
  result <- calculateBetaDiversity(data_object, method = "unifrac")
  expect_s3_class(result, "dist")
})

test_that("calculateBetaDiversity weighted UniFrac differs from unweighted", {
  fixture <- make_unifrac_fixture()
  unweighted <- calculateBetaDiversity(fixture$counts, method = "unifrac", tree = fixture$tree)
  weighted <- calculateBetaDiversity(fixture$counts, method = "wunifrac", tree = fixture$tree)
  expect_false(isTRUE(all.equal(as.numeric(unweighted), as.numeric(weighted))))
})

test_that("calculateBetaDiversity preserves Bray-Curtis distances", {
  counts <- matrix(
    c(
      1, 2, 3,
      4, 0, 2,
      1, 1, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(counts) <- paste0("S", 1:3)
  result <- as.matrix(calculateBetaDiversity(counts, method = "bray"))
  observed <- result[cbind(c("S1", "S1", "S2"), c("S2", "S3", "S3"))]
  expect_equal(observed, c(0.5, 1 / 3, 5 / 9), tolerance = 1e-8)
})
