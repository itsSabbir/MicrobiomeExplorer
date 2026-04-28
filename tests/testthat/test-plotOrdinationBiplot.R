test_that("plotOrdinationBiplot returns a ggplot object", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  ord <- performOrdination(counts, method = "PCoA")
  p <- plotOrdinationBiplot(ord)
  expect_s3_class(p, "ggplot")
})

test_that("plotOrdinationBiplot works with color_var and sample_info", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  ord  <- performOrdination(counts, method = "PCoA")
  meta <- data.frame(Group = rep(c("A", "B"), 5),
                     row.names = paste0("Sample", 1:10))
  p <- plotOrdinationBiplot(ord, sample_info = meta, color_var = "Group")
  expect_s3_class(p, "ggplot")
})

test_that("plotOrdinationBiplot ellipse adds extra layer", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  ord  <- performOrdination(counts, method = "PCoA")
  meta <- data.frame(Group = rep(c("A", "B"), 5),
                     row.names = paste0("Sample", 1:10))
  p_no  <- plotOrdinationBiplot(ord, meta, color_var = "Group", ellipse = FALSE)
  p_yes <- plotOrdinationBiplot(ord, meta, color_var = "Group", ellipse = TRUE)
  expect_gt(length(p_yes$layers), length(p_no$layers))
})

test_that("plotOrdinationBiplot label_samples works without error", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  ord <- performOrdination(counts, method = "PCoA")
  expect_s3_class(plotOrdinationBiplot(ord, label_samples = TRUE), "ggplot")
})

test_that("plotOrdinationBiplot errors on invalid input", {
  expect_error(plotOrdinationBiplot(list(x = 1)), "performOrdination")
})
