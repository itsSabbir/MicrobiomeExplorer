test_that("plotAlphaDiversityBoxplot returns list with plot and test_result", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:5)
  alpha <- calculateAlphaDiversity(counts, indices = c("Shannon", "Simpson"))
  meta  <- data.frame(Group = rep(c("A", "B"), 5), row.names = paste0("S", 1:10))
  result <- plotAlphaDiversityBoxplot(alpha, sample_info = meta,
                                      group_var = "Group", index = "Shannon")
  expect_true(is.list(result))
  expect_true(all(c("plot", "test_result") %in% names(result)))
})

test_that("plotAlphaDiversityBoxplot plot slot is a ggplot", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:5)
  alpha <- calculateAlphaDiversity(counts, indices = "Shannon")
  meta  <- data.frame(Group = rep(c("A", "B"), 5), row.names = paste0("S", 1:10))
  result <- plotAlphaDiversityBoxplot(alpha, meta, "Group", "Shannon")
  expect_s3_class(result$plot, "ggplot")
})

test_that("plotAlphaDiversityBoxplot Kruskal-Wallis test_result is htest", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:5)
  alpha <- calculateAlphaDiversity(counts, indices = "Shannon")
  meta  <- data.frame(Group = rep(c("A", "B"), 5), row.names = paste0("S", 1:10))
  result <- plotAlphaDiversityBoxplot(alpha, meta, "Group", "Shannon", test = "kruskal")
  expect_s3_class(result$test_result, "htest")
})

test_that("plotAlphaDiversityBoxplot violin plot type works", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:5)
  alpha <- calculateAlphaDiversity(counts, indices = "Shannon")
  meta  <- data.frame(Group = rep(c("A", "B"), 5), row.names = paste0("S", 1:10))
  result <- plotAlphaDiversityBoxplot(alpha, meta, "Group", "Shannon", plot_type = "violin")
  expect_s3_class(result$plot, "ggplot")
})

test_that("plotAlphaDiversityBoxplot errors on missing index column", {
  alpha <- data.frame(Sample = paste0("S", 1:5), Shannon = runif(5))
  meta  <- data.frame(Group = rep("A", 5), row.names = paste0("S", 1:5))
  expect_error(plotAlphaDiversityBoxplot(alpha, meta, "Group", index = "Simpson"),
               "not found in alpha_results")
})
