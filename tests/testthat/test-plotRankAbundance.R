test_that("plotRankAbundance returns sorted relative abundance ranks", {
  counts <- matrix(c(10, 5, 0,
                     30, 5, 10),
                   nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Sample_A", "Sample_B")
  colnames(counts) <- c("Taxon_A", "Taxon_B", "Taxon_C")

  plot <- plotRankAbundance(counts, top_n = 2, log_scale = FALSE)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Species Rank")
  expect_equal(plot$labels$y, "Relative Abundance")
  expect_equal(plot$data$Rank, c(1L, 2L))
  expect_equal(as.character(plot$data$Taxon), c("Taxon_A", "Taxon_B"))
  expect_equal(plot$data$RelativeAbundance, c(40 / 60, 10 / 60))
})

test_that("plotRankAbundance keeps relative abundances normalized", {
  counts <- matrix(c(2, 2, 6,
                     2, 2, 6),
                   nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Sample_A", "Sample_B")
  colnames(counts) <- c("Taxon_A", "Taxon_B", "Taxon_C")

  plot <- plotRankAbundance(counts, log_scale = FALSE)

  expect_equal(sum(plot$data$RelativeAbundance), 1)
  expect_equal(plot$data$RelativeAbundance[1], 0.6)
})

test_that("plotRankAbundance can use a log10 abundance scale", {
  counts <- matrix(c(10, 1,
                     20, 2),
                   nrow = 2, byrow = TRUE)
  colnames(counts) <- c("Taxon_A", "Taxon_B")

  plot <- plotRankAbundance(counts, log_scale = TRUE)
  built <- ggplot2::ggplot_build(plot)

  expect_equal(plot$labels$y, "Relative Abundance (log10 scale)")
  expect_true(all(is.finite(built$data[[1]]$y)))
})

test_that("plotRankAbundance validates numeric abundance data", {
  counts <- data.frame(Taxon_A = c("bad", "input"),
                       Taxon_B = c("still", "bad"))

  expect_error(plotRankAbundance(counts), "numeric")
})

test_that("plotRankAbundance validates top_n", {
  counts <- matrix(c(1, 2, 3, 4), nrow = 2)
  colnames(counts) <- c("Taxon_A", "Taxon_B")

  expect_error(plotRankAbundance(counts, top_n = 0), "positive integer")
})
