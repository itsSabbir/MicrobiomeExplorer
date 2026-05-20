test_that("advancedRarefactionPlot returns a ggplot with rarefaction axes", {
  counts <- matrix(c(5, 0, 5, 2, 3, 5), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Sample_A", "Sample_B")
  colnames(counts) <- paste0("Taxa_", 1:3)

  plot <- advancedRarefactionPlot(counts, n_steps = 4L)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Sequencing Depth")
  expect_equal(plot$labels$y, "Expected Species Richness")
})

test_that("advancedRarefactionPlot uses full sequencing-depth range", {
  counts <- matrix(c(20, 0, 0, 10, 10, 20), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Low", "High")
  colnames(counts) <- paste0("Taxa_", 1:3)

  built <- ggplot2::ggplot_build(advancedRarefactionPlot(counts, n_steps = 3L))
  line_data <- built$data[[1]]
  vline_data <- built$data[[2]]

  expect_equal(min(line_data$x), 1)
  expect_equal(max(line_data$x), max(rowSums(counts)))
  expect_equal(vline_data$xintercept, min(rowSums(counts)))
})

test_that("monoculture rarefaction curves stay flat at one species", {
  counts <- matrix(c(8, 0, 0, 16, 0, 0), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Small", "Large")
  colnames(counts) <- paste0("Taxa_", 1:3)

  built <- ggplot2::ggplot_build(advancedRarefactionPlot(counts, step = 5L))
  line_data <- built$data[[1]]

  expect_equal(unique(line_data$y), 1)
})

test_that("advancedRarefactionPlot errors on non-numeric input", {
  counts <- data.frame(
    Taxa_1 = c(1, 2),
    Taxa_2 = c("bad", "input")
  )
  rownames(counts) <- c("Sample_A", "Sample_B")

  expect_error(advancedRarefactionPlot(counts), "numeric")
})

test_that("uneven library-size curves reach each sample terminal depth", {
  counts <- matrix(c(4, 4, 0, 50, 25, 25), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Reads_8", "Reads_100")
  colnames(counts) <- paste0("Taxa_", 1:3)

  built <- ggplot2::ggplot_build(advancedRarefactionPlot(counts, step = 10L))
  line_data <- built$data[[1]]
  terminal_depths <- vapply(split(line_data$x, line_data$group), max, numeric(1))

  expect_equal(sort(unname(terminal_depths)), c(8, 100))
})

test_that("legacy rarefaction arguments warn instead of being silently ignored", {
  counts <- matrix(c(5, 5, 10, 0), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("Sample_A", "Sample_B")
  colnames(counts) <- paste0("Taxa_", 1:2)

  expect_warning(
    advancedRarefactionPlot(counts, indices = "Shannon"),
    "Ignoring unsupported argument"
  )
})
