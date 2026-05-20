.shared_taxa_fixture <- function() {
  counts <- matrix(c(1, 1, 0, 0, 0,
                     1, 0, 0, 0, 0,
                     1, 0, 1, 0, 0,
                     1, 0, 1, 0, 0,
                     0, 0, 0, 5, 0,
                     0, 0, 0, 5, 0),
                   nrow = 6, byrow = TRUE)
  rownames(counts) <- paste0("Sample_", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Taxon_", seq_len(ncol(counts)))
  sample_info <- data.frame(Group = rep(c("A", "B", "C"), each = 2),
                            row.names = rownames(counts))

  list(counts = counts, sample_info = sample_info)
}

test_that("plotSharedTaxa returns shared and unique taxon counts", {
  fixture <- .shared_taxa_fixture()

  plot <- plotSharedTaxa(fixture$counts, fixture$sample_info, "Group",
                         min_prevalence = 0.5)
  counts_by_set <- stats::setNames(plot$data$TaxaCount,
                                   as.character(plot$data$GroupSet))

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Group Set")
  expect_equal(plot$labels$y, "Number of Taxa")
  expect_equal(counts_by_set[["A & B"]], 1L)
  expect_equal(counts_by_set[["A"]], 1L)
  expect_equal(counts_by_set[["B"]], 1L)
  expect_equal(counts_by_set[["C"]], 1L)
})

test_that("plotSharedTaxa applies group-level min_prevalence", {
  fixture <- .shared_taxa_fixture()

  plot <- plotSharedTaxa(fixture$counts, fixture$sample_info, "Group",
                         min_prevalence = 1)

  expect_false("A" %in% as.character(plot$data$GroupSet))
  expect_true("A & B" %in% as.character(plot$data$GroupSet))
})

test_that("plotSharedTaxa can match sample_info by Sample column", {
  fixture <- .shared_taxa_fixture()
  sample_info <- data.frame(Sample = rownames(fixture$counts),
                            Group = fixture$sample_info$Group)

  plot <- plotSharedTaxa(fixture$counts, sample_info, "Group",
                         min_prevalence = 0.5)

  expect_s3_class(plot, "ggplot")
  expect_true("A & B" %in% as.character(plot$data$GroupSet))
})

test_that("plotSharedTaxa validates the grouping variable", {
  fixture <- .shared_taxa_fixture()

  expect_error(
    plotSharedTaxa(fixture$counts, fixture$sample_info, "Missing",
                   min_prevalence = 0.5),
    "group_var"
  )
})

test_that("plotSharedTaxa requires at least two groups", {
  fixture <- .shared_taxa_fixture()
  counts <- fixture$counts[1:2, , drop = FALSE]
  sample_info <- fixture$sample_info[1:2, , drop = FALSE]

  expect_error(
    plotSharedTaxa(counts, sample_info, "Group", min_prevalence = 0.5),
    "at least two groups"
  )
})
