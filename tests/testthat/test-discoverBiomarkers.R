.make_biomarker_data <- function(n_taxa = 12) {
  set.seed(42)
  groups <- rep(c("A", "B"), each = 6)
  data <- matrix(rpois(length(groups) * n_taxa, lambda = 20),
                 nrow = length(groups), ncol = n_taxa)
  rownames(data) <- paste0("S", seq_along(groups))
  colnames(data) <- paste0("OTU", seq_len(n_taxa))
  data[groups == "B", "OTU1"] <- data[groups == "B", "OTU1"] + 80
  sample_info <- data.frame(Group = groups, row.names = rownames(data))
  list(data = data, sample_info = sample_info)
}

test_that("discoverBiomarkers reports BH-adjusted p-values", {
  fixture <- .make_biomarker_data(n_taxa = 12)

  result <- discoverBiomarkers(
    fixture$data,
    fixture$sample_info,
    "Group",
    method = "kw",
    effect_size_threshold = 0
  )

  expect_true(all(c("pvalue", "padj") %in% colnames(result$all_results)))
  expect_true(all(c("pvalue", "padj") %in% colnames(result$biomarkers)))
  expect_true(all(result$all_results$padj >= result$all_results$pvalue))
})

test_that("discoverBiomarkers logs and falls back for invalid KW p-values", {
  fixture <- .make_biomarker_data(n_taxa = 12)
  fixture$data[, "OTU2"] <- 5

  expect_message(
    result <- discoverBiomarkers(
      fixture$data,
      fixture$sample_info,
      "Group",
      method = "kw",
      effect_size_threshold = 0
    ),
    "\\[biomark\\] KW failed for taxon 'OTU2'"
  )

  constant_result <- result$all_results[result$all_results$taxon == "OTU2", ]
  expect_equal(constant_result$pvalue, 1.0)
})

test_that("discoverBiomarkers supports deprecated lda_threshold alias", {
  fixture <- .make_biomarker_data(n_taxa = 12)

  expect_warning(
    result <- discoverBiomarkers(
      fixture$data,
      fixture$sample_info,
      "Group",
      method = "kw",
      lda_threshold = 0
    ),
    "deprecated"
  )

  expect_true(is.list(result))
  expect_true("biomarkers" %in% names(result))
})

test_that("discoverBiomarkers kw method returns expected structure", {
  fixture <- .make_biomarker_data(n_taxa = 12)

  result <- discoverBiomarkers(
    fixture$data,
    fixture$sample_info,
    "Group",
    method = "kw",
    effect_size_threshold = 0
  )

  expect_named(result, c("biomarkers", "all_results", "method", "n_significant"))
  expect_true(is.data.frame(result$biomarkers))
  expect_true(is.data.frame(result$all_results))
  expect_equal(result$method, "kw")
  expect_equal(result$n_significant, nrow(result$biomarkers))
})
