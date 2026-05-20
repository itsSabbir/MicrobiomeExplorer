# Interpretation: the sparse-fold guard applies to assigned validation folds,
# because those folds drive the reported cross-validation accuracy.
.make_classification_fixture <- function(groups = rep(c("A", "B"), each = 6),
                                         n_taxa = 6, signal = 80) {
  data <- matrix(1, nrow = length(groups), ncol = n_taxa)
  rownames(data) <- paste0("S", seq_along(groups))
  colnames(data) <- paste0("OTU", seq_len(n_taxa))
  data[groups == "A", "OTU1"] <- signal
  data[groups == "B", "OTU2"] <- signal
  sample_info <- data.frame(Group = groups, row.names = rownames(data))
  list(data = data, sample_info = sample_info)
}

test_that("performClassification returns expected structure", {
  skip_if_not_installed("randomForest")
  fixture <- .make_classification_fixture()

  result <- suppressMessages(performClassification(
    fixture$data,
    fixture$sample_info,
    "Group",
    n_folds = 3,
    n_repeats = 1,
    ntree = 25,
    seed = 10
  ))

  expect_named(result, c(
    "cv_accuracy", "cv_accuracy_sd", "auc", "roc_data",
    "confusion_matrix", "feature_importance"
  ))
  expect_true(is.table(result$confusion_matrix))
  expect_true(is.data.frame(result$feature_importance))
  expect_true(all(c("MeanDecreaseGini", "Taxon") %in% colnames(result$feature_importance)))
  expect_equal(nrow(result$feature_importance), ncol(fixture$data))
})

test_that("performClassification reports accuracy within probability bounds", {
  skip_if_not_installed("randomForest")
  fixture <- .make_classification_fixture()

  result <- suppressMessages(performClassification(
    fixture$data,
    fixture$sample_info,
    "Group",
    n_folds = 3,
    n_repeats = 1,
    ntree = 25,
    seed = 11
  ))

  expect_gte(result$cv_accuracy, 0)
  expect_lte(result$cv_accuracy, 1)
  expect_true(is.finite(result$cv_accuracy_sd))
})

test_that("performClassification learns separable sample groups", {
  skip_if_not_installed("randomForest")
  fixture <- .make_classification_fixture(signal = 120)

  result <- suppressMessages(performClassification(
    fixture$data,
    fixture$sample_info,
    "Group",
    n_folds = 3,
    n_repeats = 2,
    ntree = 50,
    seed = 12
  ))

  expect_gte(result$cv_accuracy, 0.9)
})

test_that("performClassification warns for sparse imbalanced folds", {
  skip_if_not_installed("randomForest")
  fixture <- .make_classification_fixture(groups = c(rep("A", 9), rep("B", 3)))

  expect_warning(
    result <- suppressMessages(performClassification(
      fixture$data,
      fixture$sample_info,
      "Group",
      n_folds = 3,
      n_repeats = 1,
      ntree = 25,
      seed = 13
    )),
    "fewer than 2 samples"
  )
  expect_gte(result$cv_accuracy, 0)
  expect_lte(result$cv_accuracy, 1)
  expect_equal(sum(result$confusion_matrix), nrow(fixture$data))
})

test_that("performClassification errors on fewer than 6 samples", {
  skip_if_not_installed("randomForest")
  fixture <- .make_classification_fixture(groups = c("A", "B", "A", "B", "A"))

  expect_error(
    suppressMessages(performClassification(
      fixture$data,
      fixture$sample_info,
      "Group",
      n_folds = 2,
      n_repeats = 1,
      ntree = 10
    )),
    "At least 6 matching samples"
  )
})
