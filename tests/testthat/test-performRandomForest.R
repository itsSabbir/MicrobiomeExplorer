.make_random_forest_fixture <- function(groups = rep(c("A", "B"), each = 8),
                                        n_taxa = 6, signal = 90) {
  data <- matrix(1, nrow = length(groups), ncol = n_taxa)
  rownames(data) <- paste0("S", seq_along(groups))
  colnames(data) <- paste0("OTU", seq_len(n_taxa))
  data[groups == "A", "OTU1"] <- signal
  data[groups == "B", "OTU2"] <- signal
  sample_info <- data.frame(Group = groups, row.names = rownames(data))
  list(data = data, sample_info = sample_info)
}

test_that("performRandomForest returns expected structure", {
  skip_if_not_installed("randomForest")
  fixture <- .make_random_forest_fixture()

  result <- performRandomForest(
    fixture$data,
    fixture$sample_info,
    "Group",
    ntree = 25,
    test_fraction = 0.25,
    seed = 20
  )

  expect_named(result, c(
    "model", "confusion_matrix", "accuracy", "importance", "oob_error",
    "predictions", "actual", "train_indices", "test_indices"
  ))
  expect_s3_class(result$model, "randomForest")
  expect_true(is.table(result$confusion_matrix))
  expect_equal(length(result$predictions), length(result$actual))
  expect_equal(length(result$train_indices) + length(result$test_indices), nrow(fixture$data))
})

test_that("performRandomForest returns one importance row per taxon", {
  skip_if_not_installed("randomForest")
  fixture <- .make_random_forest_fixture()

  result <- performRandomForest(
    fixture$data,
    fixture$sample_info,
    "Group",
    ntree = 25,
    test_fraction = 0.25,
    seed = 21
  )

  expect_true(is.data.frame(result$importance))
  expect_true(all(c("MeanDecreaseGini", "Taxon") %in% colnames(result$importance)))
  expect_equal(nrow(result$importance), ncol(fixture$data))
  expect_setequal(result$importance$Taxon, colnames(fixture$data))
})

test_that("performRandomForest errors on mismatched rownames", {
  skip_if_not_installed("randomForest")
  fixture <- .make_random_forest_fixture()
  rownames(fixture$sample_info) <- paste0("X", seq_len(nrow(fixture$sample_info)))

  expect_error(
    performRandomForest(
      fixture$data,
      fixture$sample_info,
      "Group",
      ntree = 10
    ),
    "At least 6 samples with matching IDs"
  )
})

test_that("performRandomForest reports valid separable accuracy", {
  skip_if_not_installed("randomForest")
  fixture <- .make_random_forest_fixture(signal = 120)

  result <- performRandomForest(
    fixture$data,
    fixture$sample_info,
    "Group",
    ntree = 50,
    test_fraction = 0.25,
    seed = 22
  )

  expect_gte(result$accuracy, 0)
  expect_lte(result$accuracy, 1)
  expect_gte(result$accuracy, 0.8)
})
