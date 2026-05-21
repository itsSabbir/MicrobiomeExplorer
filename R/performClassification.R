#' Cross-Validated Classification with ROC/AUC Evaluation
#'
#' Trains and evaluates a random forest classifier using repeated stratified
#' k-fold cross-validation, returning per-fold accuracy, aggregated confusion
#' matrix, and ROC data (when \pkg{pROC} is available).
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param sample_info A data.frame with sample metadata.
#' @param group_var Character string naming the binary response column.
#' @param n_folds Integer. Number of CV folds. Default: \code{5}.
#' @param n_repeats Integer. Repeated CV iterations. Default: \code{10}.
#' @param ntree Integer. Trees per fold. Default: \code{500}.
#' @param seed Integer. Random seed. Default: \code{42}.
#' @return A list with:
#'   \describe{
#'     \item{cv_accuracy}{Mean accuracy across all folds.}
#'     \item{cv_accuracy_sd}{Standard deviation of fold accuracies.}
#'     \item{auc}{Mean AUC (binary only, requires \pkg{pROC}).}
#'     \item{roc_data}{data.frame with FPR, TPR columns for plotting.}
#'     \item{confusion_matrix}{Aggregated confusion matrix.}
#'     \item{feature_importance}{Averaged importance across folds.}
#'   }
#'
#' @examples
#' \donttest{
#' counts <- matrix(rpois(200, 20), nrow = 20, ncol = 10)
#' rownames(counts) <- paste0("S", 1:20)
#' meta <- data.frame(Group = rep(c("A", "B"), 10), row.names = paste0("S", 1:20))
#' cv <- performClassification(counts, meta, "Group", n_folds = 3, n_repeats = 2)
#' cv$cv_accuracy
#' }
#'
#' @export
performClassification <- function(data, sample_info, group_var,
                                    n_folds = 5, n_repeats = 10,
                                    ntree = 500, seed = 42) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required. Install with: install.packages('randomForest')")
  }

  aligned <- .prepare_classification_inputs(data, sample_info, group_var)
  labels <- aligned$labels
  is_binary <- nlevels(labels) == 2

  set.seed(seed)
  fold_results <- .run_classification_repeats(
    aligned$data,
    labels,
    list(n_folds = n_folds, n_repeats = n_repeats,
         ntree = ntree, is_binary = is_binary)
  )
  .summarize_classification_results(fold_results, labels, is_binary)
}

.classification_min_samples <- 6L
.classification_min_fold_class_samples <- 2L

.prepare_classification_inputs <- function(data, sample_info, group_var) {
  if (!group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info."))
  }
  common <- intersect(rownames(data), rownames(sample_info))
  if (length(common) < .classification_min_samples) {
    stop("At least 6 matching samples are needed.")
  }
  data <- data[common, , drop = FALSE]
  labels <- factor(sample_info[common, group_var])

  if (nlevels(labels) < 2) {
    stop("group_var must have at least 2 levels.")
  }
  list(data = data, labels = labels)
}

.run_classification_repeats <- function(data, labels, options) {
  fold_results <- list()
  sparse_warning_sent <- FALSE
  message("[cv] using stratified fold assignment")

  for (rep_i in seq_len(options$n_repeats)) {
    fold_ids <- .stratified_fold_ids(labels, options$n_folds)
    guard <- .classification_fold_guard(labels, fold_ids, options$n_folds)
    if (!guard$ok && !sparse_warning_sent) {
      message(paste("[cv]", guard$reason))
      warning(guard$reason, call. = FALSE)
      sparse_warning_sent <- TRUE
    }

    for (fi in seq_len(options$n_folds)) {
      test_mask <- fold_ids == fi
      train_mask <- !test_mask
      if (sum(test_mask) == 0 || sum(train_mask) == 0) next

      fold_results[[length(fold_results) + 1L]] <- .fit_classification_fold(
        data,
        labels,
        list(train_mask = train_mask, test_mask = test_mask,
             ntree = options$ntree, is_binary = options$is_binary)
      )
    }
  }
  fold_results
}

.stratified_fold_ids <- function(labels, n_folds) {
  n <- length(labels)
  fold_ids <- integer(n)
  class_indices <- split(seq_len(n), labels)
  for (indices in class_indices) {
    fold_sequence <- rep(seq_len(n_folds), length.out = length(indices))
    if (length(indices) < n_folds) {
      fold_sequence <- sample(seq_len(n_folds), length(indices))
    }
    fold_ids[indices] <- sample(fold_sequence, length(fold_sequence))
  }
  fold_ids
}

.classification_fold_guard <- function(labels, fold_ids, n_folds) {
  fold_counts <- lapply(seq_len(n_folds), function(fold_i) {
    table(factor(labels[fold_ids == fold_i], levels = levels(labels)))
  })
  sparse <- any(vapply(fold_counts, function(counts) {
    any(counts < .classification_min_fold_class_samples)
  }, logical(1)))
  if (sparse) {
    return(list(
      ok = FALSE,
      reason = sprintf(
        "One or more CV folds contain fewer than %d samples from at least one class.",
        .classification_min_fold_class_samples
      ),
      payload = fold_counts
    ))
  }
  list(ok = TRUE, reason = "", payload = fold_counts)
}

.fit_classification_fold <- function(data, labels, options) {
  train_mask <- options$train_mask
  test_mask <- options$test_mask
  model <- randomForest::randomForest(
    x = data[train_mask, , drop = FALSE],
    y = labels[train_mask],
    ntree = options$ntree,
    importance = TRUE
  )

  preds <- stats::predict(model, newdata = data[test_mask, , drop = FALSE])
  probabilities <- numeric(0)
  if (options$is_binary) {
    probabilities <- stats::predict(
      model,
      newdata = data[test_mask, , drop = FALSE],
      type = "prob"
    )[, levels(labels)[2]]
  }

  list(
    accuracy = mean(preds == labels[test_mask]),
    predictions = as.character(preds),
    actuals = as.character(labels[test_mask]),
    probabilities = probabilities,
    importance = randomForest::importance(model)[, "MeanDecreaseGini", drop = FALSE]
  )
}

.summarize_classification_results <- function(results, labels, is_binary) {
  fold_accs <- vapply(results, function(result) result$accuracy, numeric(1))
  all_preds <- unlist(lapply(results, `[[`, "predictions"), use.names = FALSE)
  all_actuals <- unlist(lapply(results, `[[`, "actuals"), use.names = FALSE)
  all_probs <- unlist(lapply(results, `[[`, "probabilities"), use.names = FALSE)
  cm <- table(
    Predicted = factor(all_preds, levels = levels(labels)),
    Actual = factor(all_actuals, levels = levels(labels))
  )
  auc_val <- NA_real_
  roc_df <- NULL
  if (is_binary && requireNamespace("pROC", quietly = TRUE) &&
      length(all_probs) > 0) {
    roc_obj <- pROC::roc(all_actuals, all_probs, levels = levels(labels), quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities
    )
  }

  list(
    cv_accuracy = mean(fold_accs),
    cv_accuracy_sd = stats::sd(fold_accs),
    auc = auc_val,
    roc_data = roc_df,
    confusion_matrix = cm,
    feature_importance = .average_classification_importance(results)
  )
}

.average_classification_importance <- function(results) {
  if (length(results) == 0) {
    return(NULL)
  }
  importance <- Reduce("+", lapply(results, `[[`, "importance")) / length(results)
  importance <- as.data.frame(importance)
  importance$Taxon <- rownames(importance)
  importance[order(-importance$MeanDecreaseGini), ]
}
