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
#' \dontrun{
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

  common <- intersect(rownames(data), rownames(sample_info))
  if (length(common) < 6) stop("At least 6 matching samples are needed.")
  data <- data[common, , drop = FALSE]
  labels <- factor(sample_info[common, group_var])
  n <- length(common)
  n_classes <- nlevels(labels)

  if (n_classes < 2) stop("group_var must have at least 2 levels.")
  is_binary <- n_classes == 2

  set.seed(seed)
  fold_accs <- numeric(0)
  all_preds <- character(0)
  all_actuals <- character(0)
  all_probs <- numeric(0)
  imp_accum <- NULL

  for (rep_i in seq_len(n_repeats)) {
    fold_ids <- sample(rep(seq_len(n_folds), length.out = n))

    for (fi in seq_len(n_folds)) {
      test_mask <- fold_ids == fi
      train_mask <- !test_mask
      if (sum(test_mask) == 0 || sum(train_mask) == 0) next

      model <- randomForest::randomForest(
        x = data[train_mask, , drop = FALSE],
        y = labels[train_mask],
        ntree = ntree, importance = TRUE
      )

      preds <- stats::predict(model, newdata = data[test_mask, , drop = FALSE])
      acc <- mean(preds == labels[test_mask])
      fold_accs <- c(fold_accs, acc)

      all_preds <- c(all_preds, as.character(preds))
      all_actuals <- c(all_actuals, as.character(labels[test_mask]))

      if (is_binary) {
        probs <- stats::predict(model, newdata = data[test_mask, , drop = FALSE],
                                 type = "prob")[, levels(labels)[2]]
        all_probs <- c(all_probs, probs)
      }

      fold_imp <- randomForest::importance(model)[, "MeanDecreaseGini", drop = FALSE]
      if (is.null(imp_accum)) {
        imp_accum <- fold_imp
      } else {
        imp_accum <- imp_accum + fold_imp
      }
    }
  }

  total_folds <- length(fold_accs)
  if (!is.null(imp_accum)) imp_accum <- imp_accum / total_folds

  cm <- table(Predicted = all_preds, Actual = all_actuals)

  auc_val <- NA_real_
  roc_df <- NULL
  if (is_binary && requireNamespace("pROC", quietly = TRUE) && length(all_probs) > 0) {
    roc_obj <- pROC::roc(all_actuals, all_probs, levels = levels(labels), quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities
    )
  }

  imp_df <- NULL
  if (!is.null(imp_accum)) {
    imp_df <- as.data.frame(imp_accum)
    imp_df$Taxon <- rownames(imp_df)
    imp_df <- imp_df[order(-imp_df$MeanDecreaseGini), ]
  }

  list(
    cv_accuracy = mean(fold_accs),
    cv_accuracy_sd = stats::sd(fold_accs),
    auc = auc_val,
    roc_data = roc_df,
    confusion_matrix = cm,
    feature_importance = imp_df
  )
}
