#' Random Forest Classification of Microbiome Samples
#'
#' Trains a random forest model to classify samples by a metadata variable,
#' evaluates on a held-out test set, and returns feature importance scores
#' for biomarker discovery.
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param sample_info A data.frame with sample metadata. Row names must match
#'   \code{data}.
#' @param group_var Character string naming the response column in
#'   \code{sample_info}.
#' @param ntree Integer. Number of trees. Default: \code{500}.
#' @param mtry Integer or \code{NULL}. Variables per split; \code{NULL} uses
#'   \code{floor(sqrt(ncol(data)))}. Default: \code{NULL}.
#' @param test_fraction Numeric in (0, 1). Fraction held out for testing.
#'   Default: \code{0.3}.
#' @param seed Integer. Random seed. Default: \code{42}.
#' @return A list with:
#'   \describe{
#'     \item{model}{The \code{randomForest} object.}
#'     \item{confusion_matrix}{Table of predicted vs. actual on test set.}
#'     \item{accuracy}{Overall test-set accuracy.}
#'     \item{importance}{data.frame of variable importance, sorted descending.}
#'     \item{oob_error}{Out-of-bag error rate.}
#'     \item{predictions}{Factor of test-set predictions.}
#'     \item{actual}{Factor of test-set true labels.}
#'     \item{train_indices}{Integer vector of training row indices.}
#'     \item{test_indices}{Integer vector of test row indices.}
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' counts <- matrix(rpois(200, 20), nrow = 20, ncol = 10)
#' rownames(counts) <- paste0("S", 1:20)
#' meta <- data.frame(Group = rep(c("A", "B"), 10), row.names = paste0("S", 1:20))
#' rf <- performRandomForest(counts, meta, "Group")
#' rf$accuracy
#' }
#'
#' @importFrom stats predict
#' @export
performRandomForest <- function(data, sample_info, group_var,
                                 ntree = 500, mtry = NULL,
                                 test_fraction = 0.3, seed = 42) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required. Install with: install.packages('randomForest')")
  }
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  if (!group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info."))
  }

  common <- intersect(rownames(data), rownames(sample_info))
  if (length(common) < 6) {
    stop("At least 6 samples with matching IDs are needed.")
  }
  data <- data[common, , drop = FALSE]
  labels <- factor(sample_info[common, group_var])

  if (nlevels(labels) < 2) {
    stop("group_var must have at least 2 levels.")
  }

  set.seed(seed)
  n <- length(common)
  test_n <- max(2, round(n * test_fraction))
  test_idx <- sort(sample.int(n, test_n))
  train_idx <- setdiff(seq_len(n), test_idx)

  if (is.null(mtry)) mtry <- floor(sqrt(ncol(data)))

  model <- randomForest::randomForest(
    x = data[train_idx, , drop = FALSE],
    y = labels[train_idx],
    ntree = ntree, mtry = mtry, importance = TRUE
  )

  preds <- stats::predict(model, newdata = data[test_idx, , drop = FALSE])
  cm <- table(Predicted = preds, Actual = labels[test_idx])
  acc <- sum(diag(cm)) / sum(cm)

  imp <- as.data.frame(randomForest::importance(model))
  imp$Taxon <- rownames(imp)
  imp <- imp[order(-imp$MeanDecreaseGini), ]

  list(
    model = model,
    confusion_matrix = cm,
    accuracy = acc,
    importance = imp,
    oob_error = model$err.rate[ntree, "OOB"],
    predictions = preds,
    actual = labels[test_idx],
    train_indices = train_idx,
    test_indices = test_idx
  )
}
