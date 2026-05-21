#' ROC Curve Plot from Cross-Validation Results
#'
#' Plots the Receiver Operating Characteristic curve with AUC annotation
#' from \code{\link{performClassification}} output.
#'
#' @param classification_result List returned by
#'   \code{\link{performClassification}}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' cv <- performClassification(counts, meta, "Group")
#' plotROCCurve(cv)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_abline annotate labs
#'   theme_minimal
#' @export
plotROCCurve <- function(classification_result) {
  if (!is.list(classification_result) ||
      !"roc_data" %in% names(classification_result)) {
    stop("classification_result must be the list returned by performClassification().")
  }
  roc_df <- classification_result$roc_data
  if (is.null(roc_df) || nrow(roc_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "ROC data not available (multi-class or pROC missing)"))
  }

  auc_val <- classification_result$auc
  auc_label <- if (!is.na(auc_val)) {
    paste0("AUC = ", round(auc_val, 3))
  } else {
    "AUC: N/A"
  }

  ggplot2::ggplot(roc_df, ggplot2::aes(x = .data[["FPR"]], y = .data[["TPR"]])) +
    ggplot2::geom_line(colour = "#d73027", linewidth = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                          colour = "grey50") +
    ggplot2::annotate("text", x = 0.6, y = 0.2, label = auc_label, size = 5) +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate",
                  title = "ROC Curve") +
    ggplot2::theme_minimal()
}
