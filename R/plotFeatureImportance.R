#' Bar Chart of Random Forest Feature Importance
#'
#' Plots the top N most important features from a random forest result.
#'
#' @param rf_result List returned by \code{\link{performRandomForest}}.
#' @param top_n Integer. Number of top features to show. Default: \code{20}.
#' @param measure Character string. Importance column to plot:
#'   \code{"MeanDecreaseGini"} or \code{"MeanDecreaseAccuracy"}.
#'   Default: \code{"MeanDecreaseGini"}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' rf <- performRandomForest(counts, meta, "Group")
#' plotFeatureImportance(rf, top_n = 15)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme
#'   element_text coord_flip
#' @importFrom utils head
#' @export
plotFeatureImportance <- function(rf_result, top_n = 20,
                                   measure = "MeanDecreaseGini") {
  if (!is.list(rf_result) || !"importance" %in% names(rf_result)) {
    stop("rf_result must be the list returned by performRandomForest().")
  }
  imp <- rf_result$importance
  if (!measure %in% colnames(imp)) {
    stop(paste("measure", measure, "not found. Available:",
               paste(setdiff(colnames(imp), "Taxon"), collapse = ", ")))
  }

  imp <- imp[order(-imp[[measure]]), ]
  imp <- head(imp, top_n)
  imp$Taxon <- factor(imp$Taxon, levels = rev(imp$Taxon))

  ggplot2::ggplot(imp, ggplot2::aes(x = .data[["Taxon"]],
                                     y = .data[[measure]])) +
    ggplot2::geom_bar(stat = "identity", fill = "#3182bd", alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = measure,
                  title = paste("Top", top_n, "Features by", measure)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
}
