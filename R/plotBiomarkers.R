#' Horizontal Bar Chart of Biomarker Effect Sizes
#'
#' Visualises biomarker taxa with bars coloured by the enriched group,
#' similar to a LEfSe-style plot.
#'
#' @param biomarker_result List returned by \code{\link{discoverBiomarkers}}.
#' @param top_n Integer. Maximum number of biomarkers to display.
#'   Default: \code{20}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' bm <- discoverBiomarkers(counts, meta, "Group")
#' plotBiomarkers(bm)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal coord_flip
#'   scale_fill_manual theme element_text theme_void
#' @importFrom utils head
#' @export
plotBiomarkers <- function(biomarker_result, top_n = 20) {
  if (!is.list(biomarker_result) || !"biomarkers" %in% names(biomarker_result)) {
    stop("biomarker_result must be the list returned by discoverBiomarkers().")
  }
  bm <- biomarker_result$biomarkers
  if (nrow(bm) == 0) {
    message("No significant biomarkers to plot.")
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No significant biomarkers found"))
  }

  if ("effect_size" %in% colnames(bm)) {
    bm <- bm[order(-bm$effect_size), ]
    score_col <- "effect_size"
  } else {
    bm <- bm[order(-bm$rf_importance), ]
    score_col <- "rf_importance"
  }

  bm <- head(bm, top_n)
  bm$taxon <- factor(bm$taxon, levels = rev(bm$taxon))

  color_col <- if ("enriched_group" %in% colnames(bm)) "enriched_group" else NULL

  p <- ggplot2::ggplot(bm, ggplot2::aes(x = .data[["taxon"]],
                                          y = .data[[score_col]]))
  if (!is.null(color_col)) {
    p <- p + ggplot2::geom_bar(stat = "identity", alpha = 0.85,
                                ggplot2::aes(fill = .data[[color_col]]))
  } else {
    p <- p + ggplot2::geom_bar(stat = "identity", fill = "#3182bd", alpha = 0.85)
  }
  p + ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Effect Size", fill = "Enriched In",
                  title = "Biomarker Taxa") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
}
