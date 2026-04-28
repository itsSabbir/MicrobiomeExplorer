#' Volcano Plot for Differential Abundance Results
#'
#' Creates a volcano plot (log2 fold-change vs. -log10 p-value) from
#' differential abundance analysis results.
#'
#' @param de_results A data.frame with differential expression results.
#' @param fc_col Character string naming the fold-change column.
#'   Default: \code{"log2FoldChange"} (DESeq2 output). For edgeR use
#'   \code{"logFC"}.
#' @param pval_col Character string naming the p-value column.
#'   Default: \code{"pvalue"} (DESeq2). For edgeR use \code{"PValue"}.
#' @param fc_threshold Numeric. Absolute fold-change threshold for colour
#'   coding. Default: \code{1}.
#' @param pval_threshold Numeric. P-value threshold for colour coding.
#'   Default: \code{0.05}.
#' @param label_col Optional character string naming a column with point
#'   labels. If \code{NULL}, row names are used. Default: \code{NULL}.
#' @param point_size Numeric. Size of plotted points. Default: \code{2}.
#' @param colors Named character vector of three colours for "Up", "Down", and
#'   "NS" significance categories. Default: \code{c(Up = "#d73027", Down =
#'   "#4575b4", NS = "grey60")}.
#' @param pval_floor Numeric. Minimum p-value floor to avoid \code{log(0)}.
#'   Default: \code{1e-300}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' set.seed(42)
#' res <- data.frame(
#'   log2FoldChange = rnorm(50),
#'   pvalue = runif(50),
#'   taxon = paste0("OTU", 1:50)
#' )
#' p <- plotVolcano(res, label_col = "taxon")
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline
#'   scale_colour_manual labs theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @export
plotVolcano <- function(de_results, fc_col = "log2FoldChange",
                         pval_col = "pvalue", fc_threshold = 1,
                         pval_threshold = 0.05, label_col = NULL,
                         point_size = 2,
                         colors = c(Up = "#d73027", Down = "#4575b4", NS = "grey60"),
                         pval_floor = 1e-300) {
  if (!is.data.frame(de_results)) {
    de_results <- as.data.frame(de_results)
  }
  if (!fc_col %in% colnames(de_results)) {
    stop(paste("Column", fc_col, "not found in de_results."))
  }
  if (!pval_col %in% colnames(de_results)) {
    stop(paste("Column", pval_col, "not found in de_results."))
  }

  df <- de_results
  df$fc_val  <- df[[fc_col]]
  df$pval    <- df[[pval_col]]
  df$neg_log_p <- -log10(pmax(df$pval, pval_floor))

  df$significance <- "NS"
  df$significance[df$fc_val >  fc_threshold & df$pval < pval_threshold] <- "Up"
  df$significance[df$fc_val < -fc_threshold & df$pval < pval_threshold] <- "Down"
  df$significance <- factor(df$significance, levels = c("Up", "Down", "NS"))

  # Labels for significant points only
  if (is.null(label_col)) {
    df$label <- ifelse(df$significance != "NS", rownames(df), "")
  } else {
    df$label <- ifelse(df$significance != "NS", as.character(df[[label_col]]), "")
  }

  sig_colors <- colors

  ggplot2::ggplot(df, ggplot2::aes(x = .data[["fc_val"]], y = .data[["neg_log_p"]],
                                    colour = .data[["significance"]])) +
    ggplot2::geom_point(size = point_size, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = c(-fc_threshold, fc_threshold),
                        linetype = "dashed", colour = "grey40") +
    ggplot2::geom_hline(yintercept = -log10(pval_threshold),
                        linetype = "dashed", colour = "grey40") +
    ggplot2::scale_colour_manual(values = sig_colors) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data[["label"]]),
                              size = 3, max.overlaps = 20,
                              show.legend = FALSE) +
    ggplot2::labs(x = paste0("Log2 Fold Change (", fc_col, ")"),
                  y = paste0("-Log10 P-value (", pval_col, ")"),
                  colour = "Significance",
                  title = "Volcano Plot") +
    ggplot2::theme_minimal()
}
