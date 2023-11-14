#' Advanced Heatmap for Microbiome Data Visualization
#'
#' This function creates a comprehensive heatmap for microbiome data analysis.
#' It supports normalization, various clustering methods, and enhanced visualization options.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param normalize Should the data be normalized? Default is FALSE.
#' @param cluster_rows Should rows be clustered? Default is TRUE.
#' @param cluster_cols Should columns be clustered? Default is TRUE.
#' @param color_palette Color palette to use for heatmap.
#' @return A ComplexHeatmap plot.
#' @importFrom phyloseq plot_heatmap
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @examples
#' data(microbiome_example) # Example dataset
#' plot_microbiome_heatmap(microbiome_example, normalize = TRUE, color_palette = brewer.pal(9, "Blues"))
#' @references
#' Gu, Z. (2016). Complex heatmaps reveal patterns and correlations in multidimensional
#' genomic data. Bioinformatics, 32(18), 2847–2849.
plot_microbiome_heatmap <- function(data, normalize = FALSE,
                                    cluster_rows = TRUE,
                                    cluster_cols = TRUE,
                                    color_palette = heat.colors(10)) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  # Normalize data if requested
  if (normalize) {
    data <- t(apply(data, 1, function(x) x / sum(x)))
  }

  # Load ComplexHeatmap if not already loaded
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("ComplexHeatmap")
    library(ComplexHeatmap)
  }

  # Creating the heatmap
  Heatmap(data,
          name = "Abundance",
          col = color_palette,
          cluster_rows = cluster_rows,
          cluster_columns = cluster_cols,
          show_row_names = TRUE,
          show_column_names = TRUE)
}




