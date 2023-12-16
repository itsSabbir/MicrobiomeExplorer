#' Advanced Heatmap for Microbiome Data Visualization
#'
#' This function creates a comprehensive heatmap for microbiome data analysis.
#' It supports normalization, various clustering methods, and enhanced visualization options.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param normalize Logical TRUE or FALSE indicating if the data should be normalized.
#'                  Default: FALSE (data will not be normalized).
#' @param cluster_rows Should rows be clustered? Default is TRUE.
#' @param cluster_cols Should columns be clustered? Default is TRUE.
#' @param color_palette Color palette to use for heatmap. Default is heat.colors(10).
#' @return A ComplexHeatmap plot object.
#' @export
#' @examples
#' data(microbiome_example) # Example dataset
#' heatmap_plot <- plot_microbiome_heatmap(microbiome_example, normalize = TRUE, color_palette = RColorBrewer::brewer.pal(9, "Blues"))
#' print(heatmap_plot)
#' @references
#' Gu, Z. (2016). Complex heatmaps reveal patterns and correlations in multidimensional genomic data. Bioinformatics, 32(18), 2847–2849.
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

  # Check if data is a data frame and convert it to a matrix
  if (is.data.frame(data)) {
    if (any(!sapply(data, is.numeric))) {
      stop("Data contains non-numeric columns which cannot be processed for heatmap.")
    }
    data <- as.matrix(data)
  }

  # Normalize data if requested
  if (normalize) {
    data <- t(apply(data, 1, function(x) {
      if (sum(x) == 0) {
        stop("Invalid argument: sum of row values cannot be zero for normalization.")
      }
      x / sum(x)
    }))
  }

  # Load required libraries
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("ComplexHeatmap library is not installed. Please install it using BiocManager::install('ComplexHeatmap').")
  }

  # Load required color palettes
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("RColorBrewer library is not installed. Please install it to use custom color palettes.")
  }

  # Creating the heatmap
  heatmap_plot <- ComplexHeatmap::Heatmap(data,
                                          name = "Abundance",
                                          col = color_palette,
                                          cluster_rows = cluster_rows,
                                          cluster_columns = cluster_cols,
                                          show_row_names = TRUE,
                                          show_column_names = TRUE)

  return(heatmap_plot)
}
