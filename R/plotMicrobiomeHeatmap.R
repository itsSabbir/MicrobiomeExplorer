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
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom grDevices heat.colors
#' @import RColorBrewer
#' @examples
#' # Create a simple numeric dataset for the example
#' example_data <- matrix(rnorm(100), nrow = 10)
#' colnames(example_data) <- paste0("Taxa_", 1:10)
#' rownames(example_data) <- paste0("Sample_", 1:10)
#'
#' # Plot the heatmap
#' heatmap_plot <- plot_microbiome_heatmap(example_data,
#' normalize = TRUE, color_palette = RColorBrewer::brewer.pal(9, "Blues"))
#' print(heatmap_plot)

#' @references
#' Gu, Z. (2016). Complex heatmaps reveal patterns and correlations in multidimensional genomic data. Bioinformatics, 32(18), 2847–2849.
plot_microbiome_heatmap <- function(data, normalize = FALSE,
                                    cluster_rows = TRUE,
                                    cluster_cols = TRUE,
                                    color_palette = heat.colors(10)) {
  # Validate data input
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0 || nrow(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }

  # Convert data frame to matrix, excluding non-numeric columns
  if (is.data.frame(data)) {
    numeric_columns <- sapply(data, is.numeric)
    if (all(!numeric_columns)) {
      stop("Data does not contain any numeric columns.")
    }
    data <- as.matrix(data[, numeric_columns])
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


