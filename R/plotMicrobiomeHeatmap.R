#' Advanced Heatmap for Microbiome Data Visualization
#'
#' This function creates a comprehensive heatmap for microbiome data analysis.
#' It supports normalization, various clustering methods, and enhanced visualization options.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param normalize Logical indicating if the data should be normalized. Default: FALSE.
#' @param cluster_rows Logical indicating if rows should be clustered. Default: TRUE.
#' @param cluster_cols Logical indicating if columns should be clustered. Default: TRUE.
#' @param color_palette A color palette function to use for heatmap colors. Default is NULL,
#'        which will cause the function to dynamically create a palette based on the data.
#' @return A ComplexHeatmap plot object.
#' @export
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom grDevices colorRampPalette
#' @importFrom stats quantile
#' @import RColorBrewer phyloseq
#'
#' @examples
#' example_data <- matrix(rnorm(100), nrow = 10)
#' colnames(example_data) <- paste0("Taxa_", 1:10)
#' rownames(example_data) <- paste0("Sample_", 1:10)
#' heatmap_plot <- plot_microbiome_heatmap(example_data, normalize = TRUE)
#' print(heatmap_plot)
#' @references
#' Gu, Z. (2016). Complex heatmaps reveal patterns and correlations in multidimensional genomic data. Bioinformatics, 32(18), 2847–2849.

plot_microbiome_heatmap <- function(data, normalize = FALSE,
                                    cluster_rows = TRUE, cluster_cols = TRUE,
                                    color_palette = NULL) {
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
    data <- as.matrix(data[, numeric_columns, drop = FALSE])
  }

  # Normalize data if requested
  if (normalize) {
    row_sums <- rowSums(data)
    if (any(row_sums == 0)) {
      stop("Invalid argument: sum of row values cannot be zero for normalization.")
    }
    data <- sweep(data, 1, row_sums, FUN = "/")
  }

  # Dynamically generate a color palette based on the unique data values
  if (is.null(color_palette)) {
    unique_data_values <- length(unique(data))
    color_count <- unique_data_values + 1  # Add one more color to avoid one-to-one mapping

    # Generate a color palette function using a color ramp
    color_palette <- colorRampPalette(c("blue", "white", "red"))(color_count)
  } else if (is.function(color_palette)) {
    # If a function is provided, use it to generate the palette
    color_palette <- color_palette(length(unique(data)) + 1)  # Adjust the number of colors
  } else if (is.character(color_palette)) {
    # If a character vector is provided, use it directly
    color_palette <- color_palette
  } else {
    stop("color_palette must be a function, character vector, or NULL.")
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



