# 'advancedRarefactionPlot.R'

#' Advanced Rarefaction Plot for Microbiome Data
#'
#' Generates rarefaction curves using alpha diversity indices from microbiome data.
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param indices A character vector specifying the diversity indices to use for plotting.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param col Colors for the curves, recycled if necessary.
#' @param lty Line types for the curves, recycled if necessary.
#' @param save_plot Logical, if TRUE, the plot is saved to the specified file.
#' @param file_name Name of the file to save the plot.
#' @param file_type Type of the file to save the plot (e.g., "png", "pdf").
#' @param ... Any other additional arguments to pass to insider functions
#' @return A plot object showing rarefaction curves for each sample.
#' @export
#'
#' @examples
#' data(microbiome_example) # Load the example dataset
#' AdvancedRarefactionPlot(microbiome_example, indices = c("Shannon", "Simpson"))
#'
#' @references
#' Gotelli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in the measurement and comparison of species richness. Ecology Letters, 4(4), 379-391.
#' @importFrom ggplot2 ggplot aes_string geom_line scale_color_manual labs theme_minimal
#' @importFrom vegan rarefy
AdvancedRarefactionPlot <- function(data, indices = c("Shannon", "Simpson", "Chao1", "ACE"), xlab = "Sample Size", ylab = "Diversity Index", col = NULL, lty = 1, save_plot = FALSE, file_name = "rarefaction_plot", file_type = "pdf", ...) {
  # Validate input data
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  # Calculate alpha diversity using the calculate_alpha_diversity function
  diversity_data <- calculate_alpha_diversity(data, indices = indices)

  # Prepare plot data
  plot_data <- reshape2::melt(diversity_data, id.vars = "Sample")

  # Set dynamic colors if not specified
  if (is.null(col)) {
    num_samples <- length(unique(plot_data$Sample))
    col <- grDevices::rainbow(num_samples)
  }

  # Prepare the plot
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "variable", y = "value", group = "Sample", color = "Sample", linetype = "Sample")) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::labs(x = xlab, y = ylab, color = "Sample") +
    ggplot2::theme_minimal()

  # Display plot
  print(plot)

  # Save the plot if requested
  if (save_plot) {
    file_name_full <- paste0(file_name, ".", file_type)
    ggplot2::ggsave(file_name_full, plot, ...)
  }

  return(invisible(plot))
}
