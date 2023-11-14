# Assuming vegan package is installed or install if not present
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
}

library(vegan)

#' Plot Rarefaction Curves for Microbiome Data
#'
#' This function generates rarefaction curves for microbiome count data, useful for comparing species richness among samples.
#'
#' @details
#' \itemize{
#'   \item \strong{Customizable Sampling}: Control over the number of samples and step size for curve generation.
#'   \item \strong{Plot Customization}: Allows customization of plot aesthetics such as color, line type, and legend.
#'   \item \strong{Export Option}: Option to save the plot to a file in various formats.
#' }
#'
#' @param data A matrix or dataframe with rows as samples and columns as taxa.
#' @param step The step size for rarefaction curve calculation.
#' @param sample The number of individuals to be sampled for each step (if not specified, it uses the minimum sample size).
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @param col A vector of colors for the curves, recycled if necessary.
#' @param lty The line type for the curves, recycled if necessary.
#' @param save_plot Logical, if TRUE, the plot is saved to the specified file.
#' @param file_name The name of the file to save the plot to.
#' @param file_type The type of the file to save the plot to (e.g., "png", "pdf").
#' @param ... Additional arguments to pass to the plot function.
#' @return A plot object showing rarefaction curves for each sample.
#' @export
#'
#' @examples
#' data(microbiome_example) # Example dataset
#' plot_rarefaction(microbiome_example, sample = 100, col = c("red", "blue", "green"), save_plot = TRUE, file_name = "rarefaction_curves", file_type = "pdf")
#'
#' @references
#' Gotelli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in the measurement and comparison of species richness. Ecology Letters, 4(4), 379-391.
plot_rarefaction <- function(data, step = 1, sample = NULL, xlab = "Sample Size", ylab = "Species Richness", col = rainbow(nrow(data)), lty = 1, save_plot = FALSE, file_name = "rarefaction_plot", file_type = "pdf", ...) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (is.null(sample)) {
    sample <- min(rowSums(data))
  }

  # Ensure that the data is in the correct format for the vegan package
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # Generate the rarefaction curves
  rarefaction_curves <- rarefy(data, sample)

  # Create the plot
  if (save_plot) {
    file_name_full <- paste0(file_name, ".", file_type)
    graphics::dev.new(file = file_name_full)
  }

  plot(rarefaction_curves, xlab = xlab, ylab = ylab, type = "l", col = col, lty = lty, ...)

  # Add a legend if there are multiple curves
  if (nrow(data) > 1) {
    legend("topright", legend = rownames(data), col = col, lty = lty, cex = 0.7)
  }

  if (save_plot) {
    dev.off()
  }

  invisible(rarefaction_curves)
}
