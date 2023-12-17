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
# Example usage of the AdvancedRarefactionPlot function with the created dataset
#' @examples
#' data(microbiome_example) # Load the example dataset
#' AdvancedRarefactionPlot(microbiome_example, sample = 100,
#' col = c("red", "blue", "green"), save_plot = TRUE,
#' file_name = "advanced_rarefaction_curves", file_type = "pdf")
#'
#' @references
#' Gotelli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in the measurement and comparison of species richness. Ecology Letters, 4(4), 379-391.
#' @importFrom vegan rarefy
AdvancedRarefactionPlot <- function(data, step = 1, sample = NULL, xlab = "Sample Size", ylab = "Species Richness", col = NULL, lty = 1, statistical_analysis = FALSE, save_plot = FALSE, file_name = "rarefaction_plot", file_type = "pdf", ...) {
  # Validate input data
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (!all(sapply(data, is.numeric))) {
    stop("All columns in the data must be numeric.")
  }

  if (any(is.na(data))) {
    stop("Data contains NA values. Please remove or impute them before using this function.")
  }

  # Set default color if not specified
  if (is.null(col)) {
    col <- grDevices::rainbow(nrow(data))
  }

  # Set sample size to minimum sample size if not specified
  if (is.null(sample)) {
    sample <- min(rowSums(data))
  }

  # Convert dataframe to matrix for vegan package
  data_matrix <- as.matrix(data)

  # Generate rarefaction curves
  rarefaction_curves <- vegan::rarefy(data_matrix, sample)

  # Create a data frame for ggplot
  rarefaction_df <- as.data.frame(rarefaction_curves)
  rarefaction_df$Sample <- rownames(rarefaction_df)

  # Prepare the plot
  plot <- ggplot2::ggplot(rarefaction_df, ggplot2::aes_string(x = "Sample", y = "Species", group = "Sample", color = "Sample")) +
    ggplot2::geom_line(ggplot2::aes_string(linetype = as.character(lty)), ...) +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal()

  # Display plot
  print(plot)

  # Save the plot if requested
  if (save_plot) {
    file_name_full <- paste0(file_name, ".", file_type)
    ggplot2::ggsave(file_name_full, plot)
  }

  # Optional statistical analysis
  if (statistical_analysis) {
    # Compute basic statistics for each sample
    stats <- rarefaction_df %>%
      dplyr::group_by(Sample) %>%
      dplyr::summarise(
        Mean = mean(Species, na.rm = TRUE),
        Median = median(Species, na.rm = TRUE),
        SD = sd(Species, na.rm = TRUE)
      )

    # Simple ANOVA test if there are multiple samples
    if (nrow(rarefaction_df) > 1) {
      aov_results <- stats::aov(Species ~ Sample, data = rarefaction_df)
      anova_summary <- summary(aov_results)
    } else {
      anova_summary <- NULL
    }

    # Combine and return the results
    list(
      basic_stats = stats,
      anova_results = anova_summary
    )
  }

  return(invisible(plot))
}

