#' Plot Taxonomic Composition as Stacked Bar Chart
#'
#' Creates a stacked bar chart of relative taxonomic abundances, optionally
#' grouped by a sample metadata variable.
#'
#' @param data A matrix or data.frame with rows as samples and columns as taxa.
#' @param sample_info Optional data.frame with sample metadata. Row names must
#'   match row names of \code{data}. Default: \code{NULL}.
#' @param group_var Character string naming the column in \code{sample_info} to
#'   use for x-axis grouping. If \code{NULL}, each sample appears individually.
#'   Default: \code{NULL}.
#' @param top_n Integer. Number of most-abundant taxa to show individually;
#'   remaining taxa are collapsed into "Other". Default: \code{10}.
#' @param normalize Logical. If \code{TRUE}, convert counts to relative
#'   abundances (rows sum to 1) before plotting. Default: \code{TRUE}.
#' @param palette Character string naming an RColorBrewer qualitative palette
#'   or a character vector of hex colours. Default: \code{"Set3"}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(50, lambda = 30), nrow = 5, ncol = 10)
#' rownames(counts) <- paste0("Sample", 1:5)
#' colnames(counts) <- paste0("OTU", 1:10)
#' p <- plotTaxonomicAbundance(counts, top_n = 5)
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs
#'   theme_minimal theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
plotTaxonomicAbundance <- function(data, sample_info = NULL, group_var = NULL,
                                    top_n = 10, normalize = TRUE,
                                    palette = "Set3") {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame.")
  }
  if (!is.null(group_var) && is.null(sample_info)) {
    stop("sample_info must be provided when group_var is specified.")
  }
  if (!is.null(group_var) && !group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info columns."))
  }

  if (is.data.frame(data)) {
    data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
  }

  if (normalize) {
    rs <- rowSums(data)
    rs[rs == 0] <- 1
    data <- sweep(data, 1, rs, "/")
  }

  # Identify top_n taxa by mean abundance; lump rest into "Other"
  mean_abun <- colMeans(data)
  top_taxa <- names(sort(mean_abun, decreasing = TRUE))[seq_len(min(top_n, ncol(data)))]
  other_taxa <- setdiff(colnames(data), top_taxa)

  plot_data <- as.data.frame(data[, top_taxa, drop = FALSE])
  if (length(other_taxa) > 0) {
    plot_data$Other <- rowSums(data[, other_taxa, drop = FALSE])
  }
  plot_data$Sample <- rownames(data)

  # Add grouping variable
  x_var <- "Sample"
  if (!is.null(group_var)) {
    plot_data[[group_var]] <- sample_info[rownames(data), group_var]
    x_var <- group_var
  }

  long_data <- tidyr::pivot_longer(plot_data,
                                    cols = -c("Sample", if (!is.null(group_var)) group_var),
                                    names_to = "Taxon",
                                    values_to = "Abundance")
  long_data$Taxon <- factor(long_data$Taxon,
                             levels = c(top_taxa, if (length(other_taxa) > 0) "Other"))

  # Build color palette
  n_taxa <- nlevels(long_data$Taxon)
  if (length(palette) == 1 && palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_cols <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    colors <- RColorBrewer::brewer.pal(min(n_taxa, max_cols), palette)
    if (n_taxa > max_cols) {
      colors <- grDevices::colorRampPalette(colors)(n_taxa)
    }
  } else {
    colors <- palette
  }

  ggplot2::ggplot(long_data,
                  ggplot2::aes(x = .data[[x_var]], y = .data[["Abundance"]],
                               fill = .data[["Taxon"]])) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = x_var, y = if (normalize) "Relative Abundance" else "Abundance",
                  fill = "Taxon") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
