#' Boxplot or Violin Plot of Alpha Diversity by Group
#'
#' Visualises alpha diversity distributions across sample groups using box or
#' violin plots with optional statistical testing.
#'
#' @param alpha_results A data.frame from \code{calculate_alpha_diversity()}.
#'   Must have a \code{Sample} column and one column per diversity index.
#' @param sample_info A data.frame with sample metadata. Row names or a column
#'   named \code{"Sample"} must match \code{alpha_results$Sample}.
#' @param group_var Character string naming the grouping column in
#'   \code{sample_info}. Required.
#' @param index Character string naming the diversity index column to plot.
#'   Default: \code{"Shannon"}.
#' @param plot_type Character string: \code{"box"} or \code{"violin"}.
#'   Default: \code{"box"}.
#' @param add_points Logical. If \code{TRUE}, individual sample points are
#'   overlaid using \code{geom_jitter}. Default: \code{TRUE}.
#' @param test Character string: \code{"kruskal"} (Kruskal-Wallis) or
#'   \code{"wilcoxon"} (pairwise Wilcoxon). Default: \code{"kruskal"}.
#' @return A list with:
#'   \describe{
#'     \item{plot}{A \code{ggplot} object.}
#'     \item{test_result}{Result object from the chosen statistical test.}
#'   }
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
#' rownames(counts) <- paste0("S", 1:10)
#' colnames(counts) <- paste0("OTU", 1:5)
#' alpha <- calculate_alpha_diversity(counts, indices = c("Shannon", "Simpson"))
#' meta  <- data.frame(Group = rep(c("A", "B"), 5),
#'                     row.names = paste0("S", 1:10))
#' result <- plotAlphaDiversityBoxplot(alpha, sample_info = meta,
#'                                     group_var = "Group", index = "Shannon")
#' print(result$plot)
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter
#'   labs theme_minimal
#' @importFrom stats kruskal.test pairwise.wilcox.test
#' @export
plotAlphaDiversityBoxplot <- function(alpha_results, sample_info, group_var,
                                       index = "Shannon", plot_type = "box",
                                       add_points = TRUE, test = "kruskal") {
  if (!is.data.frame(alpha_results)) {
    stop("alpha_results must be a data.frame (output of calculate_alpha_diversity).")
  }
  if (!"Sample" %in% colnames(alpha_results)) {
    stop("alpha_results must have a 'Sample' column.")
  }
  if (!index %in% colnames(alpha_results)) {
    stop(paste("Index column", index, "not found in alpha_results."))
  }
  if (!is.data.frame(sample_info)) {
    stop("sample_info must be a data.frame.")
  }
  if (!group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info."))
  }
  if (!plot_type %in% c("box", "violin")) {
    stop("plot_type must be 'box' or 'violin'.")
  }

  # Merge alpha results with metadata
  merged <- merge(alpha_results,
                  cbind(Sample = rownames(sample_info), sample_info),
                  by = "Sample")

  # Statistical test
  formula_obj <- stats::as.formula(paste(index, "~", group_var))
  test_result <- if (test == "kruskal") {
    stats::kruskal.test(formula_obj, data = merged)
  } else {
    stats::pairwise.wilcox.test(merged[[index]],
                                 merged[[group_var]],
                                 p.adjust.method = "BH")
  }

  # Build plot
  p <- ggplot2::ggplot(merged,
                       ggplot2::aes(x = .data[[group_var]], y = .data[[index]],
                                    fill = .data[[group_var]]))
  if (plot_type == "violin") {
    p <- p + ggplot2::geom_violin(trim = FALSE, alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA)
  }
  if (add_points) {
    p <- p + ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.8)
  }
  p <- p +
    ggplot2::labs(x = group_var, y = index,
                  title = paste(index, "Diversity by", group_var)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  list(plot = p, test_result = test_result)
}
