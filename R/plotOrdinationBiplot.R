#' Plot Ordination Results with Sample Metadata Colouring
#'
#' Creates a 2D scatter plot from \code{performOrdination()} output, with
#' samples coloured and/or shaped by metadata variables.
#'
#' @param ordination_result A list as returned by \code{performOrdination()}.
#' @param sample_info Optional data.frame with sample metadata. Row names must
#'   match row names of \code{ordination_result$coordinates}.
#'   Default: \code{NULL}.
#' @param color_var Character string naming the column in \code{sample_info}
#'   to use for point colour. Default: \code{NULL}.
#' @param shape_var Character string naming the column in \code{sample_info}
#'   to use for point shape. Default: \code{NULL}.
#' @param label_samples Logical. If \code{TRUE}, sample names are added with
#'   \code{ggrepel::geom_text_repel}. Default: \code{FALSE}.
#' @param ellipse Logical. If \code{TRUE}, 95\% confidence ellipses are drawn
#'   per group (requires \code{color_var}). Default: \code{FALSE}.
#' @param point_size Numeric. Point size. Default: \code{3}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Sample", 1:10)
#' colnames(counts) <- paste0("OTU", 1:6)
#' ord <- performOrdination(counts, method = "PCoA")
#' meta <- data.frame(Group = rep(c("A", "B"), 5),
#'                    row.names = paste0("Sample", 1:10))
#' p <- plotOrdinationBiplot(ord, sample_info = meta, color_var = "Group",
#'                           ellipse = TRUE)
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal stat_ellipse
#' @importFrom ggrepel geom_text_repel
#' @export
plotOrdinationBiplot <- function(ordination_result, sample_info = NULL,
                                  color_var = NULL, shape_var = NULL,
                                  label_samples = FALSE, ellipse = FALSE,
                                  point_size = 3) {
  if (!is.list(ordination_result) || !"coordinates" %in% names(ordination_result)) {
    stop("ordination_result must be the list returned by performOrdination().")
  }

  coords <- ordination_result$coordinates
  coords$Sample <- rownames(coords)

  if (!is.null(sample_info)) {
    coords <- merge(coords,
                    cbind(Sample = rownames(sample_info), sample_info),
                    by = "Sample", all.x = TRUE)
  }

  # Build axis labels with variance explained if available
  var_exp <- ordination_result$variance_explained
  method  <- ordination_result$method
  make_axis_label <- function(i) {
    ax <- paste0(method, i)
    if (!is.null(var_exp) && length(var_exp) >= i) {
      ax <- paste0(ax, " (", round(var_exp[i] * 100, 1), "%)")
    }
    ax
  }
  xlab <- make_axis_label(1)
  ylab <- make_axis_label(2)

  # Base aesthetics
  base_aes <- if (!is.null(color_var) && !is.null(shape_var)) {
    ggplot2::aes(x = .data[["Axis1"]], y = .data[["Axis2"]],
                 colour = .data[[color_var]], shape = .data[[shape_var]])
  } else if (!is.null(color_var)) {
    ggplot2::aes(x = .data[["Axis1"]], y = .data[["Axis2"]],
                 colour = .data[[color_var]])
  } else {
    ggplot2::aes(x = .data[["Axis1"]], y = .data[["Axis2"]])
  }

  p <- ggplot2::ggplot(coords, base_aes) +
    ggplot2::geom_point(size = point_size, alpha = 0.8) +
    ggplot2::labs(x = xlab, y = ylab,
                  title = paste(method, "Ordination"),
                  colour = color_var, shape = shape_var) +
    ggplot2::theme_minimal()

  if (ellipse && !is.null(color_var)) {
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(group = .data[[color_var]], colour = .data[[color_var]]),
      level = 0.95, type = "t"
    )
  }

  if (label_samples) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = .data[["Sample"]]),
      size = 3, max.overlaps = 20, show.legend = FALSE
    )
  }

  p
}
