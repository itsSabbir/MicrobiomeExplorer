# Interpretation: shared taxa are shown as UpSet-style intersection bars because ggplot2 has no native Venn geometry.

#' Plot Rank-Abundance Curve
#'
#' Creates a Whittaker-style rank-abundance plot from microbiome abundance data.
#' Taxa are ranked by total abundance across samples and plotted against their
#' relative abundance.
#'
#' @param data A numeric matrix or data.frame with rows as samples and columns
#'   as taxa.
#' @param top_n Optional positive integer. Maximum number of ranked taxa to
#'   display. If \code{NULL}, all observed taxa are shown. Default:
#'   \code{NULL}.
#' @param log_scale Logical. If \code{TRUE}, use a log10 y-axis for relative
#'   abundance. Default: \code{FALSE}.
#' @return A \code{ggplot} object showing species rank versus relative
#'   abundance.
#'
#' @examples
#' counts <- matrix(c(10, 3, 0, 30, 5, 2), nrow = 2, byrow = TRUE)
#' rownames(counts) <- c("Sample_A", "Sample_B")
#' colnames(counts) <- c("Taxon_A", "Taxon_B", "Taxon_C")
#' plotRankAbundance(counts, top_n = 3, log_scale = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_y_log10
#'   theme_minimal
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
plotRankAbundance <- function(data, top_n = NULL, log_scale = FALSE) {
  counts <- .plot_taxa_counts(data)
  .validate_rank_log_scale(log_scale)
  plot_data <- .rank_abundance_data(counts, top_n)
  y_label <- if (log_scale) "Relative Abundance (log10 scale)" else "Relative Abundance"
  message("[rank] plotting ", nrow(plot_data), " taxa")

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[["Rank"]], y = .data[["RelativeAbundance"]])
  ) +
    ggplot2::geom_line(colour = "#3182bd", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#08519c", size = 2) +
    ggplot2::labs(x = "Species Rank", y = y_label,
                  title = "Rank-Abundance Curve") +
    ggplot2::theme_minimal()

  if (log_scale) {
    message("[rank] applying log10 abundance scale")
    plot <- plot + ggplot2::scale_y_log10()
  }

  plot
}

.plot_taxa_counts <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame.")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }
  if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1)))) {
    stop("Data must contain only numeric taxa abundances.")
  }

  counts <- as.matrix(data)
  if (!is.numeric(counts)) {
    stop("Data must contain only numeric taxa abundances.")
  }
  if (anyNA(counts) || any(counts < 0, na.rm = TRUE)) {
    stop("Data must contain non-missing, non-negative taxa abundances.")
  }
  if (sum(counts) <= 0) {
    stop("Data must contain at least one positive taxa abundance.")
  }
  if (is.null(rownames(counts))) {
    rownames(counts) <- paste0("Sample_", seq_len(nrow(counts)))
  }
  if (is.null(colnames(counts))) {
    colnames(counts) <- paste0("Taxon_", seq_len(ncol(counts)))
  }

  counts
}

.rank_abundance_data <- function(counts, top_n) {
  taxon_abundance <- colSums(counts)
  taxon_abundance <- taxon_abundance[taxon_abundance > 0]
  if (length(taxon_abundance) == 0) {
    stop("Data must contain at least one observed taxon.")
  }

  relative_abundance <- taxon_abundance / sum(taxon_abundance)
  rank_data <- data.frame(
    Taxon = names(relative_abundance),
    RelativeAbundance = unname(relative_abundance),
    stringsAsFactors = FALSE
  )
  rank_data <- rank_data[order(-rank_data$RelativeAbundance, rank_data$Taxon), ]
  rank_data <- utils::head(rank_data, .validate_rank_top_n(top_n, nrow(rank_data)))
  rank_data$Rank <- seq_len(nrow(rank_data))
  rank_data$Taxon <- factor(rank_data$Taxon, levels = rank_data$Taxon)
  rownames(rank_data) <- NULL

  rank_data[, c("Rank", "Taxon", "RelativeAbundance")]
}

.validate_rank_top_n <- function(top_n, taxa_count) {
  if (is.null(top_n)) {
    return(taxa_count)
  }
  if (!is.numeric(top_n) || length(top_n) != 1 ||
      is.na(top_n) || top_n < 1 || top_n != floor(top_n)) {
    stop("top_n must be a positive integer.")
  }

  min(as.integer(top_n), taxa_count)
}

.validate_rank_log_scale <- function(log_scale) {
  if (!is.logical(log_scale) || length(log_scale) != 1 || is.na(log_scale)) {
    stop("log_scale must be TRUE or FALSE.")
  }

  invisible(NULL)
}
