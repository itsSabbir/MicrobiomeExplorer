# 'advancedRarefactionPlot.R'

.rarefaction_default_steps <- 50L

#' Advanced Rarefaction Plot for Microbiome Data
#'
#' Generates true subsampled rarefaction curves by estimating species richness
#' across sequencing depths for each sample.
#'
#' @param data A numeric matrix or dataframe with rows as samples and columns
#'   as taxa.
#' @param step Optional positive numeric spacing between sequencing depths.
#'   When supplied, this overrides \code{n_steps}.
#' @param n_steps Positive integer number of evenly-spaced sequencing depths.
#'   Default: \code{50}.
#' @param ... Unsupported legacy plotting arguments. A warning is issued when
#'   supplied.
#' @return A \code{ggplot} object showing expected species richness by
#'   sequencing depth for each sample.
#' @export
#'
#' @examples
#' set.seed(42)
#' sample_data <- matrix(rpois(45, lambda = 10), nrow = 9, ncol = 5)
#' rownames(sample_data) <- paste0("Sample_", 1:9)
#' colnames(sample_data) <- paste0("Taxa_", 1:5)
#' advancedRarefactionPlot(sample_data)
#'
#' @references
#' Gotelli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in the measurement and comparison of species richness. Ecology Letters, 4(4), 379-391.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme_minimal scale_x_continuous
#' @importFrom vegan rarefy
#' @importFrom rlang .data
advancedRarefactionPlot <- function(data, step = NULL,
                                    n_steps = .rarefaction_default_steps, ...) {
  legacy_args <- list(...)
  .warn_rarefaction_legacy_args(legacy_args)

  counts <- .validate_rarefaction_counts(data)
  alpha_results <- calculateAlphaDiversity(counts, indices = "Chao1")
  depths <- .rarefaction_depth_grid(max(rowSums(counts)), step, n_steps)
  plot_data <- .rarefaction_plot_data(counts, depths, alpha_results$Sample)
  min_library_size <- min(rowSums(counts))
  max_library_size <- max(rowSums(counts))
  message("[rarefy] computed curves for ", nrow(counts), " samples")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["Depth"]],
                                          y = .data[["ExpectedRichness"]],
                                          group = .data[["Sample"]],
                                          color = .data[["Sample"]])) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = min_library_size, linetype = "dashed") +
    ggplot2::scale_x_continuous(limits = c(1, max_library_size)) +
    ggplot2::labs(x = "Sequencing Depth",
                  y = "Expected Species Richness",
                  color = "Sample") +
    ggplot2::theme_minimal()
}

.warn_rarefaction_legacy_args <- function(legacy_args) {
  if (length(legacy_args) == 0) {
    return(invisible(NULL))
  }

  arg_names <- names(legacy_args)
  arg_names[arg_names == ""] <- "<unnamed>"
  warning("Ignoring unsupported argument(s): ", paste(arg_names, collapse = ", "),
          call. = FALSE)
}

.validate_rarefaction_counts <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }
  if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1)))) {
    stop("Data must contain only numeric taxa counts.")
  }

  counts <- as.matrix(data)
  if (!is.numeric(counts)) {
    stop("Data must contain only numeric taxa counts.")
  }
  if (any(counts < 0, na.rm = TRUE) || anyNA(counts)) {
    stop("Data must contain non-missing, non-negative taxa counts.")
  }
  if (is.null(rownames(counts))) {
    rownames(counts) <- paste0("Sample_", seq_len(nrow(counts)))
  }
  if (any(rowSums(counts) <= 0)) {
    stop("All samples must have positive sequencing depth.")
  }
  counts
}

.rarefaction_depth_grid <- function(max_depth, step, n_steps) {
  if (!is.null(step)) {
    if (!is.numeric(step) || length(step) != 1 || step <= 0) {
      stop("step must be a single positive number.")
    }
    message("[rarefy] using fixed sequencing-depth step")
    if (step > max_depth) {
      return(1)
    }
    return(unique(c(1, seq.int(step, max_depth, by = step))))
  }

  if (!is.numeric(n_steps) || length(n_steps) != 1 || n_steps <= 0) {
    stop("n_steps must be a single positive number.")
  }
  message("[rarefy] using evenly-spaced sequencing-depth grid")
  sort(unique(round(seq.int(1, max_depth, length.out = n_steps))))
}

.sample_rarefaction_curve <- function(sample_counts, depths) {
  sample_total <- sum(sample_counts)
  sample_depths <- sort(unique(c(depths[depths <= sample_total], sample_total)))
  expected_richness <- vapply(sample_depths, function(sample_size) {
    as.numeric(vegan::rarefy(sample_counts, sample_size))
  }, numeric(1))

  data.frame(Depth = sample_depths,
             ExpectedRichness = expected_richness)
}

.rarefaction_plot_data <- function(counts, depths, sample_ids) {
  sample_curves <- lapply(seq_len(nrow(counts)), function(sample_index) {
    curve <- .sample_rarefaction_curve(counts[sample_index, ], depths)
    curve$Sample <- sample_ids[sample_index]
    curve
  })

  do.call(rbind, sample_curves)
}
