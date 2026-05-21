#' Plot Shared and Unique Taxa Across Groups
#'
#' Creates an UpSet-style summary of taxa that are unique to one group or
#' shared across multiple groups. A taxon is present in a group when it has
#' non-zero abundance in at least \code{min_prevalence} of that group's
#' samples.
#'
#' @param data A numeric matrix or data.frame with rows as samples and columns
#'   as taxa.
#' @param sample_info A data.frame containing sample metadata. Row names, or a
#'   column named \code{"Sample"}, must match row names of \code{data}.
#' @param group_var Character string naming the grouping column in
#'   \code{sample_info}.
#' @param min_prevalence Numeric in \code{[0, 1]}. Minimum within-group sample
#'   prevalence required for a taxon to be counted as present in that group.
#'   Default: \code{0.1}.
#' @return A \code{ggplot} object showing taxon counts for each group
#'   intersection.
#'
#' @examples
#' counts <- matrix(c(1, 1, 0, 1, 0, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
#' rownames(counts) <- paste0("Sample_", 1:3)
#' colnames(counts) <- paste0("Taxon_", 1:3)
#' sample_info <- data.frame(Group = c("A", "A", "B"),
#'                           row.names = rownames(counts))
#' plotSharedTaxa(counts, sample_info, "Group", min_prevalence = 0.5)
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme element_text
#' @importFrom rlang .data
#' @export
plotSharedTaxa <- function(data, sample_info, group_var, min_prevalence = 0.1) {
  counts <- .plot_taxa_counts(data)
  .validate_shared_min_prevalence(min_prevalence)
  sample_info <- .shared_taxa_sample_info(sample_info, rownames(counts), group_var)
  membership <- .shared_taxa_membership(counts, sample_info[[group_var]],
                                        min_prevalence)
  plot_data <- .shared_taxa_summary(membership)
  message("[shared] plotting ", nrow(plot_data), " group intersections")

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = .data[["GroupSet"]],
                               y = .data[["TaxaCount"]],
                               fill = .data[["Class"]])) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::labs(x = "Group Set", y = "Number of Taxa",
                  fill = "Intersection",
                  title = "Shared and Unique Taxa") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "none")
}

.validate_shared_min_prevalence <- function(min_prevalence) {
  if (!is.numeric(min_prevalence) || length(min_prevalence) != 1 ||
      is.na(min_prevalence) || min_prevalence < 0 || min_prevalence > 1) {
    stop("min_prevalence must be a single numeric value in [0, 1].")
  }

  invisible(NULL)
}

.shared_taxa_sample_info <- function(sample_info, sample_ids, group_var) {
  if (!is.data.frame(sample_info)) {
    stop("sample_info must be a data.frame.")
  }
  if (!is.character(group_var) || length(group_var) != 1 ||
      !group_var %in% colnames(sample_info)) {
    stop("group_var must name a column in sample_info.")
  }
  if (!all(sample_ids %in% rownames(sample_info)) &&
      "Sample" %in% colnames(sample_info)) {
    rownames(sample_info) <- as.character(sample_info$Sample)
  }
  if (!all(sample_ids %in% rownames(sample_info))) {
    stop("sample_info row names or Sample column must match data row names.")
  }

  sample_info <- sample_info[sample_ids, , drop = FALSE]
  if (anyNA(sample_info[[group_var]])) {
    stop("group_var contains missing values for matched samples.")
  }
  if (length(unique(sample_info[[group_var]])) < 2) {
    stop("plotSharedTaxa requires at least two groups.")
  }

  sample_info
}

.shared_taxa_membership <- function(counts, group_values, min_prevalence) {
  group_values <- as.character(group_values)
  groups <- sort(unique(group_values))
  membership <- vapply(groups, function(group) {
    prevalence <- colMeans(counts[group_values == group, , drop = FALSE] > 0)
    prevalence > 0 & prevalence >= min_prevalence
  }, logical(ncol(counts)))
  rownames(membership) <- colnames(counts)

  membership
}

.shared_taxa_summary <- function(membership) {
  observed <- rowSums(membership) > 0
  if (!any(observed)) {
    return(data.frame(GroupSet = factor(character()),
                      SharedGroups = integer(),
                      TaxaCount = integer(),
                      Class = character()))
  }

  observed_membership <- membership[observed, , drop = FALSE]
  taxon_sets <- data.frame(
    GroupSet = apply(observed_membership, 1, function(row) {
      paste(colnames(observed_membership)[row], collapse = " & ")
    }),
    SharedGroups = rowSums(observed_membership),
    stringsAsFactors = FALSE
  )
  summary <- stats::aggregate(SharedGroups ~ GroupSet, taxon_sets, length)
  names(summary)[names(summary) == "SharedGroups"] <- "TaxaCount"
  summary$SharedGroups <- taxon_sets$SharedGroups[match(summary$GroupSet,
                                                        taxon_sets$GroupSet)]
  summary$Class <- ifelse(summary$SharedGroups > 1, "Shared", "Unique")
  summary <- summary[order(-summary$TaxaCount, summary$GroupSet), ]
  summary$GroupSet <- factor(summary$GroupSet, levels = summary$GroupSet)
  rownames(summary) <- NULL

  summary[, c("GroupSet", "SharedGroups", "TaxaCount", "Class")]
}
