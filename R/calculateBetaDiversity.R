#' Calculate Beta Diversity Distance Matrix
#'
#' Computes pairwise dissimilarity/distance matrices between samples using
#' standard ecological distance metrics.
#'
#' @param data A matrix or data.frame with rows as samples and columns as taxa,
#'   or a \code{MicrobiomeData} object containing \code{rRNA16S} data.
#'   Values must be non-negative counts or relative abundances.
#' @param method Character string specifying the distance metric. One of
#'   \code{"bray"} (Bray-Curtis), \code{"jaccard"} (Jaccard),
#'   \code{"euclidean"} (Euclidean), \code{"unifrac"} (unweighted UniFrac),
#'   or \code{"wunifrac"} (weighted UniFrac). Default: \code{"bray"}.
#' @param binary Logical. If \code{TRUE}, data are converted to
#'   presence/absence before computing distances. Default: \code{FALSE}.
#' @param ... Named options. For UniFrac methods, pass \code{tree} or
#'   \code{phylogenetic_tree} unless \code{data} is a \code{MicrobiomeData}
#'   object with a \code{PhylogeneticTree} slot.
#' @return A \code{dist} object containing the pairwise distance matrix.
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Sample", 1:10)
#' colnames(counts) <- paste0("OTU", 1:6)
#' dist_mat <- calculateBetaDiversity(counts, method = "bray")
#'
#' @references
#' Bray, J.R. and Curtis, J.T. (1957). An ordination of the upland forest
#' communities of southern Wisconsin. Ecological Monographs, 27(4), 325-349.
#'
#' @importFrom vegan vegdist
#' @importFrom phyloseq UniFrac otu_table phy_tree phyloseq
#' @export
calculateBetaDiversity <- function(data, method = "bray", binary = FALSE, ...) {
  tree <- get_beta_tree_option(list(...))
  valid_methods <- c("bray", "jaccard", "euclidean", "unifrac", "wunifrac")
  if (!method %in% valid_methods) {
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))
  }

  beta_input <- get_beta_input(data, tree)
  data <- as_beta_matrix(beta_input$data)
  tree <- beta_input$tree

  if (any(data < 0, na.rm = TRUE)) {
    stop("Data must contain only non-negative values.")
  }

  if (method %in% c("unifrac", "wunifrac")) {
    return(calculate_unifrac_distance(data, tree, method == "wunifrac"))
  }

  message("[beta] calculating ", method, " distance with vegan")
  vegan::vegdist(data, method = method, binary = binary)
}

get_beta_tree_option <- function(options) {
  if (length(options) == 0) {
    return(NULL)
  }
  option_names <- names(options)
  if (is.null(option_names) || any(option_names == "")) {
    stop("Beta diversity options must be named.")
  }
  unexpected_options <- setdiff(option_names, c("tree", "phylogenetic_tree"))
  if (length(unexpected_options) > 0) {
    stop("Unexpected beta diversity option(s): ", paste(unexpected_options, collapse = ", "))
  }
  if (!is.null(options$tree) && !is.null(options$phylogenetic_tree)) {
    stop("Pass only one of tree or phylogenetic_tree.")
  }
  if (!is.null(options$tree)) {
    return(options$tree)
  }
  options$phylogenetic_tree
}

get_beta_input <- function(data, tree) {
  if (!inherits(data, "MicrobiomeData")) {
    return(list(data = data, tree = tree))
  }

  message("[beta] reading beta diversity input from MicrobiomeData")
  if (is.null(tree)) {
    tree <- methods::slot(data, "PhylogeneticTree")
  }
  counts <- methods::slot(data, "rRNA16S")
  if (is.null(counts)) {
    stop("MicrobiomeData object must contain rRNA16S data for beta diversity.")
  }
  list(data = counts, tree = tree)
}

as_beta_matrix <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame.")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Data must have non-zero dimensions.")
  }
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    data <- as.matrix(data[, numeric_cols, drop = FALSE])
  }
  data
}

calculate_unifrac_distance <- function(data, tree, weighted) {
  if (is.null(tree)) {
    stop(
      "UniFrac distance requires a phylogenetic tree. ",
      "Pass tree = <phylo> or use a MicrobiomeData object with PhylogeneticTree."
    )
  }

  metric <- if (weighted) "weighted UniFrac" else "unweighted UniFrac"
  message("[beta] calculating ", metric, " distance with phyloseq")
  tree_component <- if (inherits(tree, "phyloseq")) phyloseq::phy_tree(tree) else tree
  physeq <- phyloseq::phyloseq(
    phyloseq::otu_table(data, taxa_are_rows = FALSE),
    tree_component
  )
  phyloseq::UniFrac(
    physeq,
    weighted = weighted,
    normalized = TRUE,
    parallel = FALSE,
    fast = TRUE
  )
}
