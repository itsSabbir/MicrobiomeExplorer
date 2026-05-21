#' Build a Co-occurrence Network from Microbiome Data
#'
#' Constructs an \code{igraph} co-occurrence network from pairwise taxon
#' correlations, filtering edges by correlation strength and significance,
#' and performing community detection.
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param method Character string: \code{"spearman"} or \code{"pearson"}.
#'   Default: \code{"spearman"}.
#' @param cor_threshold Numeric. Minimum absolute correlation for an edge.
#'   Default: \code{0.6}.
#' @param pval_threshold Numeric. Maximum p-value for an edge.
#'   Default: \code{0.05}.
#' @param min_prevalence Numeric in [0, 1]. Prevalence filter passed to
#'   \code{\link{calculateCorrelation}}. Default: \code{0.1}.
#' @return A list with:
#'   \describe{
#'     \item{graph}{An \code{igraph} graph object.}
#'     \item{node_metrics}{data.frame with degree, betweenness, community.}
#'     \item{edge_list}{data.frame of edges with correlation and p-value.}
#'     \item{n_nodes}{Integer.}
#'     \item{n_edges}{Integer.}
#'     \item{modularity}{Numeric modularity score.}
#'   }
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' counts <- matrix(rpois(200, 15), nrow = 20, ncol = 10)
#' rownames(counts) <- paste0("S", 1:20)
#' colnames(counts) <- paste0("OTU", 1:10)
#' net <- buildCooccurrenceNetwork(counts, cor_threshold = 0.3)
#' net$n_edges
#' }
#'
#' @export
buildCooccurrenceNetwork <- function(data, method = "spearman",
                                       cor_threshold = 0.6,
                                       pval_threshold = 0.05,
                                       min_prevalence = 0.1) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install with: install.packages('igraph')")
  }

  corr_res <- calculateCorrelation(data, method = method,
                                    min_prevalence = min_prevalence)
  cor_mat <- corr_res$correlation
  p_mat <- corr_res$pvalue

  # Build adjacency: keep significant strong correlations
  adj <- abs(cor_mat) >= cor_threshold & p_mat <= pval_threshold
  diag(adj) <- FALSE
  adj_weighted <- cor_mat * adj

  g <- igraph::graph_from_adjacency_matrix(
    adj_weighted, mode = "undirected", weighted = TRUE, diag = FALSE
  )

  # Remove isolated nodes
  g <- igraph::delete_vertices(g, igraph::degree(g) == 0)

  if (igraph::vcount(g) == 0) {
    return(list(
      graph = g,
      node_metrics = data.frame(taxon = character(0), degree = integer(0),
                                 betweenness = numeric(0), community = integer(0)),
      edge_list = data.frame(from = character(0), to = character(0),
                              correlation = numeric(0), pvalue = numeric(0)),
      n_nodes = 0L, n_edges = 0L, modularity = NA_real_
    ))
  }

  # Edge sign attribute
  igraph::E(g)$sign <- ifelse(igraph::E(g)$weight > 0, "positive", "negative")
  igraph::E(g)$weight <- abs(igraph::E(g)$weight)

  # Community detection
  comm <- igraph::cluster_louvain(g)

  # Node metrics
  node_metrics <- data.frame(
    taxon = igraph::V(g)$name,
    degree = igraph::degree(g),
    betweenness = igraph::betweenness(g),
    community = igraph::membership(comm),
    stringsAsFactors = FALSE
  )

  # Edge list
  el <- igraph::as_data_frame(g, what = "edges")
  edge_list <- data.frame(
    from = el$from, to = el$to,
    correlation = el$weight * ifelse(el$sign == "positive", 1, -1),
    sign = el$sign,
    stringsAsFactors = FALSE
  )

  list(
    graph = g,
    node_metrics = node_metrics,
    edge_list = edge_list,
    n_nodes = igraph::vcount(g),
    n_edges = igraph::ecount(g),
    modularity = igraph::modularity(comm)
  )
}
