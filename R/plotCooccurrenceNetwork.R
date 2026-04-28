#' Plot a Co-occurrence Network
#'
#' Visualises the \code{igraph} network from
#' \code{\link{buildCooccurrenceNetwork}} using ggplot2.
#'
#' @param network_result List returned by \code{buildCooccurrenceNetwork()}.
#' @param layout Character string: \code{"fr"} (Fruchterman-Reingold),
#'   \code{"circle"}, or \code{"kk"} (Kamada-Kawai). Default: \code{"fr"}.
#' @param node_size_by Character string: \code{"degree"} or
#'   \code{"betweenness"}. Default: \code{"degree"}.
#' @param color_by Character string: \code{"community"}. Default:
#'   \code{"community"}.
#' @param label_top_n Integer. Number of highest-degree nodes to label.
#'   Default: \code{10}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' net <- buildCooccurrenceNetwork(counts, cor_threshold = 0.3)
#' plotCooccurrenceNetwork(net)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_text
#'   scale_colour_discrete scale_size_continuous labs theme_void
#' @importFrom utils head
#' @export
plotCooccurrenceNetwork <- function(network_result, layout = "fr",
                                      node_size_by = "degree",
                                      color_by = "community",
                                      label_top_n = 10) {
  if (!is.list(network_result) || !"graph" %in% names(network_result)) {
    stop("network_result must be the list returned by buildCooccurrenceNetwork().")
  }
  g <- network_result$graph
  if (igraph::vcount(g) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No edges pass the filtering thresholds"))
  }

  layout_fn <- switch(layout,
    "fr"     = igraph::layout_with_fr,
    "circle" = igraph::layout_in_circle,
    "kk"     = igraph::layout_with_kk,
    igraph::layout_with_fr
  )
  coords <- layout_fn(g)
  rownames(coords) <- igraph::V(g)$name

  node_df <- data.frame(
    name = igraph::V(g)$name,
    x = coords[, 1], y = coords[, 2],
    stringsAsFactors = FALSE
  )
  node_df <- merge(node_df, network_result$node_metrics,
                   by.x = "name", by.y = "taxon")
  node_df$community <- factor(node_df$community)
  node_df$size_val <- node_df[[node_size_by]]

  # Top labels
  top_nodes <- head(node_df[order(-node_df$degree), ], label_top_n)
  node_df$label <- ifelse(node_df$name %in% top_nodes$name, node_df$name, "")

  # Edge coordinates
  el <- igraph::as_data_frame(g, what = "edges")
  edge_df <- data.frame(
    x = coords[el$from, 1], y = coords[el$from, 2],
    xend = coords[el$to, 1], yend = coords[el$to, 2],
    sign = el$sign,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                    xend = .data[["xend"]], yend = .data[["yend"]]),
      colour = "grey70", alpha = 0.4, linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data = node_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                    colour = .data[["community"]],
                    size = .data[["size_val"]]),
      alpha = 0.85
    ) +
    ggplot2::geom_text(
      data = node_df[node_df$label != "", ],
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                    label = .data[["label"]]),
      size = 3, nudge_y = 0.05, check_overlap = TRUE
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
    ggplot2::labs(colour = "Community",
                  title = "Co-occurrence Network") +
    ggplot2::theme_void()
}
