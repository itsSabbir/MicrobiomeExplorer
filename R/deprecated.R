# Backward-compatible wrappers for renamed functions

#' @rdname calculateAlphaDiversity
#' @export
calculate_alpha_diversity <- function(...) {
  .Deprecated("calculateAlphaDiversity")
  calculateAlphaDiversity(...)
}

#' @rdname calculateStats
#' @export
calculate_stats <- function(...) {
  .Deprecated("calculateStats")
  calculateStats(...)
}

#' @rdname plotMicrobiomeHeatmap
#' @export
plot_microbiome_heatmap <- function(...) {
  .Deprecated("plotMicrobiomeHeatmap")
  plotMicrobiomeHeatmap(...)
}

#' @rdname advancedRarefactionPlot
#' @export
AdvancedRarefactionPlot <- function(...) {
  .Deprecated("advancedRarefactionPlot")
  advancedRarefactionPlot(...)
}
