#' Perform Ordination Analysis on Microbiome Data
#'
#' Computes PCoA, NMDS, or PCA from a microbiome count matrix or distance
#' object, returning 2D coordinates suitable for plotting.
#'
#' @param data A matrix or data.frame (rows = samples, cols = taxa) for PCA,
#'   or a \code{dist} object for PCoA/NMDS.
#' @param method Character string: \code{"PCoA"}, \code{"NMDS"}, or
#'   \code{"PCA"}. Default: \code{"PCoA"}.
#' @param dist_method Character string passed to \code{calculateBetaDiversity()}
#'   when \code{data} is a matrix and \code{method} is not \code{"PCA"}.
#'   Default: \code{"bray"}.
#' @param k Integer. Number of dimensions for NMDS. Default: \code{2}.
#' @param scale Logical. Whether to scale variables for PCA. Default: \code{TRUE}.
#' @return A list with components:
#'   \describe{
#'     \item{coordinates}{data.frame of sample coordinates (Axis1, Axis2).
#'       Row names are sample names.}
#'     \item{method}{Character string naming the method used.}
#'     \item{variance_explained}{Numeric vector of proportion variance explained
#'       per axis (PCoA and PCA only; NULL for NMDS).}
#'     \item{stress}{Numeric NMDS stress value (NMDS only; NULL otherwise).}
#'   }
#'
#' @examples
#' set.seed(42)
#' counts <- matrix(rpois(60, lambda = 15), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Sample", 1:10)
#' colnames(counts) <- paste0("OTU", 1:6)
#' res <- performOrdination(counts, method = "PCoA")
#' head(res$coordinates)
#'
#' @references
#' Gower, J.C. (1966). Some distance properties of latent root and vector
#' methods used in multivariate analysis. Biometrika, 53, 325-338.
#'
#' @importFrom vegan metaMDS scores
#' @importFrom ape pcoa
#' @importFrom stats prcomp
#' @export
performOrdination <- function(data, method = "PCoA", dist_method = "bray",
                               k = 2, scale = TRUE) {
  valid_methods <- c("PCoA", "NMDS", "PCA")
  if (!method %in% valid_methods) {
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))
  }

  if (method == "PCA") {
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("PCA requires a matrix or data.frame as input.")
    }
    if (is.data.frame(data)) {
      data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE])
    }
    pca_res <- stats::prcomp(data, scale. = scale)
    coords <- as.data.frame(pca_res$x[, 1:min(2, ncol(pca_res$x))])
    colnames(coords) <- paste0("Axis", seq_len(ncol(coords)))
    var_exp <- (pca_res$sdev^2 / sum(pca_res$sdev^2))[1:min(2, length(pca_res$sdev))]
    return(list(coordinates = coords, method = "PCA",
                variance_explained = var_exp, stress = NULL))
  }

  # PCoA or NMDS — need a dist object
  if (!inherits(data, "dist")) {
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("data must be a matrix, data.frame, or dist object.")
    }
    data <- calculateBetaDiversity(data, method = dist_method)
  }

  if (method == "PCoA") {
    pcoa_res <- ape::pcoa(data)
    coords <- as.data.frame(pcoa_res$vectors[, 1:min(2, ncol(pcoa_res$vectors))])
    colnames(coords) <- paste0("Axis", seq_len(ncol(coords)))
    var_exp <- pcoa_res$values$Relative_eig[1:min(2, nrow(pcoa_res$values))]
    return(list(coordinates = coords, method = "PCoA",
                variance_explained = var_exp, stress = NULL))
  }

  # NMDS
  nmds_res <- suppressMessages(vegan::metaMDS(data, k = k, trace = FALSE))
  coords <- as.data.frame(vegan::scores(nmds_res, display = "sites"))
  colnames(coords) <- paste0("Axis", seq_len(ncol(coords)))
  return(list(coordinates = coords, method = "NMDS",
              variance_explained = NULL, stress = nmds_res$stress))
}
