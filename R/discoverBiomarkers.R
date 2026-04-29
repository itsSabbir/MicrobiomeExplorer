#' Discover Biomarker Taxa that Distinguish Sample Groups
#'
#' Identifies taxa significantly associated with sample groups using a
#' LEfSe-style pipeline (Kruskal-Wallis + pairwise Wilcoxon + LDA effect
#' size) or random forest importance, or a combination of both.
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param sample_info A data.frame with sample metadata.
#' @param group_var Character string naming the grouping column.
#' @param method Character string: \code{"kw"} (Kruskal-Wallis + effect size),
#'   \code{"rf"} (random forest importance), or \code{"combined"} (intersection
#'   of both). Default: \code{"kw"}.
#' @param pval_threshold Numeric. Significance cutoff. Default: \code{0.05}.
#' @param lda_threshold Numeric. Minimum absolute effect size (log10 fold-change
#'   between group means). Default: \code{0.5} (roughly a 3x difference).
#' @param rf_top_n Integer. Top N features from RF importance to consider
#'   significant (used when \code{method} is \code{"rf"} or \code{"combined"}).
#'   Default: \code{20}.
#' @return A list with:
#'   \describe{
#'     \item{biomarkers}{data.frame of significant taxa with columns: taxon,
#'       enriched_group, pvalue, effect_size.}
#'     \item{all_results}{Full data.frame for all tested taxa.}
#'     \item{method}{Character string.}
#'     \item{n_significant}{Integer count.}
#'   }
#'
#' @examples
#' \dontrun{
#' counts <- matrix(rpois(200, 20), nrow = 20, ncol = 10)
#' rownames(counts) <- paste0("S", 1:20)
#' colnames(counts) <- paste0("OTU", 1:10)
#' meta <- data.frame(Group = rep(c("A", "B"), 10), row.names = paste0("S", 1:20))
#' bm <- discoverBiomarkers(counts, meta, "Group")
#' bm$biomarkers
#' }
#'
#' @importFrom utils head
#' @export
discoverBiomarkers <- function(data, sample_info, group_var,
                                 method = "kw", pval_threshold = 0.05,
                                 lda_threshold = 0.5, rf_top_n = 20) {
  if (!method %in% c("kw", "rf", "combined")) {
    stop("method must be 'kw', 'rf', or 'combined'.")
  }
  if (!group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info."))
  }

  common <- intersect(rownames(data), rownames(sample_info))
  if (length(common) < 6) stop("At least 6 matching samples are needed.")
  data <- data[common, , drop = FALSE]
  groups <- factor(sample_info[common, group_var])

  kw_results <- NULL
  rf_results <- NULL

  if (method %in% c("kw", "combined")) {
    kw_results <- data.frame(
      taxon = colnames(data),
      pvalue = NA_real_,
      enriched_group = NA_character_,
      effect_size = NA_real_,
      stringsAsFactors = FALSE
    )
    for (i in seq_len(ncol(data))) {
      vals <- data[, i]
      kw_p <- tryCatch(
        stats::kruskal.test(vals ~ groups)$p.value,
        error = function(e) 1.0
      )
      kw_results$pvalue[i] <- kw_p

      medians <- tapply(vals, groups, stats::median)
      kw_results$enriched_group[i] <- names(which.max(medians))

      # LDA-like effect size: log10 of ratio between max and min group means
      group_means <- tapply(vals, groups, mean)
      max_mean <- max(group_means)
      min_mean <- min(group_means)
      if (min_mean > 0) {
        kw_results$effect_size[i] <- abs(log10(max_mean / min_mean))
      } else if (max_mean > 0) {
        kw_results$effect_size[i] <- abs(log10(max_mean + 1))
      } else {
        kw_results$effect_size[i] <- 0
      }
    }
  }

  if (method %in% c("rf", "combined")) {
    rf_res <- performRandomForest(data, sample_info[common, , drop = FALSE],
                                   group_var, seed = 42)
    imp <- rf_res$importance
    imp <- imp[order(-imp$MeanDecreaseGini), ]
    rf_top <- head(imp$Taxon, rf_top_n)
    rf_results <- data.frame(
      taxon = imp$Taxon,
      rf_importance = imp$MeanDecreaseGini,
      rf_significant = imp$Taxon %in% rf_top,
      stringsAsFactors = FALSE
    )
  }

  if (method == "kw") {
    all_res <- kw_results
    sig <- all_res[all_res$pvalue < pval_threshold &
                     all_res$effect_size >= lda_threshold, ]
  } else if (method == "rf") {
    all_res <- rf_results
    sig <- all_res[all_res$rf_significant, ]
  } else {
    all_res <- merge(kw_results, rf_results, by = "taxon")
    sig <- all_res[all_res$pvalue < pval_threshold &
                     all_res$effect_size >= lda_threshold &
                     all_res$rf_significant, ]
  }

  sort_col <- if ("effect_size" %in% colnames(sig)) "effect_size"
              else if ("rf_importance" %in% colnames(sig)) "rf_importance"
              else NULL
  if (!is.null(sort_col) && nrow(sig) > 0) {
    sig <- sig[order(-abs(sig[[sort_col]])), ]
  }

  list(
    biomarkers = sig,
    all_results = all_res,
    method = method,
    n_significant = nrow(sig)
  )
}
