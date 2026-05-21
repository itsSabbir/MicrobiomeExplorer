#' Discover Biomarker Taxa that Distinguish Sample Groups
#'
#' Identifies taxa significantly associated with sample groups using a
#' LEfSe-style pipeline (Kruskal-Wallis + log10 fold-change effect size) or
#' random forest importance, or a combination of both.
#'
#' @param data A numeric matrix (rows = samples, cols = taxa).
#' @param sample_info A data.frame with sample metadata.
#' @param group_var Character string naming the grouping column.
#' @param method Character string: \code{"kw"} (Kruskal-Wallis + effect size),
#'   \code{"rf"} (random forest importance), or \code{"combined"} (intersection
#'   of both). Default: \code{"kw"}.
#' @param pval_threshold Numeric. BH-adjusted significance cutoff. Default:
#'   \code{0.05}.
#' @param lda_threshold Deprecated alias for \code{effect_size_threshold}.
#' @param rf_top_n Integer. Top N features from RF importance to consider
#'   significant (used when \code{method} is \code{"rf"} or \code{"combined"}).
#'   Default: \code{20}.
#' @param effect_size_threshold Numeric. Minimum absolute effect size, measured
#'   as log10 fold-change of group means. Default: \code{0.5} (roughly a 3x
#'   difference).
#' @return A list with:
#'   \describe{
#'     \item{biomarkers}{data.frame of significant taxa with columns: taxon,
#'       enriched_group, pvalue, padj, effect_size.}
#'     \item{all_results}{Full data.frame for all tested taxa.}
#'     \item{method}{Character string.}
#'     \item{n_significant}{Integer count.}
#'   }
#'
#' @examples
#' \donttest{
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
                                 lda_threshold = NULL, rf_top_n = 20,
                                 effect_size_threshold = NULL) {
  .validate_biomarker_inputs(method, sample_info, group_var)
  aligned <- .align_biomarker_inputs(data, sample_info, group_var)
  data <- aligned$data
  groups <- aligned$groups
  sample_info <- aligned$sample_info
  effect_size_threshold <- .resolve_effect_size_threshold(
    effect_size_threshold,
    lda_threshold,
    missing(lda_threshold)
  )

  kw_results <- NULL
  rf_results <- NULL

  if (method %in% c("kw", "combined")) {
    kw_results <- .calculate_kw_biomarkers(data, groups)
  }

  if (method %in% c("rf", "combined")) {
    rf_results <- .calculate_rf_biomarkers(
      data,
      sample_info,
      list(group_var = group_var, top_n = rf_top_n)
    )
  }

  all_res <- .combine_biomarker_results(kw_results, rf_results, method)
  sig <- .filter_biomarkers(
    all_res,
    method,
    list(pval = pval_threshold, effect_size = effect_size_threshold)
  )
  sig <- .sort_biomarkers(sig)

  list(
    biomarkers = sig,
    all_results = all_res,
    method = method,
    n_significant = nrow(sig)
  )
}

.validate_biomarker_inputs <- function(method, sample_info, group_var) {
  if (!method %in% c("kw", "rf", "combined")) {
    stop("method must be 'kw', 'rf', or 'combined'.")
  }
  if (!group_var %in% colnames(sample_info)) {
    stop(paste("group_var", group_var, "not found in sample_info."))
  }
}

.align_biomarker_inputs <- function(data, sample_info, group_var) {
  common <- intersect(rownames(data), rownames(sample_info))
  if (length(common) < 6) {
    stop("At least 6 matching samples are needed.")
  }
  sample_info <- sample_info[common, , drop = FALSE]
  list(
    data = data[common, , drop = FALSE],
    groups = factor(sample_info[[group_var]]),
    sample_info = sample_info
  )
}

.resolve_effect_size_threshold <- function(effect_size_threshold, lda_threshold,
                                           lda_missing) {
  if (!lda_missing) {
    .Deprecated("effect_size_threshold", old = "lda_threshold")
    if (!is.null(effect_size_threshold) && !is.null(lda_threshold)) {
      stop("Use only one of effect_size_threshold or lda_threshold.")
    }
    if (!is.null(lda_threshold)) {
      return(lda_threshold)
    }
  }
  if (is.null(effect_size_threshold)) {
    return(0.5)
  }
  effect_size_threshold
}

.calculate_kw_biomarkers <- function(data, groups) {
  kw_results <- data.frame(
    taxon = colnames(data),
    pvalue = NA_real_,
    padj = NA_real_,
    enriched_group = NA_character_,
    effect_size = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(ncol(data))) {
    taxon <- colnames(data)[i]
    vals <- data[, i]
    kw_results$pvalue[i] <- .biomarker_kw_pvalue(vals, groups, taxon)
    kw_results$enriched_group[i] <- .enriched_biomarker_group(vals, groups)
    kw_results$effect_size[i] <- .biomarker_effect_size(vals, groups)
  }
  kw_results$padj <- stats::p.adjust(kw_results$pvalue, method = "BH")
  kw_results
}

.calculate_rf_biomarkers <- function(data, sample_info, options) {
  rf_res <- performRandomForest(data, sample_info, options$group_var, seed = 42)
  imp <- rf_res$importance
  imp <- imp[order(-imp$MeanDecreaseGini), ]
  rf_top <- head(imp$Taxon, options$top_n)
  data.frame(
    taxon = imp$Taxon,
    rf_importance = imp$MeanDecreaseGini,
    rf_significant = imp$Taxon %in% rf_top,
    stringsAsFactors = FALSE
  )
}

.combine_biomarker_results <- function(kw_results, rf_results, method) {
  if (method == "kw") {
    return(kw_results)
  }
  if (method == "rf") {
    return(.add_kw_pvalue_columns(rf_results))
  }
  merge(kw_results, rf_results, by = "taxon")
}

.add_kw_pvalue_columns <- function(results) {
  results$pvalue <- NA_real_
  results$padj <- NA_real_
  results
}

.filter_biomarkers <- function(all_res, method, thresholds) {
  if (method == "rf") {
    return(all_res[all_res$rf_significant, ])
  }
  selected <- all_res$padj < thresholds$pval &
    all_res$effect_size >= thresholds$effect_size
  if (method == "combined") {
    selected <- selected & all_res$rf_significant
  }
  all_res[selected, ]
}

.sort_biomarkers <- function(sig) {
  sort_col <- if ("effect_size" %in% colnames(sig)) "effect_size"
              else if ("rf_importance" %in% colnames(sig)) "rf_importance"
              else NULL
  if (!is.null(sort_col) && nrow(sig) > 0) {
    return(sig[order(-abs(sig[[sort_col]])), ])
  }
  sig
}

.biomarker_kw_pvalue <- function(vals, groups, taxon) {
  tryCatch({
    pvalue <- stats::kruskal.test(vals ~ groups)$p.value
    if (!is.finite(pvalue)) {
      stop("non-finite p-value")
    }
    pvalue
  }, error = function(e) {
    message(sprintf(
      "[biomark] KW failed for taxon '%s': %s",
      taxon,
      conditionMessage(e)
    ))
    1.0
  })
}

.enriched_biomarker_group <- function(vals, groups) {
  medians <- tapply(vals, groups, stats::median)
  names(which.max(medians))
}

.biomarker_effect_size <- function(vals, groups) {
  group_means <- tapply(vals, groups, mean)
  max_mean <- max(group_means)
  min_mean <- min(group_means)
  if (min_mean > 0) {
    return(abs(log10(max_mean / min_mean)))
  }
  if (max_mean > 0) {
    return(abs(log10(max_mean + 1)))
  }
  0
}
