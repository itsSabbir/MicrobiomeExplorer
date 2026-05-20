#' Advanced Differential Expression Analysis
#'
#' @description
#' Performs differential expression analysis using specified statistical methods.
#'
#' @param microbiomeData Integer count matrix with rows as features/taxa and
#'   columns as samples (standard DESeq2/edgeR orientation). Note: most other
#'   functions in this package use rows=samples; transpose your data if needed.
#' @param conditions Factor vector specifying the condition for each sample.
#'   Length must equal \code{ncol(microbiomeData)}.
#' @param analysisType Type of analysis to perform: \code{"DESeq2"} (default) or
#'   \code{"EdgeR"}.
#' @param countThreshold The minimum count threshold for a gene to be kept for analysis.
#' @param minSamples The minimum number of samples that must meet the countThreshold.
#' @return Results of the differential expression analysis.
#'
#' @examples
#' microbiomeData <- matrix(rpois(400, lambda = 20), nrow = 100, ncol = 4)
#' conditions <- factor(rep(c("Condition1", "Condition2"), each = 2))
#'
#' results <- performDifferentialExpression(microbiomeData, conditions, analysisType = "DESeq2")
#'
#' @importFrom DESeq2 DESeqDataSetFromMatrix
#' @importFrom DESeq2 DESeq
#' @importFrom DESeq2 results
#' @importFrom DESeq2 estimateSizeFactors
#' @import edgeR
#' @import limma
#' @export
performDifferentialExpression <- function(microbiomeData, conditions, analysisType = "DESeq2", countThreshold = 5, minSamples = 2) {
  if (!is.matrix(microbiomeData)) {
    stop("microbiomeData must be a matrix.")
  }

  if (!is.factor(conditions) || length(conditions) != ncol(microbiomeData)) {
    stop("conditions must be a factor with length equal to the number of columns (samples) in microbiomeData.")
  }

  if (!analysisType %in% c("DESeq2", "EdgeR")) {
    stop("analysisType must be either 'DESeq2' or 'EdgeR'.")
  }

  keep <- rowSums(microbiomeData >= countThreshold) >= minSamples
  microbiomeData <- microbiomeData[keep, , drop = FALSE]
  features_passed <- nrow(microbiomeData)

  if (analysisType == "DESeq2") {
    resultsDESeq2 <- .run_deseq2_de(microbiomeData, conditions)
    message(sprintf("[de] DESeq2 ran with %d features after filtering", features_passed))
    return(list(DESeq2 = resultsDESeq2))
  }

  resultsEdgeR <- .run_edger_de(microbiomeData, conditions)
  message(sprintf("[de] EdgeR ran with %d features after filtering", features_passed))
  list(EdgeR = resultsEdgeR)
}

.run_deseq2_de <- function(microbiomeData, conditions) {
  dds <- DESeqDataSetFromMatrix(
    countData = microbiomeData,
    colData = data.frame(conditions = conditions),
    design = ~ conditions
  )
  dds <- tryCatch(
    DESeq(dds),
    error = function(error) {
      if (!.is_deseq_size_factor_error(error)) {
        stop("DESeq2 analysis failed: ", conditionMessage(error), call. = FALSE)
      }
      message("[de] DESeq2 size factor estimation failed; retrying with poscounts")
      dds <- DESeq2::estimateSizeFactors(dds, type = "poscounts")
      DESeq(dds)
    }
  )
  results(dds)
}

.is_deseq_size_factor_error <- function(error) {
  grepl(
    "every gene contains at least one zero|cannot compute log geometric means|geometric means|estimateSizeFactors|size factors",
    conditionMessage(error),
    ignore.case = TRUE
  )
}

.run_edger_de <- function(microbiomeData, conditions) {
  group <- factor(conditions)
  y <- DGEList(counts = microbiomeData, group = group)
  y <- calcNormFactors(y)
  design <- model.matrix(~ group)
  y <- estimateDisp(y, design)
  fit <- glmQLFit(y, design)
  resultsEdgeR <- glmQLFTest(fit, coef = 2)
  topTags(resultsEdgeR)
}

