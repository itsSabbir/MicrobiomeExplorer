#' Advanced Differential Expression Analysis
#'
#' @description
#' Performs differential expression analysis using specified statistical methods.
#'
#' @param microbiomeData Matrix or dataframe with microbiome data.
#' @param conditions Factor vector specifying the condition for each sample.
#' @param analysisType Type of analysis to perform, default "DESeq2".
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
#' @import edgeR
#' @import limma
#' @export
performDifferentialExpression <- function(microbiomeData, conditions, analysisType = "DESeq2", countThreshold = 5, minSamples = 2) {
  # Validate input data
  if (!is.matrix(microbiomeData)) {
    stop("microbiomeData must be a matrix.")
  }

  # Ensure conditions length matches the number of columns (samples) in microbiomeData
  if (!is.factor(conditions) || length(conditions) != ncol(microbiomeData)) {
    stop("conditions must be a factor with length equal to the number of columns (samples) in microbiomeData.")
  }

  if (!analysisType %in% c("DESeq2", "EdgeR")) {
    stop("analysisType must be either 'DESeq2' or 'EdgeR'.")
  }

  # Preprocessing data: Filtering low-count genes
  # Adjust this step for a standard matrix
  keep <- rowSums(microbiomeData >= countThreshold) >= minSamples
  microbiomeData <- microbiomeData[keep, ]

  # Normalizing data
  # For DESeq2, normalization is part of the DESeq workflow
  # For EdgeR, normalization factors are calculated during the analysis
  # Therefore, no explicit normalization step is needed here

  # Log transformation for EdgeR (if needed)
  if (analysisType == "EdgeR") {
    microbiomeData <- log2(microbiomeData + 1)
  }

  # Perform differential expression analysis
  if (analysisType == "DESeq2") {
    # DESeq2 Analysis
    dds <- DESeqDataSetFromMatrix(countData = microbiomeData, colData = data.frame(conditions), design = ~ conditions)
    dds <- DESeq(dds)
    resultsDESeq2 <- results(dds)
    return(list(DESeq2 = resultsDESeq2))
  } else {
    # EdgeR Analysis
    group <- factor(conditions)
    y <- DGEList(counts = microbiomeData, group = group)
    y <- calcNormFactors(y)
    design <- model.matrix(~ group)
    y <- estimateDisp(y, design)
    fit <- glmQLFit(y, design)
    resultsEdgeR <- glmQLFTest(fit, coef = 2)
    topTags <- topTags(resultsEdgeR)
    return(list(EdgeR = topTags))
  }
}




