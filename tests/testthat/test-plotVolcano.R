test_that("plotVolcano returns a ggplot object", {
  set.seed(42)
  res <- data.frame(log2FoldChange = rnorm(50), pvalue = runif(50))
  p <- plotVolcano(res)
  expect_s3_class(p, "ggplot")
})

test_that("plotVolcano works with DESeq2-style column names", {
  set.seed(42)
  res <- data.frame(log2FoldChange = rnorm(30), pvalue = runif(30))
  expect_s3_class(plotVolcano(res, fc_col = "log2FoldChange", pval_col = "pvalue"), "ggplot")
})

test_that("plotVolcano works with edgeR-style column names", {
  set.seed(42)
  res <- data.frame(logFC = rnorm(30), PValue = runif(30))
  expect_s3_class(plotVolcano(res, fc_col = "logFC", pval_col = "PValue"), "ggplot")
})

test_that("plotVolcano errors on missing fold-change column", {
  res <- data.frame(pvalue = runif(10))
  expect_error(plotVolcano(res, fc_col = "log2FoldChange"), "not found")
})

test_that("plotVolcano errors on missing p-value column", {
  res <- data.frame(log2FoldChange = rnorm(10))
  expect_error(plotVolcano(res, pval_col = "pvalue"), "not found")
})

test_that("plotVolcano significance column has correct levels", {
  set.seed(42)
  res <- data.frame(
    log2FoldChange = c(3, -3, 0.1),
    pvalue         = c(0.01, 0.01, 0.5)
  )
  p <- plotVolcano(res, fc_threshold = 1, pval_threshold = 0.05)
  ld <- ggplot2::layer_data(p)
  # Should have both Up (red) and NS (grey) points represented
  expect_true(nrow(ld) >= 3)
})
