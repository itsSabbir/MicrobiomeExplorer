# -- validate16SRNAData --
test_that("validate16SRNAData accepts valid data", {
  data <- matrix(rpois(200, 10), nrow = 10, ncol = 20)
  expect_true(validate16SRNAData(data))
})

test_that("validate16SRNAData rejects NAs", {
  data <- matrix(rpois(200, 10), nrow = 10, ncol = 20)
  data[1, 1] <- NA
  expect_error(validate16SRNAData(data), "missing values")
})

test_that("validate16SRNAData rejects negative values", {
  data <- matrix(rpois(200, 10), nrow = 10, ncol = 20)
  data[1, 1] <- -1
  expect_error(validate16SRNAData(data), "negative values")
})

test_that("validate16SRNAData rejects non-numeric data", {
  data <- data.frame(a = letters[1:5], b = letters[6:10])
  expect_error(validate16SRNAData(data), "numeric")
})

test_that("validate16SRNAData rejects too few columns", {
  data <- matrix(rpois(30, 10), nrow = 10, ncol = 3)
  expect_error(validate16SRNAData(data, minColumns = 10), "at least 10")
})

test_that("validate16SRNAData rejects insufficient non-zero entries", {
  data <- matrix(0, nrow = 5, ncol = 20)
  data[1, 1] <- 1
  expect_error(validate16SRNAData(data, minNonZeroEntries = 5), "non-zero")
})

# -- validateMetagenomicData --
test_that("validateMetagenomicData accepts valid data", {
  data <- matrix(rpois(100, 50), nrow = 10, ncol = 10)
  expect_true(validateMetagenomicData(data))
})

test_that("validateMetagenomicData rejects empty data", {
  data <- matrix(nrow = 0, ncol = 0)
  expect_error(validateMetagenomicData(data))
})

test_that("validateMetagenomicData rejects negative values", {
  data <- matrix(rpois(100, 50), nrow = 10, ncol = 10)
  data[1, 1] <- -5
  expect_error(validateMetagenomicData(data), "negative")
})

test_that("validateMetagenomicData rejects values exceeding 1e6", {
  data <- matrix(rpois(100, 50), nrow = 10, ncol = 10)
  data[1, 1] <- 2e6
  expect_error(validateMetagenomicData(data), "typical range")
})

# -- validateMetatranscriptomicData --
test_that("validateMetatranscriptomicData accepts valid data", {
  data <- matrix(rpois(100, 100), nrow = 10, ncol = 10)
  expect_true(validateMetatranscriptomicData(data))
})

test_that("validateMetatranscriptomicData rejects negative values", {
  data <- matrix(rpois(100, 100), nrow = 10, ncol = 10)
  data[1, 1] <- -1
  expect_error(validateMetatranscriptomicData(data), "negative")
})

test_that("validateMetatranscriptomicData rejects values exceeding 1e5", {
  data <- matrix(rpois(100, 100), nrow = 10, ncol = 10)
  data[1, 1] <- 2e5
  expect_error(validateMetatranscriptomicData(data), "unusually high")
})

test_that("validateMetatranscriptomicData warns on high depth variability", {
  data <- matrix(rpois(100, 100), nrow = 10, ncol = 10)
  data[1, ] <- data[1, ] * 100
  expect_warning(validateMetatranscriptomicData(data), "variability")
})

# -- validateSampleInfo --
test_that("validateSampleInfo accepts valid metadata", {
  data <- matrix(1:20, nrow = 5, ncol = 4)
  rownames(data) <- paste0("S", 1:5)
  meta <- data.frame(Group = rep("A", 5), row.names = paste0("S", 1:5))
  expect_true(validateSampleInfo(meta, data))
})

test_that("validateSampleInfo rejects mismatched nrow", {
  data <- matrix(1:20, nrow = 5, ncol = 4)
  rownames(data) <- paste0("S", 1:5)
  meta <- data.frame(Group = rep("A", 3), row.names = paste0("S", 1:3))
  expect_error(validateSampleInfo(meta, data), "number of samples")
})

test_that("validateSampleInfo rejects duplicate rownames", {
  data <- matrix(1:8, nrow = 2, ncol = 4)
  rownames(data) <- c("S1", "S2")
  # Force duplicate rownames via attr (data.frame() constructor blocks them)
  meta <- data.frame(Group = c("A", "B"), row.names = c("S1", "S2"))
  attr(meta, "row.names") <- c("S1", "S1")
  expect_error(validateSampleInfo(meta, data), "unique")
})

test_that("validateSampleInfo rejects non-matching IDs", {
  data <- matrix(1:8, nrow = 2, ncol = 4)
  rownames(data) <- c("S1", "S2")
  meta <- data.frame(Group = c("A", "B"), row.names = c("S3", "S4"))
  expect_error(validateSampleInfo(meta, data), "do not match")
})

test_that("validateSampleInfo warns on NAs in metadata", {
  data <- matrix(1:8, nrow = 2, ncol = 4)
  rownames(data) <- c("S1", "S2")
  meta <- data.frame(Group = c("A", NA), row.names = c("S1", "S2"))
  expect_warning(validateSampleInfo(meta, data), "missing values")
})
