test_that("calculateAlphaDiversity returns all requested indices", {
  set.seed(42)
  data <- matrix(rpois(50, lambda = 10), nrow = 10, ncol = 5)
  rownames(data) <- paste0("S", 1:10)
  colnames(data) <- paste0("OTU", 1:5)

  result <- calculateAlphaDiversity(data, indices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"))
  expect_true(is.data.frame(result))
  expect_true(all(c("Sample", "Shannon", "Simpson", "Chao1", "ACE", "Fisher") %in% colnames(result)))
  expect_equal(nrow(result), 10)
})

test_that("Shannon of uniform distribution equals log(S)", {
  data <- matrix(rep(1, 15), nrow = 3, ncol = 5)
  rownames(data) <- paste0("S", 1:3)
  colnames(data) <- paste0("OTU", 1:5)

  result <- calculateAlphaDiversity(data, indices = "Shannon")
  expect_equal(result$Shannon, rep(log(5), 3), tolerance = 1e-10)
})

test_that("Simpson of single-species sample is 0", {
  data <- matrix(0, nrow = 2, ncol = 5)
  data[1, 1] <- 100
  data[2, 3] <- 50
  rownames(data) <- paste0("S", 1:2)
  colnames(data) <- paste0("OTU", 1:5)

  result <- calculateAlphaDiversity(data, indices = "Simpson")
  expect_equal(result$Simpson, c(0, 0))
})

test_that("all-zero row returns NA for all indices", {
  data <- matrix(c(0, 0, 0, 0, 0, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
  rownames(data) <- c("empty", "normal")
  colnames(data) <- paste0("OTU", 1:5)

  result <- calculateAlphaDiversity(data, indices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"))
  expect_true(is.na(result$Shannon[1]))
  expect_true(is.na(result$Simpson[1]))
  expect_true(is.na(result$Chao1[1]))
  expect_true(is.na(result$ACE[1]))
  expect_true(is.na(result$Fisher[1]))
  expect_false(is.na(result$Shannon[2]))
})

test_that("Fisher index returns finite positive values for count data", {
  set.seed(123)
  data <- matrix(rpois(100, lambda = 15), nrow = 10, ncol = 10)
  rownames(data) <- paste0("S", 1:10)
  colnames(data) <- paste0("OTU", 1:10)

  result <- calculateAlphaDiversity(data, indices = "Fisher")
  expect_true(all(is.finite(result$Fisher)))
  expect_true(all(result$Fisher > 0))
})

test_that("rarefied mode works with valid count data", {
  set.seed(42)
  data <- matrix(rpois(50, lambda = 20), nrow = 10, ncol = 5)
  rownames(data) <- paste0("S", 1:10)
  colnames(data) <- paste0("OTU", 1:5)

  result <- calculateAlphaDiversity(data, indices = "Shannon", rarefied = TRUE)
  expect_true("Shannon_rarefied" %in% colnames(result))
  expect_equal(nrow(result), 10)
})

test_that("rarefied mode errors when a sample has zero total", {
  data <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 2, byrow = TRUE)
  rownames(data) <- c("empty", "normal")
  colnames(data) <- paste0("OTU", 1:3)

  expect_error(calculateAlphaDiversity(data, indices = "Shannon", rarefied = TRUE),
               "zero total count")
})

test_that("invalid input types error", {
  expect_error(calculateAlphaDiversity("not a matrix"), "matrix or dataframe")
  expect_error(calculateAlphaDiversity(matrix(nrow = 0, ncol = 0)), "non-zero dimensions")
})

test_that("unsupported index produces warning", {
  data <- matrix(rpois(20, 10), nrow = 4, ncol = 5)
  rownames(data) <- paste0("S", 1:4)
  colnames(data) <- paste0("OTU", 1:5)

  expect_warning(calculateAlphaDiversity(data, indices = "Nonexistent"), "not supported")
})
