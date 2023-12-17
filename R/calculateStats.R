# calculate_stats.R

#' Calculate Extended Statistics for Microbiome Data
#'
#' This function calculates extended statistics (mean, median, standard deviation,
#' variance, range, and interquartile range) for each feature in a microbiome dataset.
#' It provides a comprehensive statistical overview, essential for preliminary data analysis.
#'
#' @details
#' \itemize{
#'   \item \strong{Comprehensive Statistical Analysis}: Calculates key statistical measures for each feature.
#'   \item \strong{Handling Missing Values}: Ignores NA values in the calculations, ensuring robust results.
#' }
#'
#' @param data A matrix or dataframe with rows as samples and columns as features.
#' @return A dataframe with calculated statistics for each feature.
#' @export
#'
#' @examples
#' data(iris)
#' stats <- calculate_stats(iris[,1:4])
#' print(stats)
#'
#' @references
#' Chambers, J. M. (2008). Software for Data Analysis: Programming with R. Springer.
#' Gentleman, R., & Ihaka, R. (2000). Lexical Scope and Statistical Computing. Journal of Computational and Graphical Statistics, 9(3).
calculate_stats <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or dataframe.")
  }

  if (ncol(data) == 0) {
    stop("Data must have at least one column.")
  }

  stats <- apply(data, 2, function(column) {
    c(mean = mean(column, na.rm = TRUE),
      median = median(column, na.rm = TRUE),
      sd = sd(column, na.rm = TRUE),
      variance = var(column, na.rm = TRUE),
      range_min = min(column, na.rm = TRUE),
      range_max = max(column, na.rm = TRUE),
      IQR = IQR(column, na.rm = TRUE))
  })

  return(as.data.frame(t(stats)))
}


