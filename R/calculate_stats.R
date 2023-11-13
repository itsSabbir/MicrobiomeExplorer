# calculate_stats.R

#' Calculate Extended Statistics for Microbiome Data
#'
#' This function calculates extended statistics (mean, median, standard deviation,
#' variance, range, and interquartile range) for each feature in a microbiome dataset.
#'
#' @param data A matrix or dataframe with rows as samples and columns as features.
#' @return A dataframe with calculated statistics for each feature.
#' @export
#'
#' @examples
#' data(iris)
#' stats <- calculate_stats(iris[,1:4])
#' print(stats)
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
