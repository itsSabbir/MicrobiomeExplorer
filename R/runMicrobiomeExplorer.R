#' Launch Shiny App for MicrobiomeExplorer
#'
#' This function initiates a Shiny application included with the MicrobiomeExplorer
#' package. The app provides an interactive user interface for visualizing and
#' analyzing microbiome data. It supports functionalities such as generating
#' various plots, performing statistical analyses, and exploring microbiome datasets.
#'
#' The application code is located in the \code{./inst/shiny-scripts} directory
#' within the MicrobiomeExplorer package.
#'
#' @return This function does not return a value, but it opens a Shiny application
#' in the user's default web browser.
#'
#' @examples
#' \dontrun{
#'   # Launch the Shiny app from the MicrobiomeExplorer package
#'   MicrobiomeExplorer::runMicrobiomeExplorerApp()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials.
#' \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp
#' @import shinydashboard
#'
runMicrobiomeExplorerApp <- function() {
  appDir <- system.file("shiny-scripts", package = "MicrobiomeExplorer")
  shiny::runApp(appDir, display.mode = "normal")
  invisible(NULL)  # Explicitly state no return value
}
