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
#' @param host Character string. The IP address to bind to. Use
#'   \code{"0.0.0.0"} for Docker containers. Default: \code{"127.0.0.1"}.
#' @param port Integer or \code{NULL}. Port number. \code{NULL} lets Shiny
#'   choose a random available port. Default: \code{NULL}.
#' @return This function does not return a value, but it opens a Shiny application
#' in the user's default web browser.
#'
#' @examples
#' \donttest{
#'   MicrobiomeExplorer::runMicrobiomeExplorerApp()
#'   MicrobiomeExplorer::runMicrobiomeExplorerApp(host = "0.0.0.0", port = 3838)
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
runMicrobiomeExplorerApp <- function(host = "127.0.0.1", port = NULL) {
  appDir <- system.file("shiny-scripts", package = "MicrobiomeExplorer")
  tryCatch(
    shiny::runApp(appDir, display.mode = "normal", host = host, port = port),
    error = function(e) {
      if (grepl("address already in use|Failed to create server", e$message)) {
        message("Port ", port, " in use, retrying on a random port...")
        shiny::runApp(appDir, display.mode = "normal", host = host, port = NULL)
      } else {
        stop(e)
      }
    }
  )
  invisible(NULL)
}
