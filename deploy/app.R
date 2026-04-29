# Standalone app.R for shinyapps.io deployment
# This wrapper ensures the package is installed, then launches the app.

if (!requireNamespace("MicrobiomeExplorer", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("itsSabbir/MicrobiomeExplorer")
}

MicrobiomeExplorer::runMicrobiomeExplorerApp()
