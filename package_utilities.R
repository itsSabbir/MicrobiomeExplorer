# package_utilities.R
# This script contains utilities for setting up and maintaining the R package 'MicrobiomeExplorer'.
# It includes both one-time setup tasks and recurring tasks for package development.

# Load necessary libraries
library(devtools)
library(usethis)
library(roxygen2)
library(testthat)

# ---------------------------
# One-Time Setup Commands
# ---------------------------
# These commands are intended to be run once to set up the package environment.

# Create testing infrastructure (Run only once)
if(!dir.exists("tests/testthat")) {
  use_testthat()
  message("Test environment set up.")
} else {
  message("Test environment already exists.")
}

# Create a vignette template (Run only once)
vignette_name <- "my_vignette"
if(!file.exists(file.path("vignettes", paste0(vignette_name, ".Rmd")))) {
  use_vignette(vignette_name)
  message("Vignette template created.")
} else {
  message("Vignette template already exists.")
}

# Check and install missing packages (Run only once)
required_packages <- c("knitr", "rmarkdown")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  install.packages(missing_packages)
  message("Missing packages installed.")
} else {
  message("All required packages are already installed.")
}

# ---------------------------
# Recurring Tasks
# ---------------------------
# These commands are to be run regularly as you develop your package.

# Document the package: Update NAMESPACE and create/update .Rd files in 'man/'
devtools::document()

# Run tests: Execute tests in 'tests/testthat'
devtools::test()

# Check the package: Validate package structure and contents
devtools::check()

# ---------------------------
# End of Utility Script
# ---------------------------
# Remember to regularly commit changes to your Git repository.
