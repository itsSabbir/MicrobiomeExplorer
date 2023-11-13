# MicrobiomeExplorer_utilities.R
# This script sets up and maintains the 'MicrobiomeExplorer' R package,
# including creating necessary directories and handling recurring tasks.

# Load necessary libraries for package development
library(devtools)
library(usethis)
library(roxygen2)
library(testthat)

# ---------------------------
# Directory Structure Setup
# ---------------------------
# Creating necessary directories if they don't already exist

# Create a 'data' directory
if(!dir.exists("data")) {
  dir.create("data")
  message("Data directory created for MicrobiomeExplorer.")
} else {
  message("Data directory already exists for MicrobiomeExplorer.")
}

# Create an 'inst/extdata' directory
if(!dir.exists("inst/extdata")) {
  dir.create("inst/extdata", recursive = TRUE)
  message("inst/extdata directory created for MicrobiomeExplorer.")
} else {
  message("inst/extdata directory already exists for MicrobiomeExplorer.")
}

# Create a 'tests/testthat' directory
if(!dir.exists("tests/testthat")) {
  use_testthat()
  message("Test environment set up for MicrobiomeExplorer.")
} else {
  message("Test environment already exists for MicrobiomeExplorer.")
}

# Create a 'vignettes' directory
if(!dir.exists("vignettes")) {
  dir.create("vignettes")
  message("Vignettes directory created for MicrobiomeExplorer.")
} else {
  message("Vignettes directory already exists for MicrobiomeExplorer.")
}

# Create a '.github/workflows' directory
if(!dir.exists(".github/workflows")) {
  dir.create(".github/workflows", recursive = TRUE)
  message(".github/workflows directory created for MicrobiomeExplorer.")
} else {
  message(".github/workflows directory already exists for MicrobiomeExplorer.")
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
