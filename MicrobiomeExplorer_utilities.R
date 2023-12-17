# MicrobiomeExplorer_utilities.R
# This script sets up and maintains the 'MicrobiomeExplorer' R package,
# including creating necessary directories and handling recurring tasks.

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
