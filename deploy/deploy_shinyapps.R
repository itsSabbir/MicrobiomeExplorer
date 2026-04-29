# deploy/deploy_shinyapps.R
# Run interactively to deploy to shinyapps.io
#
# Prerequisites:
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name = "YOUR_ACCOUNT",
#                             token = "YOUR_TOKEN",
#                             secret = "YOUR_SECRET")

rsconnect::deployApp(
  appDir    = system.file("shiny-scripts", package = "MicrobiomeExplorer"),
  appName   = "MicrobiomeExplorer",
  appTitle  = "MicrobiomeExplorer Dashboard",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
