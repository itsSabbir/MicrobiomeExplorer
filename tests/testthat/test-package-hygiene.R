# Interpretation: reviewer-side R CMD check verifies generated docs; these
# tests pin namespace/package behavior that should change in this task.

test_that("deprecated compatibility aliases are absent from namespace", {
  deprecated_aliases <- c(
    "calculate_alpha_diversity",
    "calculate_stats",
    "plot_microbiome_heatmap",
    "AdvancedRarefactionPlot"
  )
  namespace <- asNamespace("MicrobiomeExplorer")

  expect_false(any(vapply(
    deprecated_aliases,
    exists,
    logical(1),
    envir = namespace,
    inherits = FALSE
  )))
  expect_false(any(deprecated_aliases %in% getNamespaceExports("MicrobiomeExplorer")))
})

test_that("package version is bumped for submission cleanup", {
  expect_equal(as.character(utils::packageVersion("MicrobiomeExplorer")), "0.3.0")
})
