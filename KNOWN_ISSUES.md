# Known Issues & Limitations

> Last updated: 2026-04-29 | Package version: 0.2.0

---

## Platform

| Issue | Severity | Workaround |
|-------|----------|------------|
| R crashes (segfault) when launched from Git Bash / MSYS2 on Windows | High | Use RStudio, PowerShell, or `cmd.exe` instead |
| Port conflicts when relaunching Shiny app quickly | Low | `runMicrobiomeExplorerApp()` auto-retries on a random port |

## R CMD Check Notes

These are non-blocking notes, not errors:

- **`shinyWidgets` imported but not used in R/ code** — it's used in the Shiny app UI (`inst/shiny-scripts/app.R`) which R CMD check doesn't scan for `importFrom` usage.
- **`LICENSE.md` non-standard file** — present alongside `LICENSE` for GitHub display.
- **Global variable `Sample`** — created dynamically by `pivot_longer()`; suppressed via `globalVariables("Sample")`.

## Shiny App

### Data Requirements

- **Row names are sample IDs**: both count table and metadata must use matching row names. Mismatched IDs trigger a warning on upload but silently drop unmatched samples during analysis.
- **No NA values in count data**: validation functions reject count tables with missing values. Impute or remove before uploading.
- **Minimum sample sizes**: t-SNE/UMAP require 4+ samples, random forest requires 6+, clustering requires 4+.

### Analysis Limitations

| Tab | Limitation |
|-----|-----------|
| Differential Abundance | Requires exactly 2 groups. Multi-group comparisons not supported. |
| ML Classification | ROC/AUC only computed for binary classification. Multi-class returns accuracy only. |
| Biomarkers | Effect size threshold (default 0.5) may need tuning for datasets with subtle group differences. |
| Network Analysis | Datasets with >200 taxa produce cluttered network plots. Use the "Top Taxa" filter to limit. |
| Heatmap | ComplexHeatmap objects use a different export path than ggplot — PDF/PNG export handles this automatically. |
| Alpha Diversity | "Index to Visualise" dropdown is empty until you click "Calculate & Plot" at least once. |

### Memory

- ComplexHeatmap + randomForest on large datasets (>500 taxa, >100 samples) can exceed 1GB RAM.
- shinyapps.io free tier (1GB) may fail for large datasets. Standard plan recommended for production.
- All heavy packages (DESeq2, edgeR, ComplexHeatmap) load eagerly on app startup.

## Optional Dependencies

These packages are in `Suggests` and checked at runtime with `requireNamespace()`. Functions degrade gracefully if missing:

| Package | Used By | What Happens If Missing |
|---------|---------|------------------------|
| `Rtsne` | `performDimReduction(method = "tSNE")` | Error with install instructions |
| `umap` | `performDimReduction(method = "UMAP")` | Error with install instructions |
| `pROC` | `performClassification()` | ROC/AUC skipped, accuracy still returned |
| `cluster` | `performClustering()` | Silhouette scores unavailable, clustering still works |
| `dbscan` | `performClustering(method = "dbscan")` | Error with install instructions |

## Deprecated Function Names

The following old names still work but emit deprecation warnings. They will be removed in a future version:

| Deprecated | Use Instead |
|-----------|-------------|
| `calculate_alpha_diversity()` | `calculateAlphaDiversity()` |
| `calculate_stats()` | `calculateStats()` |
| `plot_microbiome_heatmap()` | `plotMicrobiomeHeatmap()` |
| `AdvancedRarefactionPlot()` | `advancedRarefactionPlot()` |

## Not Yet Implemented

These features are on the roadmap but not in v0.2.0:

- **Phylogenetic tree support** — `phyloseq` is imported but UniFrac and tree-based analyses are not wired up.
- **Batch effect correction** — no ComBat/SVA integration.
- **Longitudinal / paired-sample analysis** — all analyses assume independent samples.
- **Multi-group differential abundance** — DE tab only supports 2-group comparisons (DESeq2/edgeR limitation in our wrapper).
- **Automatic rarefaction depth selection** — users must specify depth manually.
- **t-SNE/UMAP in Beta Diversity tab** — currently only available as standalone functions, not integrated into the ordination tab dropdown.

## Reporting Issues

File bugs at: https://github.com/itsSabbir/MicrobiomeExplorer/issues

Include: R version (`sessionInfo()`), OS, input data dimensions, and the full error message.
