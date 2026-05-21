# Tasks — MicrobiomeExplorer

> **Goal:** Make the package scientifically correct, robustly tested, and publication-ready.

---

## Task 1 — Fix EdgeR log2 pre-transform bug in DE analysis

**Status:** ✅ shipped 2026-05-20 — commit dda7722

---

## Task 2 — Fix biomarker discovery: add FDR correction and fix silent catch

**Status:** ✅ shipped 2026-05-20 — commit 06d9108

**Context:**
`discoverBiomarkers()` has two scientific issues: (1) Kruskal-Wallis p-values across all taxa are compared against `pval_threshold` without any multiple-testing correction (BH/Bonferroni), producing a high false-positive rate for datasets with hundreds of taxa. (2) `tryCatch(..., error = function(e) 1.0)` silently swallows errors. Additionally, the `lda_threshold` parameter name is misleading.

**Acceptance:**
1. After computing all per-taxon Kruskal-Wallis p-values, apply `stats::p.adjust(method = "BH")` and store as `padj`. Filtering uses `padj < pval_threshold`.
2. The returned data.frames include both `pvalue` and `padj` columns.
3. The `tryCatch` logs `message("[biomark] KW failed for taxon '...': <error>")` instead of silently returning 1.0.
4. Add `effect_size_threshold` as new parameter name (aliased from `lda_threshold` with deprecation warning).
5. **Tests** — new `tests/testthat/test-discoverBiomarkers.R`: padj present and >= pvalue, silent catch now logs, lda_threshold deprecation, structure check.

**Files to touch:**
- `R/discoverBiomarkers.R` (modified)
- `tests/testthat/test-discoverBiomarkers.R` (new)

---

## Task 3 — Rewrite rarefaction plot as true subsampled rarefaction curves

**Status:** ✅ shipped 2026-05-20 — commit 982b7dc

**Context:**
`advancedRarefactionPlot()` does NOT produce rarefaction curves. It plots diversity indices on a categorical x-axis. A real rarefaction curve plots observed species richness (y) vs. sampling depth (x) using `vegan::rarefy` at multiple depth levels.

**Acceptance:**
1. Function uses `vegan::rarefy(sample_counts, sample_size)` at evenly-spaced depths to compute expected richness.
2. ggplot: x = "Sequencing Depth", y = "Expected Species Richness", one line per sample.
3. Vertical dashed line at minimum library size.
4. Uses `calculateAlphaDiversity` (not deprecated alias).
5. **Per-sample terminal depth:** Each sample's curve MUST include its own library size as a depth point, even when using coarse `step` or `n_steps`. Without this, small-library samples get truncated (e.g., a sample with 8 reads and `step = 10` would only plot depth=1). Implementation: in the per-sample curve function, append `sample_total` to the depth vector before filtering: `sort(unique(c(depths[depths <= sample_total], sample_total)))`.
6. **No silent swallowing of legacy args:** Do NOT use `...` to absorb old parameters (`indices`, `save_plot`, `xlab`, etc.). Either omit `...` entirely or check for unexpected named args and warn.
7. **Tests** — returns ggplot, x-axis range correct, monoculture = flat line at 1, error on non-numeric, AND a test with uneven library sizes (e.g., samples of 8 and 100 reads) confirming both curves reach their own library size as terminal depth.

**Files to touch:**
- `R/advancedRarefactionPlot.R` (rewritten)
- `tests/testthat/test-AdvancedRarefactionPlot.R` (expanded)

---

## Task 4 — Add stratified CV to classification and ML test coverage

**Status:** ✅ shipped 2026-05-21 — commit 9d1c682

**Context:**
`performClassification()` fold assignment is NOT stratified — can produce single-class folds for imbalanced data. Also, `performClassification` and `performRandomForest` have zero test coverage.

**Acceptance:**
1. Stratified fold assignment using `split(seq_len(n), labels)`.
2. Guard check warns if any fold has <2 samples from any class.
3. **Tests** — `test-performClassification.R`: structure, accuracy bounds, separable data, imbalanced classes, error on <6 samples.
4. **Tests** — `test-performRandomForest.R`: structure, importance shape, error on mismatched rownames, valid accuracy.

**Files to touch:**
- `R/performClassification.R` (modified)
- `tests/testthat/test-performClassification.R` (new)
- `tests/testthat/test-performRandomForest.R` (new)

---

## Task 5 — Fix clustering metric mismatch and add test coverage

**Status:** ✅ shipped 2026-05-21 — commit f2bb84e

**Context:**
`performClustering()` uses `scale(data)` (Euclidean) for k-means but computes silhouette against Bray-Curtis `dist_mat` — metric mismatch. Also zero test coverage for clustering and network.

**Acceptance:**
1. k-means silhouette computed against `dist(input)` (Euclidean), not Bray-Curtis.
2. Returned list includes `dist_method_used` field.
3. Auto-k selection uses same distance metric as final silhouette.
4. **Tests** — `test-performClustering.R`: structure, well-separated clusters, hierarchical, DBSCAN, error on <4.
5. **Tests** — `test-buildCooccurrenceNetwork.R`: structure, correlated pair edge, threshold filtering, error on non-numeric.

**Files to touch:**
- `R/performClustering.R` (modified)
- `tests/testthat/test-performClustering.R` (new)
- `tests/testthat/test-buildCooccurrenceNetwork.R` (new)

---

## Task 6 — Add rank-abundance and shared-taxa Venn visualizations

**Status:** ✅ shipped 2026-05-21 — commit 06d1116

**Context:**
Missing standard microbiome visualizations: rank-abundance (Whittaker) plots and Venn/UpSet-style shared taxa diagrams.

**Acceptance:**
1. `plotRankAbundance(data, top_n, log_scale)` — ggplot of species rank vs relative abundance.
2. `plotSharedTaxa(data, sample_info, group_var, min_prevalence)` — ggplot of shared/unique taxa between groups.
3. Both return ggplot, have complete roxygen2 docs.
4. **Tests** for both functions.

**Files to touch:**
- `R/plotRankAbundance.R` (new)
- `R/plotSharedTaxa.R` (new)
- `tests/testthat/test-plotRankAbundance.R` (new)
- `tests/testthat/test-plotSharedTaxa.R` (new)

---

## Task 7 — Wire UniFrac distance into beta diversity pipeline

**Status:** ✅ shipped 2026-05-21 — commit 9a102db

**Context:**
`phyloseq` is imported but never used. UniFrac (weighted/unweighted) is the #1 expected 16S beta diversity metric.

**Acceptance:**
1. `MicrobiomeData` class gains `PhylogeneticTree` slot.
2. `calculateBetaDiversity()` gains `"unifrac"` and `"wunifrac"` methods using `phyloseq::UniFrac()`.
3. Error with clear message when UniFrac selected without tree.
4. Existing methods unchanged (backward compatible).
5. **Tests** — UniFrac without tree errors, with tree returns dist, weighted differs from unweighted, Bray-Curtis regression.

**Files to touch:**
- `R/microbiomeDataClass.R` (modified)
- `R/calculateBetaDiversity.R` (modified)
- `tests/testthat/test-calculateBetaDiversity.R` (modified)

---

## Task 8 — Package hygiene for CRAN/Bioconductor submission

**Status:** pending
**Blocked by:** Tasks 1-7

**Context:**
Final cleanup: remove deprecated aliases, `\dontrun` → `\donttest`, `import()` → `importFrom()`, version bump to 0.3.0, update KNOWN_ISSUES.md.

**Acceptance:**
1. Remove deprecated wrappers from `R/deprecated.R`.
2. Replace `\dontrun{}` with `\donttest{}` where appropriate.
3. Replace `import(edgeR)` / `import(limma)` with specific `importFrom()`.
4. Version bump to 0.3.0, update KNOWN_ISSUES.md.
5. `R CMD check .` — 0 errors, 0 warnings (3 expected notes OK).

**Files to touch:**
- `R/deprecated.R`, `R/performDEAnalysis.R`, `DESCRIPTION`, `KNOWN_ISSUES.md`, multiple `R/*.R` (examples)

---

## Task 9 — Fix vignette for rewritten rarefaction API

**Status:** pending

**Context:**
CI fails on `R CMD check` because the vignette `introduction_MicrobiomeExplorer.Rmd` at line 349-353 calls `advancedRarefactionPlot(microbiome_example, indices = c("Shannon", "Simpson"))`. Two problems: (1) `microbiome_example` contains decimal counts but `vegan::rarefy()` requires integers — the rewritten function now validates and rejects non-integer data. (2) The `indices` parameter no longer exists in the rewritten function signature (Task 3 replaced it with `step`/`n_steps`). The CI run `26248729033` shows: `Error in vegan::rarefy(): function accepts only integers (counts)`.

**Acceptance:**
1. The vignette chunk `plot-rarefaction` (line 349-353) calls `advancedRarefactionPlot()` with the new API (no `indices` parameter) and integer count data. Use `sampleDataset$counts` (which has integer counts) or `round(microbiome_example)` — whichever better fits the vignette's narrative.
2. The surrounding prose (lines 340-359) is updated to describe actual rarefaction curves (species richness vs. sequencing depth), not the old diversity-index plots.
3. `R CMD check .` passes vignette building without error (the `creating vignettes ... ERROR` from run `26248729033` is resolved).
4. **Tests** — no new unit tests needed; CI's `R CMD check` is the acceptance gate.

**Files to touch:**
- `vignettes/introduction_MicrobiomeExplorer.Rmd` (modified — update chunk + prose)

**Reuse existing (do not duplicate):**
- `sampleDataset` — bundled dataset with integer counts, already used elsewhere in the vignette.
- The new `advancedRarefactionPlot(data, step, n_steps)` API from Task 3.

**Out of scope (defer to future tasks):**
- Rewriting the entire vignette for v0.3.0 features
- Adding vignette sections for new functions (plotRankAbundance, plotSharedTaxa, UniFrac)

**Why now / dependencies:**
- Blocks: nothing, but CI is red — this is the sole cause of the R CMD check failure
- Blocked by: none (Task 3 already shipped)
- Why this slice: single-file fix for the only CI failure
