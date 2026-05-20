# MicrobiomeExplorer

> **Quick start** — you don't have to remember commands.
> Type `/menu` for a context-aware status board + relevant commands, or just describe what you want in plain English and Claude will route you. Common moves: "let's build X" → drafts a task; "do task N" → picks bridge or solo; "ship it" → verifies + merges. Override anytime with `/solo N` (Claude implements) or `/handoff N` (Codex implements).

R package + Shiny app for end-to-end microbiome data analysis (16S rRNA, metagenomic, metatranscriptomic): preprocessing, statistical analysis, ML classification, biomarker discovery, co-occurrence networks, and visualization.

## Roles

| Agent | Role | Trigger |
|-------|------|---------|
| **Claude Code** | Planner, reviewer, shipper. Plans tasks in `tasks/todo.md`, reviews Codex output, runs tests, merges to main. | You (the user) talking in this terminal |
| **Codex CLI** | Implementer. Receives a task block + prompt, writes code + tests in an isolated worktree, exits. | `/handoff N` → `scripts/bridge.ps1` |

## Stack

- **Language:** R (>= 4.1.0)
- **Framework:** Shiny + shinydashboard; phyloseq, vegan, DESeq2/edgeR/limma, ComplexHeatmap, randomForest, igraph, plotly
- **Test:** testthat (edition 3) — `Rscript -e "devtools::test()"`
- **Docs:** roxygen2
- **Deploy:** shinyapps.io via `deploy/deploy_shinyapps.R` + Docker (`Dockerfile`)

## Key entry points

- `R/runMicrobiomeExplorer.R` — launches the Shiny app via `runMicrobiomeExplorerApp()`
- `inst/shiny-scripts/app.R` — bundled Shiny app source (UI + server)
- `R/microbiomeDataClass.R` — S4 `MicrobiomeData` class (core domain object)
- `deploy/app.R` + `deploy/deploy_shinyapps.R` — shinyapps.io deployment
- `DESCRIPTION` / `NAMESPACE` — R package metadata and exports

## Bridge workflow files

- `scripts/bridge.ps1` — hands a task to Codex in an isolated worktree
- `scripts/verify-codex.ps1` — preflight check (codex, git, R available)
- `scripts/cleanup-worktrees.ps1` — remove stale task worktrees
- `tasks/todo.md` — task specs with acceptance criteria
- `tasks/lessons.md` — accumulated mistake-prevention rules
- `tasks/handoff-log.jsonl` — structured telemetry from each handoff
- `AGENTS.md` — Codex-facing rules (engineering charter, anti-patterns, R conventions)

## Non-obvious constraints

- **Bioconductor-only deps** — `phyloseq`, `ComplexHeatmap`, `DESeq2`, `edgeR`, `limma` install via `BiocManager::install(...)`, not `install.packages()`.
- **Optional Suggests gated at runtime** — `Rtsne`, `umap`, `pROC`, `cluster`, `dbscan` use `requireNamespace()`. Don't promote to Imports.
- **Deprecated aliases still exported** — `calculate_alpha_diversity`, `calculate_stats`, `plot_microbiome_heatmap`, `AdvancedRarefactionPlot`. Scheduled for removal; don't depend on them.
- **Sample-ID matching by row names only** — mismatched IDs warn on upload, then silently drop.
- **Port retry built in** — `runMicrobiomeExplorerApp()` handles "address already in use". Don't add another retry layer.
- **R CMD check has 3 expected notes** — `shinyWidgets` unused (not scanned in `inst/`), `LICENSE.md` non-standard, `Sample` global. Don't "fix" these.
- **Windows quirk** — Git Bash / MSYS2 segfaults. Use RStudio, PowerShell, or cmd.exe.
- **`KNOWN_ISSUES.md` is canonical** — consult before "fixing" intentional behavior.

## How to run / test

- Dev: `devtools::load_all(".")` then `runMicrobiomeExplorerApp()`
- Test: `Rscript -e "devtools::test()"` (or `R CMD check .` for full package check)
- Build: `devtools::build()` for source tarball; `docker build -t microbiome-explorer .` for container
- Verify bridge: `pwsh -File scripts/verify-codex.ps1`
