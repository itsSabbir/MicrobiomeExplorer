# Codex Instructions — MicrobiomeExplorer

You (Codex) are the **implementer** in this repo. Claude Code (a separate agent) is the manager that planned this task and will review your work.

## Project overview

R package + Shiny app for end-to-end microbiome data analysis (16S, metagenomic, metatranscriptomic). Stack: R (>= 4.1.0), Shiny + shinydashboard, phyloseq, vegan, DESeq2/edgeR/limma, ComplexHeatmap, randomForest, igraph, plotly. Tests use testthat (edition 3); docs use roxygen2.

## Your scope

- You run inside an isolated **git worktree** (path passed via `--cd`), not the main repo.
- You implement exactly the task block passed in your prompt. The full plan lives at `../<repo>/tasks/todo.md` for context but you focus on the one task.
- You may read any file in the worktree. You may write any file in the worktree. Do not write outside it.
- You commit your work to the feature branch before exiting. You do **not** push.

## Rules (overrides global `~/.codex/AGENTS.md` where they conflict)

1. **One task per session.** Do not start a second task even if it seems related — Claude will hand it off explicitly.
2. **Tests in the same change.** Write testthat `test_that()` blocks for new functionality. Place them in `tests/testthat/test-<module>.R`. Write the test files even if you can't run them — see rule 3.
3. **Don't run install or tests; don't run git.** The bridge sandbox blocks outbound network (so `install.packages()` / `BiocManager::install()` fail) and disallows `.git` writes (so `git add` / `git commit` fail). Skip both. Just write code + tests into the worktree. The bridge stages and commits your work after you exit; the reviewer (Claude) runs tests on the resulting branch.
4. **Write `.codex-commit-msg`** in the worktree root before exiting. Single line, imperative mood, no AI labels. Example: `add UniFrac distance calculation with testthat coverage`. The bridge passes this through to `git commit -m`. The file itself is deleted before staging — it never lands in the commit.
5. **Ambiguity: write your interpretation as a code comment** at the top of your first new file, then proceed. Claude will challenge if needed.
6. **Match existing style.** Use `<-` for assignment (not `=`), roxygen2 `#'` for docs, snake_case for functions (camelCase for S4 methods where existing code uses it). Adjacent-code "improvements" are scope creep.
7. **Stop on unexpected sandbox violations.** If a write to the worktree itself fails, report and exit; do not retry with `--dangerously-bypass-approvals-and-sandbox`. (Network-blocked installs and `.git`-write-blocked commits are expected, not unexpected — rule 3 covers them.)

## R-specific conventions

- **S4 classes** — the core domain object is `MicrobiomeData` in `R/microbiomeDataClass.R`. New analysis modules should accept/return this class where appropriate.
- **Bioconductor deps** — `phyloseq`, `ComplexHeatmap`, `DESeq2`, `edgeR`, `limma` come from Bioconductor. Never add them with `install.packages()`. Gate optional deps with `requireNamespace()`.
- **Optional Suggests** — `Rtsne`, `umap`, `pROC`, `cluster`, `dbscan` are gated at runtime via `requireNamespace()`. Don't promote them to `Imports`.
- **roxygen2** — all exported functions must have `@export`, `@param`, `@return`, `@examples` (or `@noRd` for internal helpers).
- **NAMESPACE** — managed by roxygen2. Run `devtools::document()` mentally; don't hand-edit NAMESPACE.
- **Deprecated aliases** — `calculate_alpha_diversity`, `calculate_stats`, `plot_microbiome_heatmap`, `AdvancedRarefactionPlot` are scheduled for removal. Never depend on them in new code.
- **Shiny app** lives at `inst/shiny-scripts/app.R`, resolved via `system.file()`. Never reference by relative path.

## Engineering Charter — non-negotiable

These come from `~/.claude/CLAUDE.md § Engineering Charter`. Every change must satisfy all that apply. Claude scores against this list on `/codex-review`; violations are blocking.

1. **TDD** — failing test first; commit only when green.
2. **Single-responsibility** — functions ≤ 40 LOC, ≤ 1 concern. Split before approaching the limit.
3. **Structured returns** — named lists with `ok`, `reason`, `payload` fields, not bare logicals, when callers branch on outcome.
4. **Observability** — `message("[tag] ...")` at every decision point (tag lowercase, ≤ 12 chars).
5. **Backward compatibility** — no breaking changes without a deprecation path.
6. **Per-feature env-var rollback** — new behavior should be flag-gateable via `getOption()` or `Sys.getenv()`.
7. **No silent catches** — every `tryCatch` either logs with context or rethrows with added context.
8. **Comments explain WHY** — one line, max two. Never `# set the delay`.
9. **Idempotency** — operations safe to call twice.
10. **Cross-platform reuse** — platform-specific code only when truly platform-specific.

## Anti-patterns to avoid

These are blocking on review. If a change introduces any of them without explicit justification in the commit message, Claude will bounce it back.

- **Code duplication** — before writing a new util/class/parser, grep the repo for an existing one. Fork != reuse. If your task block lists "Reuse existing:" entries, treat them as load-bearing.
- **Premature abstraction** — no S4 generics/classes with one implementation. Build the second use case before extracting the abstraction.
- **Files > 300 LOC** — split into focused modules. If your change pushes a file past 300, factor before commit.
- **Functions with > 3 params** — use a named list or `...` for options.
- **Magic numbers / unclear identifiers** — `MAX_RETRIES <- 5L` with a WHY comment, not `if (retries < 5)`.
- **Defensive validation for impossible internal states** — validate at function entry; don't wrap deterministic internal calls in speculative `tryCatch`. Boundary-only.
- **Re-implementation of base R or well-known package primitives** — use `vapply`, `match.arg`, `tryCatch`, etc. Don't roll your own.
- **Diff churn unrelated to the task** — no rename sprees, no reformatting adjacent code, no "while I'm here" cleanups. If you notice dead code, flag it; don't delete it unless the task says to.
- **Mock-heavy tests** — prefer tests against real (stubbed-at-the-boundary) dependencies over mocks. Mocks that drift from real behavior produce false confidence.
- **Bare logicals for state callers branch on** — same root cause as charter rule 3.
- **Wide pattern catches** (`tryCatch(..., error = function(e) NULL)`) — narrow to specific conditions; let the rest bubble.

## What Claude will check on review (in addition to charter + anti-patterns)

- Acceptance criteria from the task block all met (each item checked).
- Tests assert real behavior — both pass AND fail paths covered for each new function/behavior.
- No dead code, no commented-out blocks (git history is the archive).
- Diff scope matches the task — no surprise files, no unrelated cleanup.
- No secrets, no hardcoded URLs pointing at prod.
- Reuse audit: if a similar utility existed and wasn't used, that's a violation.
- Commit message accurate, imperative, no AI labels (`claude`, `codex`, `ai`, `assistant`).

If you skip any of these expect a bounce-back.
