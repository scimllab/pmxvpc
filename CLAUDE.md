# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`pmxvpc` is an R package that generates Visual Predictive Check (VPC) plots for pharmacometric (PK/PD) model diagnostics — comparing simulated vs. observed concentration-time data, with optional prediction (pcVPC) and prediction- and variance-corrected (pvcVPC) rescaling. It's consumed by an R Shiny app (this repo lives at `Rshinyapp/pmxvpc`).

## Commands

R is installed but **not on PATH** in this environment. Invoke it directly, e.g.:

```bash
"/c/Program Files/R/R-4.6.1/bin/Rscript.exe" -e 'devtools::document()'
"/c/Program Files/R/R-4.6.1/bin/Rscript.exe" -e 'devtools::load_all(quiet = TRUE)'
```

(There may be multiple R versions installed under `/c/Program Files/R/`; prefer the newest.)

- **Regenerate NAMESPACE and `man/*.Rd`** after any roxygen comment change: `devtools::document()`. Always run this — do not hand-edit `man/*.Rd` or `NAMESPACE` except as a last resort (see Gotchas below).
- **Load the package for interactive testing**: `devtools::load_all(quiet = TRUE)`.
- **There is no `testthat` suite.** Verification happens via standalone scripts in `dev/` (see below) — run them directly with `Rscript`, not via `devtools::test()`.
- Running a quoted inline `Rscript -e '<multi-line script>'` has segfaulted repeatedly in this environment (likely a quoting/encoding issue going through the bash tool) — **always write the script to a file and run `Rscript path/to/script.R`** instead.

## Testing approach

There is no formal test suite. Verification is done through persistent, re-runnable scripts in `dev/` (gitignored, but some are already tracked from before the ignore rule was added — see Gotchas), each self-contained and runnable with `Rscript dev/<name>.R`:

- `dev/test_pc_pvc_formulas.R` — hand-traceable unit checks for the pc/pvc correction math, directly against `vpc_correction_reference()`/`apply_vpc_correction()`.
- `dev/test_real_data_full.R` — full-pipeline integration tests against the real dataset (`dev/vpc_sim.csv`/`vpc_obs.csv`), covering every combination of `correction` × `pc_method` × `time_binning`.
- `dev/test_combine_and_save.R` — tests for `combine_vpc_plots()` and the `save_path` options.
- `dev/generate_scenario_plots.R` — not a test; regenerates one VPC PNG per scenario for visual inspection.

Each script uses a tiny local `check(label, ok)` helper that prints `[PASS]`/`[FAIL]` per assertion and a final `ALL CHECKS PASSED` / `SOME CHECKS FAILED` summary — grep for `FAIL|ALL CHECKS|SOME CHECKS` to get a quick pass/fail readout. When adding a new feature, extend the most relevant existing script rather than creating a new one unless the feature is substantial enough to warrant its own file. **Clean up any PNG/CSV files a test script writes** (`rm -f dev/*.png` etc.) before finishing — these are gitignored but clutter the directory.

`dev/vpc_sim.csv` / `dev/vpc_obs.csv` are real pharmacometric data (50 subjects, 100 simulated replicates, 12 nominal time points) but are gitignored and not present in a fresh worktree — copy them in from the main checkout (`dev/` in the repo root) if missing. The simulated data has a `PRED` column; the observed data does **not** — `PRED` is a population-only prediction (verified identical across all 100 replicates for a given `ID`+`TIME`), so it can be legitimately joined onto the observed data from the simulated data's `REP == 1` rows when a test needs a `PRED`-bearing observed dataset.

## Architecture

### Pipeline

`load_vpc_data()` → `configure_strata()` → `vpc_plot_settings()` → `plot_vpc_strata()` (→ `combine_vpc_plots()`), plus `summarise_vpc_data()` / `summarise_vpc_percentiles()` for tables. `run_vpc_analysis()` is the all-in-one orchestrator that chains all of the above and also accepts `*_save_path` arguments to write CSVs/plots directly.

Data flows through the pipeline as a `list(sim = <data.frame>, obs = <data.frame>)`. Internal standardized columns are prefixed `.vpc_` (`.vpc_time`, `.vpc_endpoint`, `.vpc_time_bin`, `.vpc_subject`, `.vpc_repl`, `.vpc_pred`) and coexist with the original source columns (which are preserved for stratification). `map_vpc_columns()` (`R/vpc_data_processing.R`) is what creates these from user-specified column-name arguments; `available_strata()` excludes the `.vpc_*` columns from stratification candidates.

### pc/pvc correction (`vpc_correction_reference()` / `apply_vpc_correction()` in `R/vpc_data_processing.R`)

This is the most intricate part of the codebase and went through several design iterations — read the function-level comments in `vpc_data_processing.R` before changing it, they document the current (final) formula precisely.

- `pc_method` ("proportional" or "additive") controls how the prediction-corrected value `pcY` is computed from `bin_pred` and each record's own `PRED`.
- `reference_source` ("obs_only" default, or "pooled") controls which data `bin_pred` (the per-bin typical prediction) is derived from. It does **not** affect the pvc variance reference.
- pvc's variance reference has no predicted-variance column input at all — it's derived entirely from the scatter of `pcY` across simulated replicates: a simulated record's denominator is its own replicate's variance (that replicate's `pcY` pooled with all observed `pcY` in the bin); the shared numerator `bin_var` is the median of those per-replicate variances across all replicates in the bin. An observed record has no replicate of its own, so its denominator is the single value mathematically equivalent to correcting it once per replicate and taking the median of the results (this collapsing is exact, not an approximation — verified in `dev/test_pc_pvc_formulas.R`). Because of this, `pvc` requires `sim_repl` to be mapped; `pc` does not.
- A named-but-missing prediction column (`sim_pred`/`obs_pred`) is silently ignored at load time unless `correction` actually requests `pc`/`pvc` — `load_vpc_data()` has no visibility into what correction will later be requested, so strict validation is deferred to `vpc_correction_reference()`, which raises a correction-specific error only when the column is actually needed.

### Time binning (`assign_vpc_time_bins()` in `R/vpc_data_processing.R`, exposed via `load_vpc_data(time_binning = ...)`)

Four methods: `"none"` (raw time, unbinned), `"explicit_center"` (nearest-neighbor to a fixed `nominal_times` vector — the original/default behavior), `"explicit_bound"` (group by `time_bin_breaks`, bin value = interval midpoint), `"kmeans"` (auto-derive `n_time_bins` centers via k-means). `time_binning = NULL` (the default) auto-selects `"explicit_center"` or `"none"` based on whether `nominal_times` was supplied, preserving old call sites' behavior unchanged.

For `"kmeans"`, bin centers are derived **once** from the pooled sim+obs time values (not independently per dataset) so both datasets snap to identical bins — computing them separately would silently break the sim/obs join downstream. `kmeans()` is also seeded (`set.seed(123)`) since it is otherwise non-reproducible run-to-run even with `nstart > 1`.

### Plot combining (`combine_vpc_plots()` in `R/combine_vpc_plots.R`)

Uses `patchwork::wrap_plots()` to arrange a `vpc_plot_collection`'s per-stratum panels into one image. Each VPC panel already sets its own `legend.position = "bottom"` theme (in `make_vpc_plot()`, `R/vpc_plot.R`), but `patchwork` does not inherit that for the *collected* legend — it must be set explicitly via `& ggplot2::theme(legend.position = ...)` on the combined object, which is what `legend_position` (default `"bottom"`) does.

`plots_per_panel` paginates: `NULL` (default) returns a single combined plot object, exactly as before pagination existed. Once set, the function **always** returns a list of combined plots (even if only one page results) for return-type stability. Multi-page saves get `_page<N>` inserted before the file extension.

### Validation style

Exported functions validate every argument up front with explicit type/shape checks and `stop(..., call. = FALSE)` before doing any work; internal helpers are comparatively lax. Settings-building functions (`vpc_plot_settings()`) return a plain validated list, not an S4/R6 object — downstream functions read off it by name and apply their own `NULL` → default fallback (e.g. `if (is.null(plot_settings$correction)) "none" else plot_settings$correction`) since a user-constructed `plot_settings` list may not have every field.

## Gotchas

- **CRLF line endings with embedded non-breaking spaces (U+00A0).** Several `R/*.R` and `man/*.Rd` files have CRLF line endings, and a few specific lines have a non-breaking space in the indentation (likely from a copy-paste at some point) instead of a regular space. The `Edit` tool's exact-string matching will silently fail against these lines — `grep`/`cat -A` for `M-BM-` to spot them, and anchor edits on adjacent *clean* lines instead of lines containing one. If you must rewrite such a line via a shell pipeline, the pipeline will likely strip the CRLF entirely (converting the whole file to LF) — restore it afterward with `sed -i 's/\r$//; s/$/\r/'` and check whether the original file had a trailing newline at EOF (many don't — compare `tail -c 10 file | xxd` against an untouched sibling file before and after).
- **`dev/` is gitignored but not empty of tracked files.** A `git rm`-less gitignore rule was added after `dev/how_to_use_this.R` was already tracked, so it remains tracked and shows up in `git status`/`git diff` normally — but `git add dev/<file>` on it requires `git add -f` because git's ignore-check still fires for new staged hunks under an ignored directory, even on an already-tracked path. Everything else added under `dev/` since then (test scripts, CSVs, generated PNGs) is untracked and ignored.
- **`dev/how_to_use_this.R` is the maintainer's own scratch/exploration file.** It gets edited directly by the human between sessions (commented-out parameters, changed `REP` values, etc.) — treat uncommitted changes there as intentional, not something to revert, unless explicitly asked to.
