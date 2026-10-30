# pmxvpc

Visual Predictive Check (VPC) plots for pharmacometric (PK/PD) model
diagnostics. Compares simulated vs. observed concentration-time data,
with optional prediction-corrected (pcVPC) and prediction- and
variance-corrected (pvcVPC) rescaling, stratification, time binning,
and summary tables. Consumed by an R Shiny app.

## Installation

```r
# From the package source directory
devtools::install()
```

## Quick start

```r
library(pmxvpc)

# sim columns: ID, REP, TIME, DV, IPRED, PRED, AMT, DOSE, ISM, MDV
# obs columns: ID, TIME, DV, AMT, DOSE, MDV, ISM
data <- load_vpc_data(
  sim_data = "sim.csv",
  obs_data = "obs.csv",
  sim_time = "TIME",
  sim_endpoint = "DV",
  obs_time = "TIME",
  obs_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  obs_subject = "ID",
  nominal_times = c(0, 1, 2, 4, 8, 24, 48, 72, 96, 120, 144, 168)
)

strata <- configure_strata(
  data = data,
  eligible_vars = c("DOSE")
)

settings <- vpc_plot_settings(
  strat_vars = c("DOSE"),
  log_scale = TRUE
)

plots <- plot_vpc_strata(data, settings)

plots$plots[[1]]
```

`plots` is a `"vpc_plot_collection"`: a named list of per-stratum
`ggplot` objects (`plots$plots`) plus the settings and stratum
groupings used to build them.

## The pipeline

```
load_vpc_data() -> configure_strata() -> vpc_plot_settings() -> plot_vpc_strata()
                                                                       |
                                                                       v
                                                          combine_vpc_plots() (optional)
```

Plus, independent of plotting:

```
summarise_vpc_data()          # dataset overview (counts, descriptive stats)
summarise_vpc_percentiles()   # simulated vs. observed percentiles by stratum/time
```

`run_vpc_analysis()` chains all of the above into a single call and
also accepts `*_save_path` arguments to write CSVs/plots directly:

```r
result <- run_vpc_analysis(
  sim_data = "sim.csv",
  obs_data = "obs.csv",
  sim_time = "TIME",
  sim_endpoint = "DV",
  obs_time = "TIME",
  obs_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  obs_subject = "ID",
  nominal_times = c(0, 1, 2, 4, 8, 24, 48, 72, 96, 120, 144, 168),
  strat_vars = c("DOSE"),
  overview_save_path = "overview.csv",
  percentiles_save_path = "percentiles.csv",
  combined_plot_save_path = "combined_vpc.png"
)

result$plots         # vpc_plot_collection
result$overview       # data frame
result$percentiles    # data frame
result$combined_plot  # combined plot (also saved to combined_plot_save_path)
```

## Prediction and variance correction (pcVPC / pvcVPC)

A plain VPC compares observed vs. simulated percentiles over time. If
subjects received different doses or had different covariates, their
raw concentrations aren't directly comparable across a bin — a
**prediction-corrected** VPC (`correction = "pc"`) rescales every
record to what it would look like at the bin's typical prediction
before comparing.

pc doesn't address a separate issue: the *spread* of values within a
bin can itself be wider or narrower than typical, for reasons
unrelated to structural model fit (e.g. one simulated replicate's
random draw happens to be tighter or wider than another purely by
chance). A **prediction- and variance-corrected** VPC
(`correction = "pvc"`) additionally rescales each record's deviation
by the ratio of the bin's typical spread to that record's own local
spread, isolating genuine model misfit from expected
replicate-to-replicate variability. Where pc and pvc plots look
similar, that extra correction isn't changing much; where they
diverge, some of what the plain/pc VPC showed was variability noise
rather than misfit.

```r
settings <- vpc_plot_settings(
  strat_vars = c("DOSE"),
  correction = "pvc",       # "none" (default), "pc", or "pvc"
  pc_method = "proportional", # or "additive"
  reference_source = "obs_only" # or "pooled"
)
```

- `correction = "pc"` requires `sim_pred`/`obs_pred` to be mapped in
  `load_vpc_data()`.
- `correction = "pvc"` additionally requires `sim_repl` to be mapped —
  its variance reference comes from the scatter of prediction-corrected
  values across simulated replicates, not a separate predicted-variance
  column.

See `?vpc_plot_settings` for the full mathematical definition, and
Bergstrand et al. (2011), *AAPS J* 13(2):143-151,
[doi:10.1208/s12248-011-9255-z](https://doi.org/10.1208/s12248-011-9255-z),
for the original pcVPC method.

## Time binning

`load_vpc_data(time_binning = ...)` supports four methods:

- `"none"` — use raw time values, unbinned.
- `"explicit_center"` (default when `nominal_times` is supplied) —
  nearest-neighbor assignment to a fixed `nominal_times` vector.
- `"explicit_bound"` — group by `time_bin_breaks` interval boundaries;
  the bin value is the interval midpoint.
- `"kmeans"` — automatically derive `n_time_bins` bin centers via
  1-D k-means (seeded for reproducibility).

## Combining stratified plots

```r
combined <- combine_vpc_plots(
  plots,
  plots_per_panel = 4,   # NULL (default) puts every panel on one plot
  legend_position = "bottom",
  save_path = "combined_vpc.png"
)
```

Arranges every panel in a `vpc_plot_collection` into one combined
image via `patchwork`. When there are more panels than
`plots_per_panel` allows, panels are split across multiple pages,
each saved with `_page<N>` inserted before the file extension.

## Summary tables

```r
overview <- summarise_vpc_data(
  data,
  strat_vars = c("DOSE"),
  save_path = "overview.csv"
)

percentiles <- summarise_vpc_percentiles(
  data,
  strat_vars = c("DOSE"),
  probs = c(0.05, 0.95),
  save_path = "percentiles.csv"
)
```

Both accept the same `correction`/`pc_method`/`reference_source`
options as `vpc_plot_settings()`, and an optional `save_path` to
write the result to CSV.

## Development

See [CLAUDE.md](CLAUDE.md) for commands, testing approach, and
architecture notes.
