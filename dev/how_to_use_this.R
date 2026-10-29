vpc_data <- load_vpc_data(
  sim_data = "dev/vpc_sim.csv",
  obs_data = "dev/vpc_obs.csv",
  
  sim_time = "TIME",
  sim_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  
  obs_time = "TIME",
  obs_endpoint = "DV",
  obs_subject = "ID",
  
  sim_time_is_binned = FALSE,
  nominal_times = c(0, 1, 2, 4, 8, 24),
  exclude_subjects = c("101", "105")
)

strata <- configure_strata(
  data = vpc_data,
  eligible_vars = c("DOSE"),
  show_codes = TRUE
)
# strata <- configure_strata(
#   data = vpc_data,
#   eligible_vars = c("ARM", "PART"),
#   label_sources = list(
#     ARM = "ARM_LABEL"
#   ),
#   show_codes = TRUE
# )

# strata <- configure_strata(
#   data = vpc_data,
#   eligible_vars = c("ARM"),
#   manual_labels = list(
#     ARM = "1 = Placebo\n2 = Active\n3 = Control"
#   ),
#   show_codes = TRUE
# )
strata$config
strata$eligible_vars
strata$label_maps



settings <- vpc_plot_settings(
  strat_vars = strata$eligible_vars,
  color_by = strata$eligible_vars,
  palette = "Blue",
  lower_pct = 5,
  upper_pct = 95,
  use_bootstrap = TRUE,
  B = 300,
  confidence = 0.95,
  log_scale = TRUE,
  show_points = TRUE,
  x_axis_label = "Time (hours)",
  y_axis_label = "Concentration"
)

result <- plot_vpc_strata(
  data = vpc_data,
  settings = settings
)

get_strata_display_label(
  value = 1,
  mapping = strata$label_maps$ARM,
  show_code = strata$show_codes
)


# First plot
result$plots[[1]]

# Plot names
names(result$plots)

# All grouping combinations
result$groups

ggplot2::ggsave(
  filename = paste0("dev/vpc_plot_",names(result$plots),".png"),
  plot = result$plots[[1]],
  width = 8,
  height = 6,
  dpi = 300
)



# Data from load_vpc_data()
vpc_data <- load_vpc_data(
  sim_data = "dev/vpc_sim.csv",
  obs_data = "dev/vpc_obs.csv",
  sim_time = "TIME",
  sim_endpoint = "DV",
  obs_time = "TIME",
  obs_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  obs_subject = "ID"
)

# Select the variables to summarize by
strat_vars <- c("DOSE")

# Dataset overview
overview <- summarise_vpc_data(
  data = vpc_data,
  strat_vars = strat_vars
)

# VPC percentile summary
percentile_summary <- summarise_vpc_percentiles(
  data = vpc_data,
  strat_vars = strat_vars,
  probs = c(0.05, 0.95)
)

# Inspect results
print(overview)
print(percentile_summary)


settings <- vpc_plot_settings(
  strat_vars = c("TREATMENT"),
  lower_pct = 5,
  upper_pct = 95
)

percentile_summary <- summarise_vpc_percentiles(
  data = vpc_data,
  strat_vars = settings$strat_vars,
  probs = settings$probs
)

utils::write.csv(
  overview,
  "vpc_data_overview.csv",
  row.names = FALSE
)

utils::write.csv(
  percentile_summary,
  "vpc_percentile_summary.csv",
  row.names = FALSE
)




# The observed data has no PRED column, but PRED is a population-only
# prediction (identical across all 100 REPs for a given ID + TIME in
# vpc_sim.csv), so the same subjects' PRED can be joined in from the
# simulated data (REP == 1) by ID + TIME. pvc no longer needs a
# PREDVAR column -- its variance reference is derived from the
# scatter of prediction-corrected values across simulated replicates,
# using sim_repl (see vpc_plot_settings()'s correction/pc_method docs).
sim_raw <- readr::read_csv("dev/vpc_sim.csv", show_col_types = FALSE)
obs_raw <- readr::read_csv("dev/vpc_obs.csv", show_col_types = FALSE)

pred_lookup <- unique(sim_raw[sim_raw$REP == 1, c("ID", "TIME", "PRED")])
obs_raw <- merge(obs_raw, pred_lookup, by = c("ID", "TIME"), all.x = TRUE)

devtools::load_all(quiet = TRUE)
result <- run_vpc_analysis(
  sim_data = sim_raw,
  obs_data = obs_raw,

  sim_time = "TIME",
  sim_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",

  obs_time = "TIME",
  obs_endpoint = "DV",
  obs_subject = "ID",

  # sim_pred = "PRED",
  # obs_pred = "PRED",

  strat_vars = character(0), #c("DOSE")

  nominal_times = NULL, #c(0, 1, 2, 4, 8, 24)

  # label_sources = list(
  #   TREATMENT = "TREATMENT_LABEL"
  # ),

  show_codes = TRUE,

  plot_settings = vpc_plot_settings(
    lower_pct = 5,
    upper_pct = 95,
    palette = "Blue",
    use_bootstrap = FALSE,
    point_shape = 1,
    point_color = "green",
    correction = "none",
    log_scale = FALSE
  )
)

# Display a plot
print(result$plots$plots[[1]])

# Review the overview
result$overview

# Review percentile estimates
result$percentiles

# Export summaries
write.csv(result$overview, "dev/overview.csv", row.names = FALSE)
write.csv(result$percentiles, "dev/percentiles.csv", row.names = FALSE)

# "Circle" = 16,
# "Open circle" = 1,
# "Triangle" = 17,
# "Square" = 15,
# "Plus" = 3
