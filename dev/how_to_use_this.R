vpc_data <- load_vpc_data(
  sim_data = "dev/vpc_pkg_sim.csv",
  obs_data = "dev/vpc_pkg_obs.csv",
  
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
  filename = "vpc_plot.png",
  plot = result$plots[[1]],
  width = 8,
  height = 6,
  dpi = 300
)



# Data from load_vpc_data()
vpc_data <- load_vpc_data(
  sim_data = "simulated.csv",
  obs_data = "observed.csv",
  sim_time = "TIME",
  sim_endpoint = "DV",
  obs_time = "TIME",
  obs_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  obs_subject = "ID"
)

# Select the variables to summarize by
strat_vars <- c("TREATMENT")

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




result <- run_vpc_analysis(
  sim_data = "simulated.csv",
  obs_data = "observed.csv",
  
  sim_time = "TIME",
  sim_endpoint = "DV",
  sim_repl = "REP",
  sim_subject = "ID",
  
  obs_time = "TIME",
  obs_endpoint = "DV",
  obs_subject = "ID",
  
  strat_vars = c("TREATMENT"),
  
  nominal_times = c(0, 1, 2, 4, 8, 24),
  
  label_sources = list(
    TREATMENT = "TREATMENT_LABEL"
  ),
  
  show_codes = TRUE,
  
  plot_settings = vpc_plot_settings(
    lower_pct = 5,
    upper_pct = 95,
    palette = "Blue",
    use_bootstrap = FALSE
  )
)


# Display a plot
print(result$plots$plots[[1]])

# Review the overview
result$overview

# Review percentile estimates
result$percentiles

# Export summaries
write.csv(result$overview, "overview.csv", row.names = FALSE)
write.csv(result$percentiles, "percentiles.csv", row.names = FALSE)