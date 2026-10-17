#' Run a complete VPC analysis
#'
#' Prepare data, configure stratification, generate VPC plots,
#' and calculate data overview and percentile summaries.
#'
#' @param sim_data Simulated data frame or CSV path.
#' @param obs_data Observed data frame or CSV path.
#' @param sim_time Simulated time column.
#' @param sim_endpoint Simulated endpoint column.
#' @param obs_time Observed time column.
#' @param obs_endpoint Observed endpoint column.
#' @param sim_repl Optional simulated replicate column.
#' @param sim_subject Optional simulated subject ID column.
#' @param obs_subject Optional observed subject ID column.
#' @param sim_time_is_binned Whether simulated time is already binned.
#' @param nominal_times Optional nominal sampling times.
#' @param exclude_subjects Optional subject IDs to exclude.
#' @param strat_vars Variables eligible for stratification and plotting.
#' @param label_sources Optional named list of label-source variables.
#' @param manual_labels Optional named list of manual label definitions.
#' @param show_codes Whether to show original numeric codes with labels.
#' @param plot_settings Optional list returned by vpc_plot_settings().
#' @param make_plots Whether to generate plots.
#' @param make_summaries Whether to generate summary tables.
#'
#' @return An object of class "vpc_analysis", containing prepared
#'   data, stratification configuration, plot settings, plots,
#'   overview, and percentile summaries.
#' @export
run_vpc_analysis <- function(
    sim_data,
    obs_data,
    sim_time,
    sim_endpoint,
    obs_time,
    obs_endpoint,
    sim_repl = NULL,
    sim_subject = NULL,
    obs_subject = NULL,
    sim_time_is_binned = FALSE,
    nominal_times = NULL,
    exclude_subjects = NULL,
    strat_vars = character(0),
    label_sources = list(),
    manual_labels = list(),
    show_codes = TRUE,
    plot_settings = NULL,
    make_plots = TRUE,
    make_summaries = TRUE
) {
  
  # 1. Load and prepare datasets.
  prepared <- load_vpc_data(
    sim_data = sim_data,
    obs_data = obs_data,
    sim_time = sim_time,
    sim_endpoint = sim_endpoint,
    obs_time = obs_time,
    obs_endpoint = obs_endpoint,
    sim_repl = sim_repl,
    sim_subject = sim_subject,
    obs_subject = obs_subject,
    sim_time_is_binned = sim_time_is_binned,
    nominal_times = nominal_times,
    exclude_subjects = exclude_subjects
  )
  
  # 2. Configure stratification and label mappings.
  strata <- configure_strata(
    data = prepared,
    eligible_vars = strat_vars,
    label_sources = label_sources,
    manual_labels = manual_labels,
    show_codes = show_codes
  )
  
  # 3. Create or validate plot settings.
  if (is.null(plot_settings)) {
    plot_settings <- vpc_plot_settings(
      strat_vars = strat_vars
    )
  } else {
    if (!is.list(plot_settings) ||
        !all(c("strat_vars", "probs", "palette") %in%
             names(plot_settings))) {
      stop(
        "plot_settings must be a list returned by vpc_plot_settings().",
        call. = FALSE
      )
    }
    
    # Use the explicitly supplied stratification configuration.
    plot_settings$strat_vars <- strat_vars
  }
  
  # 4. Generate plots and summaries.
  plots <- NULL
  overview <- NULL
  percentiles <- NULL
  
  if (make_plots) {
    plots <- plot_vpc_strata(
      data = prepared,
      settings = plot_settings
    )
  }
  
  if (make_summaries) {
    overview <- summarise_vpc_data(
      data = prepared,
      strat_vars = strat_vars
    )
    
    percentiles <- summarise_vpc_percentiles(
      data = prepared,
      strat_vars = strat_vars,
      probs = plot_settings$probs
    )
  }
  
  structure(
    list(
      data = prepared,
      strata = strata,
      plot_settings = plot_settings,
      plots = plots,
      overview = overview,
      percentiles = percentiles
    ),
    class = "vpc_analysis"
  )
}