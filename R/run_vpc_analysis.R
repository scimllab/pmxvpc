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
#' @param sim_pred Optional simulated population prediction column.
#'   Required for a prediction-corrected ("pc") or prediction- and
#'   variance-corrected ("pvc") VPC; see \code{plot_settings}. If the
#'   named column is missing from the data it is silently ignored
#'   unless correction is actually requested.
#' @param obs_pred Optional observed population prediction column.
#'   Missing columns are silently ignored as for \code{sim_pred}. A
#'   prediction- and variance-corrected ("pvc") VPC also requires
#'   \code{sim_repl} to be mapped, since the variance reference is
#'   derived from the scatter of prediction-corrected values across
#'   simulated replicates.
#' @param sim_time_is_binned Whether simulated time is already binned.
#' @param time_binning Time-binning method: "explicit_center",
#'   "explicit_bound", "kmeans", or "none". See load_vpc_data() for
#'   details; NULL auto-selects based on \code{nominal_times}.
#' @param nominal_times Optional nominal sampling times.
#' @param time_bin_breaks Optional explicit bin boundaries, used when
#'   \code{time_binning = "explicit_bound"}.
#' @param n_time_bins Optional number of bins, used when
#'   \code{time_binning = "kmeans"}.
#' @param exclude_subjects Optional subject IDs to exclude.
#' @param strat_vars Variables eligible for stratification and plotting.
#' @param label_sources Optional named list of label-source variables.
#' @param manual_labels Optional named list of manual label definitions.
#' @param show_codes Whether to show original numeric codes with labels.
#' @param plot_settings Optional list returned by vpc_plot_settings().
#' @param make_plots Whether to generate plots.
#' @param make_summaries Whether to generate summary tables.
#' @param overview_save_path Optional file path to save the overview
#'   data frame to as CSV. NULL (default) does not save.
#' @param percentiles_save_path Optional file path to save the
#'   percentiles data frame to as CSV. NULL (default) does not save.
#' @param combined_plot_save_path Optional file path to save a
#'   combined plot (all strata arranged into one image via
#'   combine_vpc_plots()) to. NULL (default) does not create or save
#'   a combined plot. Ignored when \code{make_plots = FALSE}. When
#'   \code{plots_per_panel} causes more than one page, each page is
#'   saved with "_page<N>" inserted before the file extension.
#' @param plots_per_panel Maximum number of stratum panels per
#'   combined plot, passed to combine_vpc_plots(). NULL (default)
#'   puts every panel on one combined plot. Only applies when
#'   \code{combined_plot_save_path} is supplied.
#'
#' @references
#' Bergstrand M, Hooker AC, Wallin JE, Karlsson MO. Prediction-corrected
#' visual predictive checks for diagnosing nonlinear mixed-effects
#' models. \emph{AAPS J}. 2011;13(2):143-151.
#' \doi{10.1208/s12248-011-9255-z}
#'
#' @return An object of class "vpc_analysis", containing prepared
#'   data, stratification configuration, plot settings, plots,
#'   overview, and percentile summaries, plus a combined_plot entry
#'   when \code{combined_plot_save_path} is supplied (a single plot,
#'   or a list of plots if \code{plots_per_panel} produces more than
#'   one page -- see combine_vpc_plots()).
#'
#' @examples
#' result <- run_vpc_analysis(
#'   sim_data = pmxvpc_sim_example,
#'   obs_data = pmxvpc_obs_example,
#'   sim_time = "TIME",
#'   sim_endpoint = "DV",
#'   obs_time = "TIME",
#'   obs_endpoint = "DV",
#'   sim_repl = "REP",
#'   sim_subject = "ID",
#'   obs_subject = "ID",
#'   nominal_times = c(0, 1, 2, 4, 8, 12, 24),
#'   strat_vars = "DOSE"
#' )
#' result$overview
#' result$plots$plots[[1]]
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
    sim_pred = NULL,
    obs_pred = NULL,
    sim_time_is_binned = FALSE,
    time_binning = NULL,
    nominal_times = NULL,
    time_bin_breaks = NULL,
    n_time_bins = NULL,
    exclude_subjects = NULL,
    strat_vars = character(0),
    label_sources = list(),
    manual_labels = list(),
    show_codes = TRUE,
    plot_settings = NULL,
    make_plots = TRUE,
    make_summaries = TRUE,
    overview_save_path = NULL,
    percentiles_save_path = NULL,
    combined_plot_save_path = NULL,
    plots_per_panel = NULL
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
    sim_pred = sim_pred,
    obs_pred = obs_pred,
    sim_time_is_binned = sim_time_is_binned,
    time_binning = time_binning,
    nominal_times = nominal_times,
    time_bin_breaks = time_bin_breaks,
    n_time_bins = n_time_bins,
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
  combined_plot <- NULL

  if (make_plots) {
    plots <- plot_vpc_strata(
      data = prepared,
      settings = plot_settings
    )

    if (!is.null(combined_plot_save_path)) {
      combined_plot <- combine_vpc_plots(
        plots,
        plots_per_panel = plots_per_panel,
        save_path = combined_plot_save_path
      )
    }
  }
  
  if (make_summaries) {
    correction <- if (is.null(plot_settings$correction)) {
      "none"
    } else {
      plot_settings$correction
    }

    pc_method <- if (is.null(plot_settings$pc_method)) {
      "proportional"
    } else {
      plot_settings$pc_method
    }

    reference_source <- if (is.null(plot_settings$reference_source)) {
      "obs_only"
    } else {
      plot_settings$reference_source
    }

    overview <- summarise_vpc_data(
      data = prepared,
      strat_vars = strat_vars,
      correction = correction,
      pc_method = pc_method,
      reference_source = reference_source,
      save_path = overview_save_path
    )

    percentiles <- summarise_vpc_percentiles(
      data = prepared,
      strat_vars = strat_vars,
      probs = plot_settings$probs,
      correction = correction,
      pc_method = pc_method,
      reference_source = reference_source,
      save_path = percentiles_save_path
    )
  }

  structure(
    list(
      data = prepared,
      strata = strata,
      plot_settings = plot_settings,
      plots = plots,
      overview = overview,
      percentiles = percentiles,
      combined_plot = combined_plot
    ),
    class = "vpc_analysis"
  )
}