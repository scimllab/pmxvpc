
#' Load and prepare simulated and observed VPC data
#'
#' Reads simulated and observed datasets, maps their columns to the
#' expected VPC structure, optionally excludes subjects, prepares
#' the datasets, and identifies common stratification variables.
#'
#' @param sim_data Simulated data frame or path to a CSV file.
#' @param obs_data Observed data frame or path to a CSV file.
#' @param sim_time Name of the simulated time column.
#' @param sim_endpoint Name of the simulated endpoint/concentration column.
#' @param sim_repl Name of the simulated replicate column, or NULL.
#' @param sim_subject Name of the simulated subject ID column, or NULL.
#' @param obs_time Name of the observed time column.
#' @param obs_endpoint Name of the observed endpoint/concentration column.
#' @param obs_subject Name of the observed subject ID column, or NULL.
#' @param sim_pred Name of the simulated population prediction
#'   column (e.g. PRED), or NULL. Required for a prediction-corrected
#'   ("pc") or prediction- and variance-corrected ("pvc") VPC; if the
#'   named column is missing from the data it is silently ignored
#'   (treated as not mapped) unless correction is actually requested,
#'   in which case plotting/summarizing will raise a clear error.
#' @param obs_pred Name of the observed population prediction
#'   column, or NULL. Required for a prediction-corrected ("pc") or
#'   prediction- and variance-corrected ("pvc") VPC; missing columns
#'   are silently ignored as for \code{sim_pred}. A prediction- and
#'   variance-corrected ("pvc") VPC also requires \code{sim_repl} to
#'   be mapped, since the variance reference is derived from the
#'   scatter of prediction-corrected values across simulated
#'   replicates (there is no separate predicted-variance column).
#' @param sim_time_is_binned Whether simulated time is already binned.
#' @param time_binning Method used to bin the x-axis (time) variable:
#'   "explicit_center" (nearest-neighbor assignment to a fixed vector
#'   of bin centers, \code{nominal_times}), "explicit_bound" (group by
#'   user-supplied interval boundaries, \code{time_bin_breaks}),
#'   "kmeans" (automatically derive \code{n_time_bins} bin centers via
#'   1-dimensional k-means clustering), or "none" (use the actual time
#'   values, unbinned). Defaults to NULL, which auto-selects for
#'   backward compatibility: "explicit_center" when \code{nominal_times}
#'   is supplied, otherwise "none".
#' @param nominal_times Optional numeric vector of nominal sampling times,
#'   or a comma-separated string. NULL or "" uses actual time values.
#'   Used when \code{time_binning = "explicit_center"}.
#' @param time_bin_breaks Optional numeric vector of strictly increasing
#'   bin boundaries, or a comma-separated string. Required when
#'   \code{time_binning = "explicit_bound"}.
#' @param n_time_bins Optional positive integer number of bins. Required
#'   when \code{time_binning = "kmeans"}.
#' @param exclude_subjects Optional character vector of subject IDs to
#'   exclude, or a comma-separated string.
#'
#' @return A list containing:
#' \describe{
#'   \item{sim}{Prepared simulated dataset.}
#'   \item{obs}{Prepared observed dataset.}
#'   \item{available_strata}{Common stratification variables.}
#'   \item{nominal_times}{Nominal times used, or NULL.}
#'   \item{time_binning}{Time-binning method actually used.}
#'   \item{bin_centers}{Resolved bin centers for "explicit_center" or
#'     "kmeans" binning (kmeans-derived, when applicable), or NULL.}
#'   \item{time_bin_breaks}{Parsed bin boundaries for "explicit_bound"
#'     binning, or NULL.}
#' }
#'
#' @examples
#' data <- load_vpc_data(
#'   sim_data = pmxvpc_sim_example,
#'   obs_data = pmxvpc_obs_example,
#'   sim_time = "TIME",
#'   sim_endpoint = "DV",
#'   obs_time = "TIME",
#'   obs_endpoint = "DV",
#'   sim_repl = "REP",
#'   sim_subject = "ID",
#'   obs_subject = "ID",
#'   nominal_times = c(0, 1, 2, 4, 8, 12, 24)
#' )
#' data$available_strata
#' @export
load_vpc_data <- function(
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
    exclude_subjects = NULL
) {
  
  # Read a CSV path or accept an existing data frame.
  read_input <- function(x, name) {
    if (is.data.frame(x)) {
      return(x)
    }
    
    if (is.character(x) && length(x) == 1L &&
        !is.na(x) && nzchar(x)) {
      if (!file.exists(x)) {
        stop(name, " file does not exist: ", x, call. = FALSE)
      }
      
      return(read_vpc_csv(x))
    }
    
    stop(
      name, " must be a data frame or a single CSV file path.",
      call. = FALSE
    )
  }
  
  sim <- read_input(sim_data, "Simulated data")
  obs <- read_input(obs_data, "Observed data")
  
  # Validate required column mappings.
  validate_column <- function(data, column, label, optional = FALSE) {
    if (is.null(column) ||
        (is.character(column) && length(column) == 1L &&
         !is.na(column) && !nzchar(column))) {
      if (optional) {
        return(NULL)
      }
      
      stop("A column must be specified for ", label, ".",
           call. = FALSE)
    }
    
    if (!is.character(column) || length(column) != 1L ||
        is.na(column)) {
      stop(label, " must be a single column name.",
           call. = FALSE)
    }
    
    if (!column %in% names(data)) {
      stop(
        "Column '", column, "' was not found for ", label, ".",
        call. = FALSE
      )
    }
    
    column
  }

  # Unlike validate_column(), a prediction column that is named but
  # missing from the data is silently skipped (treated as not mapped)
  # rather than erroring -- these columns are only ever needed when
  # correction = "pc"/"pvc" is actually requested later via
  # vpc_plot_settings(), which load_vpc_data() has no visibility into.
  # If correction is requested without a valid mapped column,
  # vpc_correction_reference() raises a clear, correction-specific
  # error at that point instead.
  validate_pred_column <- function(data, column, label) {
    if (is.null(column)) {
      return(NULL)
    }

    if (!is.character(column) || length(column) != 1L || is.na(column)) {
      stop(label, " must be a single column name.",
           call. = FALSE)
    }

    if (!nzchar(column) || !column %in% names(data)) {
      return(NULL)
    }

    column
  }

  sim_time <- validate_column(
    sim, sim_time, "simulated time"
  )
  sim_endpoint <- validate_column(
    sim, sim_endpoint, "simulated endpoint"
  )
  sim_repl <- validate_column(
    sim, sim_repl, "simulated replicate", optional = TRUE
  )
  sim_subject <- validate_column(
    sim, sim_subject, "simulated subject", optional = TRUE
  )
  
  obs_time <- validate_column(
    obs, obs_time, "observed time"
  )
  obs_endpoint <- validate_column(
    obs, obs_endpoint, "observed endpoint"
  )
  obs_subject <- validate_column(
    obs, obs_subject, "observed subject", optional = TRUE
  )

  sim_pred <- validate_pred_column(sim, sim_pred, "simulated prediction")
  obs_pred <- validate_pred_column(obs, obs_pred, "observed prediction")

  # Parse nominal sampling times.
  if (is.character(nominal_times) &&
      length(nominal_times) == 1L &&
      !is.na(nominal_times)) {
    
    if (!nzchar(trimws(nominal_times))) {
      nominal_times <- NULL
    } else {
      nominal_times <- trimws(
        strsplit(nominal_times, ",", fixed = TRUE)[[1]]
      )
      
      nominal_times <- suppressWarnings(
        as.numeric(nominal_times)
      )
    }
  }
  
  if (!is.null(nominal_times)) {
    if (!is.numeric(nominal_times) ||
        !length(nominal_times) ||
        any(!is.finite(nominal_times)) ||
        anyDuplicated(nominal_times)) {
      stop(
        "Nominal times must be unique numeric values, ",
        "or NULL to use actual time values.",
        call. = FALSE
      )
    }
  }

  # Determine the time-binning method. NULL auto-selects for backward
  # compatibility: nominal_times supplied -> "explicit_center";
  # nominal_times omitted -> "none" (use actual time values).
  valid_time_binning <- c(
    "none", "explicit_center", "explicit_bound", "kmeans"
  )

  if (is.null(time_binning)) {
    time_binning <- if (is.null(nominal_times)) "none" else "explicit_center"
  } else if (!is.character(time_binning) || length(time_binning) != 1L ||
             is.na(time_binning) || !time_binning %in% valid_time_binning) {
    stop(
      "time_binning must be one of: ",
      paste(valid_time_binning, collapse = ", "), ", or NULL.",
      call. = FALSE
    )
  }

  # Parse explicit bin boundaries.
  if (is.character(time_bin_breaks) &&
      length(time_bin_breaks) == 1L &&
      !is.na(time_bin_breaks)) {

    if (!nzchar(trimws(time_bin_breaks))) {
      time_bin_breaks <- NULL
    } else {
      time_bin_breaks <- trimws(
        strsplit(time_bin_breaks, ",", fixed = TRUE)[[1]]
      )

      time_bin_breaks <- suppressWarnings(as.numeric(time_bin_breaks))
    }
  }

  if (time_binning == "explicit_center" && is.null(nominal_times)) {
    stop(
      "nominal_times must be supplied when time_binning = 'explicit_center'.",
      call. = FALSE
    )
  }

  if (time_binning == "explicit_bound") {
    if (is.null(time_bin_breaks) ||
        !is.numeric(time_bin_breaks) ||
        length(time_bin_breaks) < 2 ||
        any(!is.finite(time_bin_breaks)) ||
        is.unsorted(time_bin_breaks, strictly = TRUE)) {
      stop(
        "time_bin_breaks must be at least two strictly increasing ",
        "finite numeric values when time_binning = 'explicit_bound'.",
        call. = FALSE
      )
    }
  }

  if (time_binning == "kmeans") {
    if (is.character(n_time_bins) && length(n_time_bins) == 1L &&
        !is.na(n_time_bins)) {
      n_time_bins <- suppressWarnings(as.numeric(n_time_bins))
    }

    if (is.null(n_time_bins) ||
        !is.numeric(n_time_bins) ||
        length(n_time_bins) != 1L ||
        is.na(n_time_bins) ||
        !is.finite(n_time_bins) ||
        n_time_bins != as.integer(n_time_bins) ||
        n_time_bins < 1) {
      stop(
        "n_time_bins must be a positive integer when ",
        "time_binning = 'kmeans'.",
        call. = FALSE
      )
    }

    n_time_bins <- as.integer(n_time_bins)
  }

  # Parse excluded subject IDs.
  if (is.null(exclude_subjects)) {
    exclude_subjects <- character()
  } else if (length(exclude_subjects) == 1L &&
             is.character(exclude_subjects) &&
             !is.na(exclude_subjects)) {
    exclude_subjects <- strsplit(
      exclude_subjects, ",", fixed = TRUE
    )[[1]]
  }
  
  if (!is.character(exclude_subjects) ||
      anyNA(exclude_subjects)) {
    stop(
      "exclude_subjects must be character IDs or NULL.",
      call. = FALSE
    )
  }
  
  exclude_subjects <- trimws(exclude_subjects)
  exclude_subjects <- exclude_subjects[nzchar(exclude_subjects)]
  
  # Standardize column names and mappings.
  sim <- map_vpc_columns(
    sim,
    time_col = sim_time,
    endpoint_col = sim_endpoint,
    repl_col = sim_repl,
    subject_col = sim_subject,
    pred_col = sim_pred,
    time_is_binned = isTRUE(sim_time_is_binned),
    data_name = "Simulated data"
  )

  obs <- map_vpc_columns(
    obs,
    time_col = obs_time,
    endpoint_col = obs_endpoint,
    repl_col = NULL,
    subject_col = obs_subject,
    pred_col = obs_pred,
    data_name = "Observed data"
  )
  
  # For kmeans binning, derive bin centers once from the pooled sim +
  # obs time values so both datasets snap to the same bins (running
  # kmeans separately per dataset would give mismatched centers and
  # break the sim/obs join on Time_Bin downstream).
  bin_centers <- NULL

  if (time_binning == "kmeans") {
    pooled_time <- c(sim$.vpc_time, obs$.vpc_time)
    pooled_time <- pooled_time[is.finite(pooled_time)]

    if (length(unique(pooled_time)) < n_time_bins) {
      stop(
        "Not enough distinct time values (", length(unique(pooled_time)),
        ") for the requested number of kmeans bins (", n_time_bins, ").",
        call. = FALSE
      )
    }

    # Fix the seed so the same data always yields the same bins:
    # kmeans() is otherwise sensitive to R's global RNG state even
    # with nstart > 1, which would make VPC bins non-reproducible.
    set.seed(123)

    bin_centers <- sort(as.numeric(
      stats::kmeans(pooled_time, centers = n_time_bins, nstart = 10)$centers
    ))

    prepare_binning <- "explicit_center"
    prepare_centers <- bin_centers
  } else if (time_binning == "explicit_center") {
    bin_centers <- sort(unique(nominal_times))
    prepare_binning <- time_binning
    prepare_centers <- nominal_times
  } else {
    prepare_binning <- time_binning
    prepare_centers <- nominal_times
  }

  # Prepare both datasets, binning time the same way for sim and obs.
  sim <- prepare_vpc_dataset(
    sim,
    time_binning = prepare_binning,
    nominal_times = prepare_centers,
    bin_breaks = time_bin_breaks,
    n_bins = n_time_bins,
    dataset_name = "Simulated data"
  )

  obs <- prepare_vpc_dataset(
    obs,
    time_binning = prepare_binning,
    nominal_times = prepare_centers,
    bin_breaks = time_bin_breaks,
    n_bins = n_time_bins,
    exclude_subjects = exclude_subjects,
    dataset_name = "Observed data"
  )

  # Find common stratification variables.
  strata <- available_strata(sim, obs)

  list(
    sim = sim,
    obs = obs,
    available_strata = strata,
    nominal_times = nominal_times,
    time_binning = time_binning,
    bin_centers = bin_centers,
    time_bin_breaks = time_bin_breaks
  )
}
