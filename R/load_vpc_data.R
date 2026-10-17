
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
#' @param sim_time_is_binned Whether simulated time is already binned.
#' @param nominal_times Optional numeric vector of nominal sampling times,
#'   or a comma-separated string. NULL or "" uses actual time values.
#' @param exclude_subjects Optional character vector of subject IDs to
#'   exclude, or a comma-separated string.
#'
#' @return A list containing:
#' \describe{
#'   \item{sim}{Prepared simulated dataset.}
#'   \item{obs}{Prepared observed dataset.}
#'   \item{available_strata}{Common stratification variables.}
#'   \item{nominal_times}{Nominal times used, or NULL.}
#' }
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
    sim_time_is_binned = FALSE,
    nominal_times = NULL,
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
    time_is_binned = isTRUE(sim_time_is_binned),
    data_name = "Simulated data"
  )
  
  obs <- map_vpc_columns(
    obs,
    time_col = obs_time,
    endpoint_col = obs_endpoint,
    repl_col = NULL,
    subject_col = obs_subject,
    data_name = "Observed data"
  )
  
  # Prepare both datasets.
  #
  # This retains the original module's preparation behavior:
  # nominal times supplied -> time_is_binned = FALSE;
  # nominal times omitted  -> time_is_binned = TRUE.
  prepare_as_binned <- is.null(nominal_times)
  
  sim <- prepare_vpc_dataset(
    sim,
    nominal_times = nominal_times,
    time_is_binned = prepare_as_binned,
    dataset_name = "Simulated data"
  )
  
  obs <- prepare_vpc_dataset(
    obs,
    nominal_times = nominal_times,
    time_is_binned = prepare_as_binned,
    exclude_subjects = exclude_subjects,
    dataset_name = "Observed data"
  )
  
  # Find common stratification variables.
  strata <- available_strata(sim, obs)
  
  list(
    sim = sim,
    obs = obs,
    available_strata = strata,
    nominal_times = nominal_times
  )
}
