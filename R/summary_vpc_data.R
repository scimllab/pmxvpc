
#' Summarise simulated and observed VPC data
#'
#' Produce a dataset overview for every common stratification
#' combination.
#'
#' @param data A list containing prepared data frames named sim and obs.
#' @param strat_vars Character vector of stratification variables.
#'   Defaults to character(0).
#' @param correction VPC correction method: "none" (default), "pc"
#'   (prediction-corrected), or "pvc" (prediction- and
#'   variance-corrected). Needs mapped prediction columns; "pvc"
#'   also needs sim_repl. See vpc_plot_settings() for details.
#' @param pc_method Rule used to compute the prediction-corrected
#'   value: "proportional" (default) or "additive". See
#'   vpc_plot_settings() for details.
#' @param reference_source Which data the per-bin correction
#'   reference is computed from: "obs_only" (default) or "pooled".
#'   See vpc_plot_settings() for details.
#' @param save_path Optional CSV file path to save the result to
#'   (via \code{utils::write.csv()}). NULL (default) skips saving.
#'
#' @return A data frame with stratum values, dataset name, record count,
#'   subject count, time-bin count, descriptive statistics, and
#'   nonpositive endpoint count.
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
#' summarise_vpc_data(data, strat_vars = "DOSE")
#' @export
summarise_vpc_data <- function(
    data,
    strat_vars = character(0),
    correction = "none",
    pc_method = "proportional",
    reference_source = "obs_only",
    save_path = NULL
) {

  if (!is.null(save_path) &&
      (!is.character(save_path) || length(save_path) != 1L ||
       is.na(save_path) || !nzchar(save_path))) {
    stop("save_path must be a single file path, or NULL.", call. = FALSE)
  }

  maybe_save <- function(result) {
    if (!is.null(save_path)) {
      utils::write.csv(result, save_path, row.names = FALSE)
    }

    result
  }

  if (!is.list(data) ||
      !is.data.frame(data$sim) ||
      !is.data.frame(data$obs)) {
    stop(
      "data must contain data frames named sim and obs.",
      call. = FALSE
    )
  }
  
  if (!is.character(strat_vars) || anyNA(strat_vars)) {
    stop("strat_vars must be a character vector.",
         call. = FALSE)
  }

  if (!is.character(correction) || length(correction) != 1L ||
      is.na(correction) || !correction %in% c("none", "pc", "pvc")) {
    stop("correction must be 'none', 'pc', or 'pvc'.", call. = FALSE)
  }

  if (!is.character(pc_method) || length(pc_method) != 1L ||
      is.na(pc_method) || !pc_method %in% c("proportional", "additive")) {
    stop("pc_method must be 'proportional' or 'additive'.", call. = FALSE)
  }

  if (!is.character(reference_source) || length(reference_source) != 1L ||
      is.na(reference_source) ||
      !reference_source %in% c("obs_only", "pooled")) {
    stop(
      "reference_source must be 'obs_only' or 'pooled'.",
      call. = FALSE
    )
  }

  required_cols <- c(".vpc_endpoint", ".vpc_time_bin")
  
  for (nm in c("sim", "obs")) {
    missing <- setdiff(required_cols, names(data[[nm]]))
    
    if (length(missing)) {
      stop(
        nm, " data is missing required columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  common_vars <- intersect(
    names(data$sim),
    names(data$obs)
  )
  
  invalid_vars <- setdiff(strat_vars, common_vars)
  
  if (length(invalid_vars)) {
    stop(
      "Stratification variables must exist in both datasets: ",
      paste(invalid_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  strata <- get_vpc_strata(
    sim_data = data$sim,
    obs_data = data$obs,
    strat_vars = strat_vars
  )
  
  if (!nrow(strata)) {
    return(maybe_save(data.frame(
      Message = "No common stratification combinations found."
    )))
  }
  
  summarise_one_dataset <- function(
    group_data, dataset_name, stratum
  ) {

    y <- group_data$.vpc_endpoint
    
    if (!is.numeric(y)) {
      stop(".vpc_endpoint must be numeric.", call. = FALSE)
    }
    
    valid_y <- y[is.finite(y)]
    
    n_subjects <- if (".vpc_subject" %in% names(group_data)) {
      length(unique(stats::na.omit(group_data$.vpc_subject)))
    } else {
      NA_integer_
    }
    
    result <- data.frame(
      Dataset = dataset_name,
      Records = nrow(group_data),
      Subjects = n_subjects,
      Time_bins = length(unique(
        stats::na.omit(group_data$.vpc_time_bin)
      )),
      Mean = if (length(valid_y)) mean(valid_y) else NA_real_,
      SD = if (length(valid_y) > 1L) {
        stats::sd(valid_y)
      } else {
        NA_real_
      },
      Median = if (length(valid_y)) {
        stats::median(valid_y)
      } else {
        NA_real_
      },
      Minimum = if (length(valid_y)) min(valid_y) else NA_real_,
      Maximum = if (length(valid_y)) max(valid_y) else NA_real_,
      Nonpositive = sum(
        is.finite(y) & y <= 0,
        na.rm = TRUE
      ),
      check.names = FALSE
    )
    
    if (length(strat_vars)) {
      values <- as.data.frame(
        lapply(strat_vars, function(v) {
          as.character(stratum[[v]][1L])
        }),
        stringsAsFactors = FALSE
      )
      
      names(values) <- strat_vars
      result <- cbind(values, result)
    }
    
    result
  }
  
  rows <- lapply(seq_len(nrow(strata)), function(i) {
    stratum <- strata[i, , drop = FALSE]

    sim_i <- filter_vpc_stratum(data$sim, stratum, strat_vars)
    obs_i <- filter_vpc_stratum(data$obs, stratum, strat_vars)

    if (!identical(correction, "none")) {
      reference <- vpc_correction_reference(
        sim_i, obs_i,
        correction = correction,
        pc_method = pc_method,
        reference_source = reference_source
      )

      sim_i <- apply_vpc_correction(
        sim_i, reference, correction, pc_method = pc_method,
        is_simulated = TRUE
      )
      obs_i <- apply_vpc_correction(
        obs_i, reference, correction, pc_method = pc_method,
        is_simulated = FALSE
      )
    }

    rbind(
      summarise_one_dataset(
        sim_i, "Simulated", stratum
      ),
      summarise_one_dataset(
        obs_i, "Observed", stratum
      )
    )
  })
  
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  maybe_save(result)
}