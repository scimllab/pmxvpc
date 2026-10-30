#' Summarise VPC percentiles by stratum and time
#'
#' Calculate simulated and observed lower, median, and upper
#' endpoint percentiles for every stratum and time bin.
#'
#' @param data A list containing prepared data frames named sim and obs.
#' @param strat_vars Character vector of stratification variables.
#' @param probs Numeric vector of two probabilities for the lower
#'   and upper prediction percentiles. Defaults to c(0.05, 0.95).
#'
#' @param correction VPC correction method: "none" (default), "pc"
#'   for a prediction-corrected VPC, or "pvc" for a prediction- and
#'   variance-corrected VPC. Requires prediction (and, for "pvc",
#'   prediction variance) columns to be mapped via load_vpc_data().
#' @param pc_method Rule used to compute the prediction-corrected
#'   value: "proportional" (default) or "additive". See
#'   vpc_plot_settings() for details.
#' @param reference_source Which data the per-bin correction
#'   reference is computed from: "obs_only" (default) or "pooled".
#'   See vpc_plot_settings() for details.
#' @param save_path Optional file path to save the resulting data
#'   frame to as CSV (via \code{utils::write.csv()}). NULL (default)
#'   does not save.
#'
#' @return A data frame containing stratum values, time bins,
#'   simulated and observed percentiles, and record counts.
#' @export
summarise_vpc_percentiles <- function(
    data,
    strat_vars = character(0),
    probs = c(0.05, 0.95),
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
  
  if (!is.numeric(probs) || length(probs) != 2L ||
      any(!is.finite(probs)) || any(probs <= 0 | probs >= 1) ||
      probs[1L] >= probs[2L]) {
    stop(
      "probs must contain two increasing probabilities between 0 and 1.",
      call. = FALSE
    )
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
  
  rows <- lapply(seq_len(nrow(strata)), function(i) {
    
    stratum <- strata[i, , drop = FALSE]
    
    sim_i <- filter_vpc_stratum(
      data$sim, stratum, strat_vars
    )
    
    obs_i <- filter_vpc_stratum(
      data$obs, stratum, strat_vars
    )

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

    sim_sum <- summarise_vpc(
      sim_i,
      probs = probs,
      time_col = ".vpc_time_bin",
      endpoint_col = ".vpc_endpoint"
    )
    
    obs_sum <- summarise_vpc(
      obs_i,
      probs = probs,
      time_col = ".vpc_time_bin",
      endpoint_col = ".vpc_endpoint"
    )
    
    rename_summary_columns <- function(
    x, prefix
    ) {
      
      expected <- c(
        lower = paste0(prefix, "_lower"),
        median = paste0(prefix, "_median"),
        upper = paste0(prefix, "_upper"),
        n = paste0(prefix, "_n")
      )
      
      for (old in names(expected)) {
        if (old %in% names(x)) {
          names(x)[names(x) == old] <- expected[[old]]
        }
      }
      
      x
    }
    
    sim_sum <- rename_summary_columns(sim_sum, "Sim")
    obs_sum <- rename_summary_columns(obs_sum, "Obs")
    
    if (!"Time_Bin" %in% names(sim_sum) ||
        !"Time_Bin" %in% names(obs_sum)) {
      stop(
        "summarise_vpc() must return a Time_Bin column.",
        call. = FALSE
      )
    }
    
    result <- dplyr::full_join(
      sim_sum,
      obs_sum,
      by = "Time_Bin"
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
  })
  
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  maybe_save(result)
}