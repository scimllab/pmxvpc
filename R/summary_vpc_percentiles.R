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
#' @return A data frame containing stratum values, time bins,
#'   simulated and observed percentiles, and record counts.
#' @export
summarise_vpc_percentiles <- function(
    data,
    strat_vars = character(0),
    probs = c(0.05, 0.95)
) {
  
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
    return(data.frame(
      Message = "No common stratification combinations found."
    ))
  }
  
  rows <- lapply(seq_len(nrow(strata)), function(i) {
    
    stratum <- strata[i, , drop = FALSE]
    
    sim_i <- filter_vpc_stratum(
      data$sim, stratum, strat_vars
    )
    
    obs_i <- filter_vpc_stratum(
      data$obs, stratum, strat_vars
    )
    
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
  
  result
}