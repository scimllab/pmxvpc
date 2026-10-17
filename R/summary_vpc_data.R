
#' Summarise simulated and observed VPC data
#'
#' Produce a dataset overview for every common stratification
#' combination.
#'
#' @param data A list containing prepared data frames named sim and obs.
#' @param strat_vars Character vector of stratification variables.
#'   Defaults to character(0).
#'
#' @return A data frame with stratum values, dataset name, record count,
#'   subject count, time-bin count, descriptive statistics, and
#'   nonpositive endpoint count.
#' @export
summarise_vpc_data <- function(
    data,
    strat_vars = character(0)
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
  
  summarise_one_dataset <- function(
    df, dataset_name, stratum
  ) {
    
    group_data <- filter_vpc_stratum(
      df,
      stratum,
      strat_vars
    )
    
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
    
    rbind(
      summarise_one_dataset(
        data$sim, "Simulated", stratum
      ),
      summarise_one_dataset(
        data$obs, "Observed", stratum
      )
    )
  })
  
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  
  result
}