default_nominal_times <- c(
  0, 1, 2, 3,
  3.92, 4, 4.08, 4.16, 4.25, 4.5, 4.75,
  seq(5, 10, 1),
  12, 16, 24
)

vpc_required_internal <- c(
  ".vpc_time", ".vpc_endpoint"
)

read_vpc_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "unique"
  )
}

# Keep all original columns and add standardized internal fields.
map_vpc_columns <- function(
    data,
    time_col,
    endpoint_col,
    repl_col = NULL,
    subject_col = NULL,
    part_col = NULL,
    treatment_col = NULL,
    time_is_binned = FALSE,
    data_name = "Data") {
  
  selected <- c(time_col, endpoint_col, repl_col,subject_col,
                part_col, treatment_col)
  selected <- selected[ !is.na(selected) &
                          nzchar(selected)] #!is.null(selected) &
  
  missing_cols <- setdiff(selected, names(data))
  
  if (length(missing_cols)) {
    stop(
      data_name, " is missing selected columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (anyDuplicated(selected)) {
    stop("Time, endpoint, subject, PART and TREATMENT mappings must be distinct.")
  }
  
  if (!is.numeric(data[[time_col]])) {
    stop(data_name, ": the mapped time column must be numeric.")
  }
  
  # Convert the mapped endpoint to numeric when possible.
  endpoint <- data[[endpoint_col]]
  
  if (!is.numeric(endpoint)) {
    
    endpoint_chr <- trimws(as.character(endpoint))
    
    # Treat empty strings and BLQ/LLOQ values as missing.
    ignore <- is.na(endpoint_chr) |
      endpoint_chr == "" |
      grepl("BLQ|LLOQ", endpoint_chr, ignore.case = TRUE)
    
    endpoint_chr[ignore] <- NA_character_
    
    endpoint_num <- suppressWarnings(
      as.numeric(endpoint_chr)
    )
    
    # Identify remaining nonnumeric values.
    bad <- !is.na(endpoint_chr) & is.na(endpoint_num)
    
    if (any(bad)) {
      bad_values <- unique(endpoint_chr[bad])
      
      stop(
        data_name,
        ": the mapped endpoint contains nonnumeric values: ",
        paste(head(bad_values, 10), collapse = ", "),
        ". Please correct these values before loading."
      )
    }
    
    endpoint <- endpoint_num
  }
  
  # Preserve the original covariate columns for stratification.
  result <- as.data.frame(data)
  
  result$.vpc_time <- data[[time_col]]
  result$.vpc_endpoint <- endpoint #data[[endpoint_col]]
  # Add replicate ID only when a replicate column is supplied.
  if (!is.null(repl_col) &&
      length(repl_col) == 1 &&
      !is.na(repl_col) &&
      nzchar(repl_col)) {
    result$.vpc_repl <- data[[repl_col]]
  }
  
  if (!is.null(subject_col) && nzchar(subject_col)) {
    result$.vpc_subject <- as.character(data[[subject_col]])
  }
  
  # Standardize optional group columns too, while retaining originals.
  if (!is.null(part_col) && nzchar(part_col)) {
    result$.vpc_part <- data[[part_col]]
  }
  
  if (!is.null(treatment_col) && nzchar(treatment_col)) {
    result$.vpc_treatment <- data[[treatment_col]]
  }
  
  attr(result, "vpc_time_is_binned") <- time_is_binned
  
  result
}

assign_vpc_time_bins <- function(
    data,
    nominal_times = default_nominal_times,
    time_col = ".vpc_time",
    output_col = ".vpc_time_bin") {
  
  if (!time_col %in% names(data)) {
    stop("Time column not found: ", time_col)
  }
  
  if (!is.numeric(data[[time_col]])) {
    stop("Time must be numeric.")
  }
  
  if (length(nominal_times) == 0 ||
      any(!is.finite(nominal_times))) {
    stop("Nominal times must be finite numeric values.")
  }
  
  data[[output_col]] <- vapply(
    data[[time_col]],
    function(x) {
      if (is.na(x) || !is.finite(x)) return(NA_real_)
      
      nominal_times[
        which.min(abs(nominal_times - x))
      ]
    },
    numeric(1)
  )
  
  data
}

prepare_vpc_dataset <- function(
    data,
    nominal_times = default_nominal_times,
    time_is_binned = FALSE,
    exclude_subjects = character(0),
    dataset_name = "Dataset") {
  
  if (!all(vpc_required_internal %in% names(data))) {
    stop("Map the time and endpoint columns before preparing data.")
  }
  
  if (".vpc_subject" %in% names(data) &&
      length(exclude_subjects) > 0) {
    data <- data[
      !(as.character(data$.vpc_subject) %in%
          as.character(exclude_subjects)),
      ,
      drop = FALSE
    ]
  }
  
  if (time_is_binned) {
    data$.vpc_time_bin <- data$.vpc_time
  } else {
    data <- assign_vpc_time_bins(
      data,
      nominal_times = nominal_times
    )
  }
  
  data <- data |>
    dplyr::filter(
      !is.na(.vpc_time_bin),
      is.finite(.vpc_endpoint),
      .vpc_endpoint > 0
    )
  
  if (nrow(data) == 0) {
    stop(dataset_name, " has no usable positive endpoint records.")
  }
  
  data
}

# Identify variables available in both datasets.
available_strata <- function(sim_data, obs_data) {
  excluded <- c(
    ".vpc_time", ".vpc_endpoint", ".vpc_subject",
    ".vpc_part", ".vpc_treatment", ".vpc_time_bin"
  )
  
  intersect(
    setdiff(names(sim_data), excluded),
    setdiff(names(obs_data), excluded)
  )
}

# Keep combinations that occur in both datasets.
get_vpc_strata <- function(
    sim_data,
    obs_data,
    strat_vars = character(0)) {
  
  if (length(strat_vars) == 0) {
    return(tibble::tibble(.all_data = "All data"))
  }
  
  missing_sim <- setdiff(strat_vars, names(sim_data))
  missing_obs <- setdiff(strat_vars, names(obs_data))
  
  if (length(missing_sim) || length(missing_obs)) {
    stop("All stratification variables must exist in both datasets.")
  }
  
  sim_groups <- sim_data |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(strat_vars))
    )
  
  obs_groups <- obs_data |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(strat_vars))
    )
  
  dplyr::inner_join(
    sim_groups,
    obs_groups,
    by = strat_vars
  ) |>
    dplyr::distinct()
}

filter_vpc_stratum <- function(
    data,
    stratum,
    strat_vars = character(0)) {
  
  if (length(strat_vars) == 0) return(data)
  
  result <- data
  
  for (variable in strat_vars) {
    target <- stratum[[variable]][1]
    
    if (is.na(target)) {
      result <- result[is.na(result[[variable]]), , drop = FALSE]
    } else {
      result <- result[
        !is.na(result[[variable]]) &
          as.character(result[[variable]]) ==
          as.character(target),
        ,
        drop = FALSE
      ]
    }
  }
  
  result
}

# Summarise arbitrary numeric endpoint data by time.
summarise_vpc <- function(
    data,
    probs = c(0.05, 0.95),
    time_col = ".vpc_time_bin",
    endpoint_col = ".vpc_endpoint",
    repl_col = ".vpc_repl",
    by_replicate = FALSE
) {
  
  if (length(probs) != 2 ||
      probs[1] <= 0 ||
      probs[2] >= 1 ||
      probs[1] >= probs[2]) {
    stop("Prediction percentiles must be increasing and inside (0, 1).")
  }
  
  if (by_replicate) {
    if (is.null(repl_col) ||
        length(repl_col) != 1 ||
        is.na(repl_col) ||
        !nzchar(repl_col)) {
      stop("repl_col must be supplied when by_replicate = TRUE.")
    }
    
    if (!repl_col %in% names(data)) {
      stop("Replicate column not found: ", repl_col)
    }
  }
  
  # Group by time, optionally including replicate
  group_cols <- c(
    time_col,
    if (by_replicate) repl_col
  )
  
  data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      lower = unname(stats::quantile(
        .data[[endpoint_col]], probs[1], na.rm = TRUE
      )),
      median = stats::median(
        .data[[endpoint_col]], na.rm = TRUE
      ),
      upper = unname(stats::quantile(
        .data[[endpoint_col]], probs[2], na.rm = TRUE
      )),
      n = sum(is.finite(.data[[endpoint_col]])),
      .groups = "drop"
    ) |>
    dplyr::rename(Time_Bin = dplyr::all_of(time_col))
}

# Bootstrap confidence intervals for observed quantiles.
bootstrap_vpc_ci <- function(
    data,
    probs = c(0.05, 0.95),
    confidence = 0.95,
    B = 300,
    seed = 123,
    time_col = ".vpc_time_bin",
    endpoint_col = ".vpc_endpoint",
    repl_col = ".vpc_repl",
    subject_col = NULL) {
  
  if (confidence <= 0 || confidence >= 1) {
    stop("Confidence must be between 0 and 1.")
  }
  
  if (B < 100) stop("Use at least 100 bootstrap replicates.")
  
  set.seed(seed)
  alpha <- (1 - confidence) / 2
  
  groups <- data |>
    dplyr::group_by(.data[[time_col]]) |>
    dplyr::group_split()
  
  results <- lapply(groups, function(g) {
    x <- g[[endpoint_col]]
    ok <- is.finite(x) & x > 0
    g <- g[ok, , drop = FALSE]
    x <- g[[endpoint_col]]
    
    if (!length(x)) return(NULL)
    
    statistic <- function(v) {
      c(
        lower = unname(stats::quantile(v, probs[1])),
        median = stats::median(v),
        upper = unname(stats::quantile(v, probs[2]))
      )
    }
    
    observed <- statistic(x)
    
    # If subject IDs are available, resample subjects as clusters.
    if (!is.null(subject_col) && subject_col %in% names(g) &&
        !all(is.na(g[[subject_col]]))) {
      
      ids <- unique(g[[subject_col]])
      boot <- replicate(B, {
        sampled <- sample(ids, length(ids), replace = TRUE)
        
        xb <- unlist(lapply(sampled, function(id) {
          g[[endpoint_col]][g[[subject_col]] == id]
        }), use.names = FALSE)
        
        statistic(xb)
      })
    } else {
      boot <- replicate(B, {
        statistic(sample(x, length(x), replace = TRUE))
      })
    }
    
    ci <- apply(
      boot, 1, stats::quantile,
      probs = c(alpha, 1 - alpha),
      na.rm = TRUE,
      names = FALSE
    )
    
    tibble::tibble(
      Time_Bin = g[[time_col]][1],
      lower = observed["lower"],
      median = observed["median"],
      upper = observed["upper"],
      lower_ci_low = ci[1, 1],
      lower_ci_high = ci[2, 1],
      median_ci_low = ci[1, 2],
      median_ci_high = ci[2, 2],
      upper_ci_low = ci[1, 3],
      upper_ci_high = ci[2, 3],
      n = length(x)
    )
  })
  
  dplyr::bind_rows(results)
}