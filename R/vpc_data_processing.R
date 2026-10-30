#' Default nominal sampling times
#'
#' A built-in vector of nominal sampling times, usable as the
#' \code{nominal_times} argument to \code{\link{load_vpc_data}} or
#' \code{\link{run_vpc_analysis}} when a study-specific schedule isn't
#' available.
#'
#' @examples
#' default_nominal_times
#' @export
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
    pred_col = NULL,
    part_col = NULL,
    treatment_col = NULL,
    time_is_binned = FALSE,
    data_name = "Data") {

  selected <- c(time_col, endpoint_col, repl_col, subject_col,
                pred_col, part_col, treatment_col)
  selected <- selected[!is.na(selected) & nzchar(selected)]
  
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
  result$.vpc_endpoint <- endpoint

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

  # Population prediction columns used for pcVPC/pvcVPC corrections.
  if (!is.null(pred_col) && nzchar(pred_col)) {
    if (!is.numeric(data[[pred_col]])) {
      stop(data_name, ": the mapped prediction column must be numeric.")
    }

    result$.vpc_pred <- data[[pred_col]]
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

# Assign each record's time to a bin value using one of four methods:
#
# "none":            the bin value is the raw time (no binning).
# "explicit_center":  nearest-neighbor assignment to a fixed vector of
#                     bin centers (nominal_times).
# "explicit_bound":   records are grouped by user-supplied interval
#                     boundaries (bin_breaks); the bin value is the
#                     midpoint of the interval each record falls in.
# "kmeans":           bin centers are found automatically via
#                     1-dimensional k-means clustering on the time
#                     values (n_bins clusters), then assigned the
#                     same way as "explicit_center".
assign_vpc_time_bins <- function(
    data,
    method = "explicit_center",
    nominal_times = default_nominal_times,
    bin_breaks = NULL,
    n_bins = NULL,
    time_col = ".vpc_time",
    output_col = ".vpc_time_bin") {

  if (!time_col %in% names(data)) {
    stop("Time column not found: ", time_col)
  }

  if (!is.numeric(data[[time_col]])) {
    stop("Time must be numeric.")
  }

  valid_methods <- c("none", "explicit_center", "explicit_bound", "kmeans")

  if (!is.character(method) || length(method) != 1L ||
      is.na(method) || !method %in% valid_methods) {
    stop(
      "method must be one of: ",
      paste(valid_methods, collapse = ", "), "."
    )
  }

  if (method == "none") {
    data[[output_col]] <- data[[time_col]]
    return(data)
  }

  if (method == "explicit_bound") {
    if (length(bin_breaks) < 2 ||
        !is.numeric(bin_breaks) ||
        any(!is.finite(bin_breaks)) ||
        is.unsorted(bin_breaks, strictly = TRUE)) {
      stop(
        "bin_breaks must be at least two strictly increasing ",
        "finite numeric values."
      )
    }

    midpoints <- (bin_breaks[-length(bin_breaks)] + bin_breaks[-1]) / 2

    bin_index <- cut(
      data[[time_col]],
      breaks = bin_breaks,
      include.lowest = TRUE,
      labels = FALSE
    )

    data[[output_col]] <- midpoints[bin_index]
    return(data)
  }

  centers <- if (method == "kmeans") {
    if (is.null(n_bins) || length(n_bins) != 1L ||
        is.na(n_bins) || !is.finite(n_bins) ||
        n_bins != as.integer(n_bins) || n_bins < 1) {
      stop("n_bins must be a positive integer for kmeans binning.")
    }

    times <- data[[time_col]][
      !is.na(data[[time_col]]) & is.finite(data[[time_col]])
    ]

    if (length(unique(times)) < n_bins) {
      stop(
        "Not enough distinct time values (", length(unique(times)),
        ") for the requested number of kmeans bins (", n_bins, ")."
      )
    }

    # Fix the seed so the same data always yields the same bins:
    # kmeans() is otherwise sensitive to R's global RNG state even
    # with nstart > 1, which would make VPC bins non-reproducible.
    set.seed(123)

    sort(as.numeric(
      stats::kmeans(times, centers = n_bins, nstart = 10)$centers
    ))
  } else {
    if (length(nominal_times) == 0 ||
        !is.numeric(nominal_times) ||
        any(!is.finite(nominal_times))) {
      stop("Nominal times must be finite numeric values.")
    }

    sort(unique(nominal_times))
  }

  data[[output_col]] <- vapply(
    data[[time_col]],
    function(x) {
      if (is.na(x) || !is.finite(x)) return(NA_real_)

      centers[which.min(abs(centers - x))]
    },
    numeric(1)
  )

  data
}

prepare_vpc_dataset <- function(
    data,
    time_binning = "explicit_center",
    nominal_times = default_nominal_times,
    bin_breaks = NULL,
    n_bins = NULL,
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
      method = time_binning,
      nominal_times = nominal_times,
      bin_breaks = bin_breaks,
      n_bins = n_bins
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

# Compute the per-bin reference used to rescale endpoints for a
# prediction-corrected (pcVPC) or prediction- and variance-corrected
# (pvcVPC) plot or summary.
#
# bin_pred: median PRED per bin.
#   reference_source = "obs_only" (default): observed predictions only.
#   reference_source = "pooled": simulated and observed predictions
#     combined. PRED is a population-model quantity (not
#     residual-driven), so pooling just gives a larger, more stable
#     sample of the same underlying value; it does not mix genuinely
#     different populations.
#
# For "pvc", the variance reference is derived from the scatter of
# prediction-corrected values (pcY), not from a predicted-variance
# column:
#   - pcY is computed per record using pc_method and bin_pred (the
#     same pcY that "pc" would produce).
#   - A simulated record's denominator is the variance of its own
#     replicate's pcY pooled with the (single) observed pcY set for
#     that bin: repl_var[r] = var({sim pcY in replicate r} union
#     {obs pcY in the bin}).
#   - bin_var (the shared numerator) is the median of repl_var across
#     all replicates in that bin. This always uses the replicate
#     structure, independent of reference_source (which only controls
#     bin_pred).
#   - An observed record has no replicate of its own, so it uses every
#     replicate's repl_var in turn and is rescaled once per replicate;
#     its final corrected value is the median across those R
#     corrected values. Because the pvc rescaling is an affine
#     function of sqrt(bin_var / repl_var[r]) for fixed r, and median
#     commutes with a (per-record) strictly monotonic affine
#     transform, this is mathematically equivalent to using a single
#     effective obs_var per bin such that
#     sqrt(bin_var / obs_var) == median_r(sqrt(bin_var / repl_var[r])).
#     obs_var is precomputed this way so observed data keeps one row
#     per observed record (no replicate-level blow-up) while still
#     getting exactly the median-of-per-replicate-corrections result.
vpc_correction_reference <- function(
    sim_data,
    obs_data,
    correction = "none",
    pc_method = "proportional",
    reference_source = "obs_only",
    time_col = ".vpc_time_bin",
    endpoint_col = ".vpc_endpoint",
    pred_col = ".vpc_pred",
    repl_col = ".vpc_repl") {

  if (identical(correction, "none")) {
    return(NULL)
  }

  if (!correction %in% c("pc", "pvc")) {
    stop("correction must be one of 'none', 'pc', or 'pvc'.")
  }

  if (!reference_source %in% c("obs_only", "pooled")) {
    stop("reference_source must be 'obs_only' or 'pooled'.")
  }

  if (!pc_method %in% c("proportional", "additive")) {
    stop("pc_method must be 'proportional' or 'additive'.")
  }

  require_pred_column <- function(data, col, label, dataset_name) {
    if (!col %in% names(data)) {
      stop(
        dataset_name, " must include a mapped ", label,
        " column to use a prediction-corrected VPC."
      )
    }

    x <- data[[col]]

    if (!is.numeric(x) || any(!is.finite(x) | x <= 0)) {
      stop(
        dataset_name, ": the ", label,
        " column must contain positive finite values."
      )
    }

    x
  }

  if (reference_source == "pooled") {
    reference_time <- c(sim_data[[time_col]], obs_data[[time_col]])
    reference_pred <- c(
      require_pred_column(
        sim_data, pred_col, "prediction", "Simulated data"
      ),
      require_pred_column(
        obs_data, pred_col, "prediction", "Observed data"
      )
    )
  } else {
    reference_time <- obs_data[[time_col]]
    reference_pred <- require_pred_column(
      obs_data, pred_col, "prediction", "Observed data"
    )
  }

  pred <- vapply(
    split(reference_pred, reference_time),
    stats::median, numeric(1), na.rm = TRUE
  )

  if (correction == "pc") {
    return(list(pred = pred, var = NULL, repl_var = NULL, obs_var = NULL))
  }

  if (!repl_col %in% names(sim_data)) {
    stop(
      "Simulated data must include a mapped replicate column to use ",
      "a prediction- and variance-corrected VPC."
    )
  }

  compute_pcy <- function(data, dataset_name) {
    indiv_pred <- require_pred_column(
      data, pred_col, "prediction", dataset_name
    )
    bin_pred_i <- pred[as.character(data[[time_col]])]

    if (anyNA(bin_pred_i)) {
      stop("Every time bin must be present in the correction reference.")
    }

    endpoint <- data[[endpoint_col]]

    if (pc_method == "proportional") {
      endpoint * bin_pred_i / indiv_pred
    } else {
      endpoint - indiv_pred + bin_pred_i
    }
  }

  sim_pcy <- compute_pcy(sim_data, "Simulated data")
  obs_pcy <- compute_pcy(obs_data, "Observed data")

  sim_bin <- as.character(sim_data[[time_col]])
  obs_bin <- as.character(obs_data[[time_col]])
  sim_repl <- as.character(sim_data[[repl_col]])

  group_var <- function(vals) {
    vals <- vals[is.finite(vals)]

    if (length(vals) < 2) {
      return(NA_real_)
    }

    stats::var(vals)
  }

  bins <- names(pred)
  bin_var <- stats::setNames(rep(NA_real_, length(bins)), bins)
  repl_var <- stats::setNames(vector("list", length(bins)), bins)
  obs_var <- stats::setNames(rep(NA_real_, length(bins)), bins)

  for (b in bins) {
    obs_vals_b <- obs_pcy[obs_bin == b]
    sim_idx_b <- which(sim_bin == b)

    if (!length(sim_idx_b)) {
      stop(
        "No simulated records found for time bin ", b,
        " -- cannot compute a variance reference."
      )
    }

    repls_b <- unique(sim_repl[sim_idx_b])

    # Each replicate's denominator pools that replicate's own sim pcY
    # with the observed pcY for this bin.
    repl_var_b <- vapply(repls_b, function(r) {
      group_var(c(sim_pcy[sim_idx_b[sim_repl[sim_idx_b] == r]], obs_vals_b))
    }, numeric(1))

    names(repl_var_b) <- repls_b

    if (all(is.na(repl_var_b))) {
      stop(
        "Not enough records to compute a variance reference for ",
        "time bin ", b, "."
      )
    }

    repl_var[[b]] <- repl_var_b
    bin_var_b <- stats::median(repl_var_b, na.rm = TRUE)
    bin_var[[b]] <- bin_var_b

    # Collapse the per-replicate corrections observed data would get
    # into a single effective denominator (see the function-level
    # comment above for the derivation).
    valid_repl_var <- repl_var_b[!is.na(repl_var_b)]
    median_scale <- stats::median(sqrt(bin_var_b / valid_repl_var))
    obs_var[[b]] <- bin_var_b / median_scale^2
  }

  list(pred = pred, var = bin_var, repl_var = repl_var, obs_var = obs_var)
}

# Rescale a dataset's endpoint using the shared bin-level reference
# from vpc_correction_reference().
#
# "pc": the prediction-corrected value (pcY), using either rule:
#   "proportional": pcY = endpoint * bin_pred / individual_pred
#   "additive":      pcY = endpoint - individual_pred + bin_pred
#
# "pvc": pvcY = (pcY - bin_pred) * sqrt(bin_var / individual_var) + bin_pred
#   - For simulated records (is_simulated = TRUE), individual_var is
#     that record's own replicate's (pre-median) variance from
#     reference$repl_var -- so the ratio compares one replicate's
#     spread against the typical (median) replicate's spread.
#   - For observed records, individual_var is reference$obs_var for
#     that bin -- a single effective denominator, precomputed in
#     vpc_correction_reference(), that is mathematically equivalent to
#     correcting each observed record once per simulated replicate
#     (using that replicate's own repl_var) and taking the median of
#     the resulting values.
apply_vpc_correction <- function(
    data,
    reference,
    correction = "none",
    pc_method = "proportional",
    time_col = ".vpc_time_bin",
    endpoint_col = ".vpc_endpoint",
    pred_col = ".vpc_pred",
    repl_col = ".vpc_repl",
    is_simulated = FALSE) {

  if (identical(correction, "none") || is.null(reference)) {
    return(data)
  }

  if (!pc_method %in% c("proportional", "additive")) {
    stop("pc_method must be 'proportional' or 'additive'.")
  }

  key <- as.character(data[[time_col]])
  bin_pred <- reference$pred[key]

  if (anyNA(bin_pred)) {
    stop("Every time bin must be present in the correction reference.")
  }

  pred <- data[[pred_col]]
  endpoint <- data[[endpoint_col]]

  pc_y <- if (pc_method == "proportional") {
    endpoint * bin_pred / pred
  } else {
    endpoint - pred + bin_pred
  }

  if (correction == "pc") {
    data[[endpoint_col]] <- pc_y
    return(data)
  }

  bin_var <- reference$var[key]

  if (is_simulated) {
    if (!repl_col %in% names(data)) {
      stop(
        "Simulated data must include a mapped replicate column to use ",
        "a prediction- and variance-corrected VPC."
      )
    }

    repl_key <- as.character(data[[repl_col]])

    indiv_var <- vapply(seq_along(key), function(i) {
      rv <- reference$repl_var[[key[i]]]
      v <- rv[repl_key[i]]

      if (is.null(v) || is.na(v)) bin_var[i] else unname(v)
    }, numeric(1))
  } else {
    obs_var <- reference$obs_var[key]

    indiv_var <- ifelse(is.na(obs_var), bin_var, obs_var)
  }

  data[[endpoint_col]] <- (pc_y - bin_pred) *
    sqrt(bin_var / indiv_var) + bin_pred

  data
}

# Identify variables available in both datasets.
available_strata <- function(sim_data, obs_data) {
  excluded <- c(
    ".vpc_time", ".vpc_endpoint", ".vpc_subject",
    ".vpc_part", ".vpc_treatment", ".vpc_time_bin",
    ".vpc_pred"
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