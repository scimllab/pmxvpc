#' Build the mapping between original numeric codes and
#' user-selected character labels or manually entered labels.
#'
#' @param sim Prepared simulated data frame.
#' @param obs Prepared observed data frame.
#' @param variable Name of the numeric stratification variable to label.
#' @param label_source Name of a common character or factor variable
#'   to derive labels from, or NULL to skip.
#' @param manual_labels Named character vector of manual code-to-label
#'   overrides, e.g. \code{c(`1` = "A")}.
#'
#' @return A data frame with \code{code} and \code{label} columns
#'   mapping each stratification code to its display label.
#'
#' @examples
#' data <- load_vpc_data(
#'   sim_data = pmxvpc_sim_example,
#'   obs_data = pmxvpc_obs_example,
#'   sim_time = "TIME",
#'   sim_endpoint = "DV",
#'   obs_time = "TIME",
#'   obs_endpoint = "DV",
#'   nominal_times = c(0, 1, 2, 4, 8, 12, 24)
#' )
#' build_strata_label_map(
#'   sim = data$sim,
#'   obs = data$obs,
#'   variable = "DOSE",
#'   manual_labels = c(`100` = "Low dose", `300` = "High dose")
#' )
#' @export

build_strata_label_map <- function(
    sim,
    obs,
    variable,
    label_source = NULL,
    manual_labels = NULL) {
  
  # Validate the stratification variable.
  if (!variable %in% names(sim) ||
      !variable %in% names(obs)) {
    stop(
      "Stratification variable '", variable,
      "' must exist in both datasets."
    )
  }
  
  # Start with an empty mapping.
  mapping <- data.frame(
    code = character(),
    label = character(),
    stringsAsFactors = FALSE
  )
  
  # Use the explicitly selected character label variable.
  if (!is.null(label_source) &&
      length(label_source) == 1 &&
      !is.na(label_source) &&
      nzchar(label_source)) {
    
    if (!label_source %in% names(sim) ||
        !label_source %in% names(obs)) {
      stop(
        "Label variable '", label_source,
        "' must exist in both datasets."
      )
    }
    
    sim_map <- data.frame(
      code = as.character(sim[[variable]]),
      label = as.character(sim[[label_source]]),
      stringsAsFactors = FALSE
    )
    
    obs_map <- data.frame(
      code = as.character(obs[[variable]]),
      label = as.character(obs[[label_source]]),
      stringsAsFactors = FALSE
    )
    
    candidate <- unique(rbind(sim_map, obs_map))
    
    candidate <- candidate[
      !is.na(candidate$code) &
        !is.na(candidate$label) &
        nzchar(candidate$label),
      ,
      drop = FALSE
    ]
    
    # Only accept codes that map to exactly one label.
    label_counts <- vapply(
      split(candidate$label, candidate$code),
      function(x) length(unique(x)),
      integer(1)
    )
    
    valid_codes <- names(label_counts[label_counts == 1])
    
    mapping <- candidate[
      candidate$code %in% valid_codes,
      ,
      drop = FALSE
    ]
    
    mapping <- unique(mapping)
  }
  
  # Manual labels override labels from the selected character column.
  if (!is.null(manual_labels) &&
      length(manual_labels) > 0) {
    
    manual_map <- data.frame(
      code = names(manual_labels),
      label = unname(manual_labels),
      stringsAsFactors = FALSE
    )
    
    manual_map <- manual_map[
      !is.na(manual_map$code) &
        !is.na(manual_map$label) &
        nzchar(manual_map$label),
      ,
      drop = FALSE
    ]
    
    mapping <- mapping[
      !mapping$code %in% manual_map$code,
      ,
      drop = FALSE
    ]
    
    mapping <- rbind(mapping, manual_map)
  }
  
  rownames(mapping) <- NULL
  mapping
}


# Format a value for display without changing the underlying data.
get_strata_display_label <- function(
    value,
    mapping,
    show_code = TRUE) {
  
  code <- as.character(value)
  
  if (is.null(mapping) ||
      !is.data.frame(mapping) ||
      !all(c("code", "label") %in% names(mapping)) ||
      nrow(mapping) == 0) {
    return(code)
  }
  
  idx <- match(code, mapping$code)
  
  if (is.na(idx)) {
    return(code)
  }
  
  label <- mapping$label[idx]
  
  if (isTRUE(show_code)) {
    paste0(code, " - ", label)
  } else {
    label
  }
}


# Parse manual entries such as:
# 1 = Placebo
# 2 = Active
parse_manual_strata_labels <- function(x) {
  
  if (is.null(x) ||
      length(x) == 0 ||
      is.na(x[1]) ||
      !nzchar(trimws(x[1]))) {
    return(setNames(character(), character()))
  }
  
  lines <- strsplit(
    x[1],
    "\n",
    fixed = TRUE
  )[[1]]
  
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  
  result <- character()
  
  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    
    if (length(parts) < 2) {
      next
    }
    
    code <- trimws(parts[1])
    label <- trimws(paste(parts[-1], collapse = "="))
    
    if (nzchar(code) && nzchar(label)) {
      result[code] <- label
    }
  }
  
  result
}