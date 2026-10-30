
#' Configure VPC stratification variables and labels
#'
#' Configure which common variables can be used for stratification
#' and optionally assign readable labels to numeric categorical
#' variables.
#'
#' @param data A list containing simulated data in \code{sim} and
#'   observed data in \code{obs}.
#' @param eligible_vars Character vector of variables eligible for
#'   stratification. Defaults to character(0).
#' @param label_sources Named list or character vector mapping
#'   numeric variables to character/factor label variables.
#'   Use "" or NULL to use numeric codes.
#' @param manual_labels Named list of manual label specifications.
#'   Each value can be a string with one mapping per line, such as
#'   "1 = Placebo".
#' @param show_codes Whether to display original numeric codes
#'   alongside labels.
#'
#' @return A list containing the configuration, eligible variables,
#'   label maps, and show_codes setting.
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
#' strata <- configure_strata(data, eligible_vars = "DOSE")
#' strata$eligible_vars
#' @export
configure_strata <- function(
    data,
    eligible_vars = character(0),
    label_sources = list(),
    manual_labels = list(),
    show_codes = TRUE
) {
  
  # Validate input data.
  if (!is.list(data) ||
      !is.data.frame(data$sim) ||
      !is.data.frame(data$obs)) {
    stop(
      "data must be a list containing data frames named sim and obs.",
      call. = FALSE
    )
  }
  
  if (!is.character(eligible_vars) || anyNA(eligible_vars)) {
    stop("eligible_vars must be a character vector.",
         call. = FALSE)
  }
  
  if (!is.logical(show_codes) ||
      length(show_codes) != 1L ||
      is.na(show_codes)) {
    stop("show_codes must be TRUE or FALSE.",
         call. = FALSE)
  }
  
  if (!is.list(label_sources) &&
      !is.character(label_sources)) {
    stop("label_sources must be a named list or character vector.",
         call. = FALSE)
  }
  
  if (!is.list(manual_labels) &&
      !is.character(manual_labels)) {
    stop("manual_labels must be a named list or character vector.",
         call. = FALSE)
  }
  
  # Find variables shared by both datasets.
  common_vars <- intersect(
    names(data$sim),
    names(data$obs)
  )
  
  invalid_vars <- setdiff(eligible_vars, common_vars)
  
  if (length(invalid_vars)) {
    stop(
      "Stratification variables must exist in both datasets: ",
      paste(invalid_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Identify numeric variables requiring optional label mapping.
  is_numeric_variable <- function(variable) {
    is.numeric(data$sim[[variable]]) ||
      is.numeric(data$obs[[variable]])
  }
  
  numeric_vars <- eligible_vars[
    vapply(eligible_vars, is_numeric_variable, logical(1))
  ]
  
  # Identify variables that can supply character labels.
  is_character_variable <- function(variable) {
    is.character(data$sim[[variable]]) ||
      is.factor(data$sim[[variable]]) ||
      is.character(data$obs[[variable]]) ||
      is.factor(data$obs[[variable]])
  }
  
  character_vars <- common_vars[
    vapply(common_vars, is_character_variable, logical(1))
  ]
  
  # Normalize named configuration values.
  normalize_named_values <- function(x, argument) {
    if (!length(x)) {
      return(list())
    }
    
    nms <- names(x)
    
    if (is.null(nms) || anyNA(nms) ||
        any(!nzchar(nms)) || anyDuplicated(nms)) {
      stop(
        argument, " must have unique, non-empty variable names.",
        call. = FALSE
      )
    }
    
    as.list(x)
  }
  
  label_sources <- normalize_named_values(
    label_sources, "label_sources"
  )
  
  manual_labels <- normalize_named_values(
    manual_labels, "manual_labels"
  )
  
  # Construct the final source and manual-label configuration.
  sources <- setNames(
    lapply(numeric_vars, function(variable) {
      
      source <- label_sources[[variable]]
      
      if (is.null(source) || !length(source)) {
        return("")
      }
      
      if (!is.character(source) ||
          length(source) != 1L || is.na(source)) {
        stop(
          "Label source for '", variable,
          "' must be a single variable name or NULL.",
          call. = FALSE
        )
      }
      
      source <- trimws(source)
      
      if (!nzchar(source)) {
        return("")
      }
      
      if (!source %in% character_vars) {
        stop(
          "Label source '", source,
          "' for '", variable,
          "' must be a common character or factor variable.",
          call. = FALSE
        )
      }
      
      source
    }),
    numeric_vars
  )
  
  manual <- setNames(
    lapply(numeric_vars, function(variable) {
      value <- manual_labels[[variable]]
      
      if (is.null(value)) {
        return("")
      }
      
      if (!is.character(value) ||
          anyNA(value)) {
        stop(
          "Manual labels for '", variable,
          "' must be character data.",
          call. = FALSE
        )
      }
      
      paste(value, collapse = "\n")
    }),
    numeric_vars
  )
  
  # Build the label maps using the package's existing helpers.
  label_maps <- list()
  
  for (variable in numeric_vars) {
    
    source <- sources[[variable]]
    
    if (!nzchar(source)) {
      source <- NULL
    }
    
    label_maps[[variable]] <- build_strata_label_map(
      sim = data$sim,
      obs = data$obs,
      variable = variable,
      label_source = source,
      manual_labels = parse_manual_strata_labels(
        manual[[variable]]
      )
    )
  }
  
  config <- list(
    eligible_vars = eligible_vars,
    label_sources = sources,
    manual_labels = manual,
    show_codes = show_codes
  )
  
  list(
    config = config,
    eligible_vars = eligible_vars,
    label_maps = label_maps,
    show_codes = show_codes
  )
}
