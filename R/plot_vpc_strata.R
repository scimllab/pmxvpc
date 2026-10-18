
#' Generate VPC plots for all strata
#'
#' @param data A list returned by load_vpc_data(), containing
#'   sim, obs, and available_strata.
#' @param settings A list returned by vpc_plot_settings().
#'
#' @return A list containing settings, groups, and a named list of
#'   ggplot objects.
#' @export
plot_vpc_strata <- function(
    data,
    settings = vpc_plot_settings()
) {
  
  required <- c("sim", "obs", "available_strata")
  
  if (!is.list(data) ||
      !all(required %in% names(data))) {
    stop(
      "data must contain sim, obs, and available_strata.",
      call. = FALSE
    )
  }
  
  if (!is.list(settings) ||
      !all(c("strat_vars", "probs", "palette") %in%
           names(settings))) {
    stop(
      "settings must be created with vpc_plot_settings().",
      call. = FALSE
    )
  }
  
  #available <- data$available_strata %||% character()
  
  available <-if (is.null(data$available_strata)) {
    character()
  } else {
    data$available_strata
  }
  
  unknown <- setdiff(settings$strat_vars, available)
  
  if (length(unknown)) {
    stop(
      "Unknown stratification variables: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.null(settings$color_by) &&
      !settings$color_by %in% available) {
    stop(
      "color_by must be NULL or an available stratification variable.",
      call. = FALSE
    )
  }
  
  groups <- get_vpc_strata(
    data$sim,
    data$obs,
    strat_vars = settings$strat_vars
  )
  
  if (!nrow(groups)) {
    stop("No strata were found for the selected variables.",
         call. = FALSE)
  }
  
  plots <- vector("list", nrow(groups))
  
  for (i in seq_len(nrow(groups))) {
    
    stratum <- groups[i, , drop = FALSE]
    
    sim_i <- filter_vpc_stratum(
      data$sim,
      stratum,
      settings$strat_vars
    )
    
    obs_i <- filter_vpc_stratum(
      data$obs,
      stratum,
      settings$strat_vars
    )
    
    p <- make_vpc_plot(
      sim_data = sim_i,
      obs_data = obs_i,
      prediction_probs = settings$probs,
      confidence = settings$confidence,
      palette = settings$palette,
      log_scale = settings$log_scale,
      show_points = settings$show_points,
      B = settings$B,
      use_bootstrap = settings$use_bootstrap,
      color_by = settings$color_by,
      obs_median_color = settings$obs_median_color,
      sim_median_color = settings$sim_median_color,
      obs_tail_color = settings$obs_tail_color,
      sim_tail_color = settings$sim_tail_color,
      median_linetype = settings$median_linetype,
      obs_tail_linetype = settings$obs_tail_linetype,
      sim_tail_linetype = settings$sim_tail_linetype,
      point_color = settings$point_color,
      point_shape = settings$point_shape,
      line_width = settings$line_width,
      point_size = settings$point_size,
      ci_alpha = settings$ci_alpha,
      x_axis_label = settings$x_axis_label,
      y_axis_label = settings$y_axis_label
    )
    
    # Apply axis labels consistently.
    p <- p + ggplot2::labs(
      x = if (nzchar(trimws(settings$x_axis_label))) {
        settings$x_axis_label
      } else {
        NULL
      },
      y = if (nzchar(trimws(settings$y_axis_label))) {
        settings$y_axis_label
      } else {
        NULL
      }
    )
    
    plots[[i]] <- p
  }
  
  # Create readable names for the plot list.
  if (length(settings$strat_vars)) {
    plot_names <- vapply(
      seq_len(nrow(groups)),
      function(i) {
        paste(
          vapply(
            settings$strat_vars,
            function(v) {
              paste0(v, "=", as.character(groups[[v]][i]))
            },
            character(1)
          ),
          collapse = ", "
        )
      },
      character(1)
    )
  } else {
    plot_names <- "All data"
  }
  
  names(plots) <- make.unique(plot_names)
  
  structure(
    list(
      settings = settings,
      groups = groups,
      plots = plots
    ),
    class = "vpc_plot_collection"
  )
}