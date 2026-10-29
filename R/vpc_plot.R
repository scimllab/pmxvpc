#' vpc plot
#' @export
#'
vpc_palettes <- list(
  Blue = c(
    fill = "#87CEEB", sim = "#2166AC",
    obs = "#D73027", points = "#333333"
  ),
  Viridis = c(
    fill = "#80CDC1", sim = "#5E3C99",
    obs = "#E66101", points = "#333333"
  ),
  `Okabe-Ito` = c(
    fill = "#56B4E9", sim = "#0072B2",
    obs = "#D55E00", points = "#333333"
  ),
  Dark = c(
    fill = "#8DA0CB", sim = "#4D4D4D",
    obs = "#E41A1C", points = "#333333"
  ),
  Set1 = c(
    fill = "#80B1D3", sim = "#377EB8",
    obs = "#E41A1C", points = "#333333"
  )
)

make_vpc_plot <- function(
    sim_data,
    obs_data,
    prediction_probs = c(0.05, 0.95),
    confidence = 0.95,
    palette = "Blue",
    log_scale = TRUE,
    show_points = TRUE,
    B = 300,
    use_bootstrap = FALSE,
    time_col = ".vpc_time_bin",
    endpoint_col = ".vpc_endpoint",
    repl_col = ".vpc_repl",
    color_by = NULL,
    subject_col = ".vpc_subject",
    obs_median_color = "#000000",
    sim_median_color = "#2166AC",
    obs_tail_color = "#000000",
    sim_tail_color = "#D73027",
    median_linetype = "solid",
    obs_tail_linetype = "dashed",
    sim_tail_linetype = "solid",
    point_color = "#333333",
    point_shape = 16,
    line_width = 1,
    point_size = 1.2,
    ci_alpha = 0.18,
    x_axis_label = "Time",
    y_axis_label = "Concentration") {
  
  if (!palette %in% names(vpc_palettes)) {
    stop("Unknown palette.")
  }
  
  if (!time_col %in% names(sim_data) ||
      !time_col %in% names(obs_data)) {
    stop("The time-bin column is missing.")
  }
  
  if (!endpoint_col %in% names(sim_data) ||
      !endpoint_col %in% names(obs_data)) {
    stop("The endpoint column is missing.")
  }
  
  # Keep the source datasets unchanged.
  # Exclude non-positive values only from this log-scale plot.
  if (log_scale) {
    sim_data <- sim_data[
      is.finite(sim_data[[endpoint_col]]) &
        sim_data[[endpoint_col]] > 0,
      ,
      drop = FALSE
    ]
    
    obs_data <- obs_data[
      is.finite(obs_data[[endpoint_col]]) &
        obs_data[[endpoint_col]] > 0,
      ,
      drop = FALSE
    ]
  }
  
  if (nrow(sim_data) == 0 || nrow(obs_data) == 0) {
    stop(
      "No positive endpoint values remain for both datasets. ",
      "Switch to a linear y-axis or check the endpoint mapping."
    )
  }
  
  sim_by_repl <- summarise_vpc(
    sim_data,
    probs = prediction_probs,
    time_col = time_col,
    endpoint_col = endpoint_col,
    repl_col = repl_col,
    by_replicate = TRUE
  )
  ci_probs <- c(0.025, 0.975)
  
  sim_sum <- sim_by_repl |>
    dplyr::group_by(Time_Bin) |>
    dplyr::summarise(
      # Replicate-specific p05 distribution
      p05_median = stats::median(lower, na.rm = TRUE),
      p05_CI_lower = stats::quantile(
        lower, ci_probs[1], na.rm = TRUE
      ),
      p05_CI_upper = stats::quantile(
        lower, ci_probs[2], na.rm = TRUE
      ),
      
      # Replicate-specific median distribution
      median = stats::median(median, na.rm = TRUE),
      median_CI_lower = stats::quantile(
        median, ci_probs[1], na.rm = TRUE
      ),
      median_CI_upper = stats::quantile(
        median, ci_probs[2], na.rm = TRUE
      ),
      
      # Replicate-specific p95 distribution
      p95_median = stats::median(upper, na.rm = TRUE),
      p95_CI_lower = stats::quantile(
        upper, ci_probs[1], na.rm = TRUE
      ),
      p95_CI_upper = stats::quantile(
        upper, ci_probs[2], na.rm = TRUE
      ),
      
      n_replicates = dplyr::n(),
      .groups = "drop"
    )
  
  subject_arg <- if (
    subject_col %in% names(obs_data)
  ) subject_col else NULL
  
  obs_sum <- summarise_vpc(
    obs_data,
    probs = prediction_probs,
    time_col = time_col,
    repl_col = NULL,
    by_replicate = FALSE,
    endpoint_col = endpoint_col
  )
  
  if (use_bootstrap) {
    obs_ci <- bootstrap_vpc_ci(
      obs_data,
      probs = prediction_probs,
      confidence = confidence,
      B = B,
      time_col = time_col,
      endpoint_col = endpoint_col,
      subject_col = subject_arg
    )
    
    ci_cols <- setdiff(names(obs_ci), names(obs_sum))
    
    obs_sum <- dplyr::left_join(
      obs_sum,
      dplyr::select(
        obs_ci,
        Time_Bin,
        dplyr::all_of(ci_cols)
      ),
      by = "Time_Bin"
    )
  }
  
  pal <- vpc_palettes[[palette]]
  
  p <- ggplot2::ggplot() +
    # Central 95% of replicate-specific P05 values
    ggplot2::geom_ribbon(
      data = sim_sum,
      ggplot2::aes(
        x = Time_Bin,
        ymin = p05_CI_lower,
        ymax = p05_CI_upper,
        fill = "Simulated percentile 95% interval"
      ),
      
      alpha = ci_alpha * 0.7
    ) +
    # Central 95% of replicate-specific medians
    ggplot2::geom_ribbon(
      data = sim_sum,
      ggplot2::aes(
        x = Time_Bin,
        ymin = median_CI_lower,
        ymax = median_CI_upper,
        fill = "Simulated median 95% interval"
      ),
      alpha = ci_alpha
    ) +
    # Central 95% of replicate-specific P95 values
    ggplot2::geom_ribbon(
      data = sim_sum,
      ggplot2::aes(
        x = Time_Bin,
        ymin = p95_CI_lower,
        ymax = p95_CI_upper,
        fill="Simulated percentile 95% interval"
      ),
      alpha = ci_alpha * 0.7
    ) +
    ggplot2::geom_line(
      data = sim_sum,
      ggplot2::aes(x = Time_Bin, y = p05_median,
                   colour = "Simulated percentiles",
                   linetype = "Simulated percentiles"),
      linewidth = line_width
    ) +
    ggplot2::geom_line(
      data = sim_sum,
      ggplot2::aes(x = Time_Bin, y = p95_median,
                   colour =  "Simulated percentiles",
                   linetype = "Simulated percentiles"),
      
      linewidth = line_width
    ) +
    ggplot2::geom_line(
      data = sim_sum,
      ggplot2::aes(x = Time_Bin, y = median,
                   colour =  "Simulated median",
                   linetype = "Simulated median"),
      linewidth = line_width * 1.2
    )
  
  # Observed P5 and P95: black dashed by default.
  p <- p +
    ggplot2::geom_line(
      data = obs_sum,
      ggplot2::aes(x = Time_Bin, y = lower,
                   colour =  "Observed percentiles",
                   linetype = "Observed percentiles"),
      linewidth = line_width
    ) +
    ggplot2::geom_line(
      data = obs_sum,
      ggplot2::aes(x = Time_Bin, y = upper,
                   colour =  "Observed percentiles",
                   linetype = "Observed percentiles"),
      linewidth = line_width
    )
  
  # Observed P50: black solid by default.
  p <- p +
    ggplot2::geom_line(
      data = obs_sum,
      ggplot2::aes(x = Time_Bin, y = median,
                   colour =  "Observed median",
                   linetype = "Observed median"),
      linewidth = line_width * 1.2
    )
  
  if (show_points) {
    p<-p+ggplot2::geom_point(
      data = obs_data,
      ggplot2::aes(
        x = .data[[time_col]],
        y = .data[[endpoint_col]],
        colour =  "Observed concentrations"
      ),
      shape=point_shape,
      size = point_size,
      alpha = 0.45
    )
  }
  
  # Ensure the observed median column has a consistent name.
  if (!"median" %in% names(obs_sum)) {
    median_col <- intersect(
      c("obs_median", "Obs_median", "median.x", "median.y"),
      names(obs_sum)
    )
    
    if (length(median_col) > 0) {
      obs_sum$median <- obs_sum[[median_col[1]]]
    } else {
      stop(
        "Observed summary has no median column. Available columns: ",
        paste(names(obs_sum), collapse = ", ")
      )
    }
  }
  
  p<-p +
    ggplot2::labs(
      x = if (nzchar(x_axis_label)) x_axis_label else NULL,
      y = if (nzchar(y_axis_label)) y_axis_label else NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }
  
  legend_labels <- c(
    "Observed median",
    "Simulated median",
    "Observed percentiles",
    "Simulated percentiles",
    "Observed concentrations",
    "Simulated median 95% interval",
    "Simulated percentile 95% interval"
  )
  
  p <- p +
    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "Observed median" = obs_median_color,
        "Simulated median" = sim_median_color,
        "Observed percentiles" = obs_tail_color,
        "Simulated percentiles" = sim_tail_color,
        "Observed concentrations" = point_color
      ),
      breaks = legend_labels[1:5],
      guide = ggplot2::guide_legend(
        override.aes = list(
          linetype = c(
            median_linetype,
            median_linetype,
            obs_tail_linetype,
            sim_tail_linetype,
            "blank"
          ),
          shape = c(NA, NA, NA, NA, point_shape)
        )
      )
    ) +
    ggplot2::scale_linetype_manual(
      name = "",
      values = c(
        "Observed median" = median_linetype,
        "Simulated median" = median_linetype,
        "Observed percentiles" = obs_tail_linetype,
        "Simulated percentiles" = sim_tail_linetype
      ),
      breaks = legend_labels[1:4],
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c(
        "Simulated percentile 95% interval" = sim_tail_color,
        "Simulated median 95% interval" = sim_median_color
      ),
      breaks = legend_labels[6:7],
      guide = ggplot2::guide_legend(
        order = 2,
        override.aes = list(
          alpha = c(ci_alpha, ci_alpha * 0.7),
          linetype = 0
        )
      )
    )+
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1,
        override.aes = list(
          linetype = c(
            median_linetype,
            median_linetype,
            obs_tail_linetype,
            sim_tail_linetype,
            "blank"
          ),
          shape = c(NA, NA, NA, NA, point_shape)
        )
      ),
      fill = ggplot2::guide_legend(
        order = 2,
        override.aes = list(
          alpha = c(ci_alpha, ci_alpha * 0.7)
        )
      )
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.box.just = "left"
    )
  p
}