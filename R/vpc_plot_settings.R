#' Create VPC plot settings
#'
#' @param strat_vars Character vector of stratification variables.
#' @param color_by Variable used to color observed points, or NULL.
#' @param palette Color palette name.
#' @param lower_pct Lower prediction percentile.
#' @param upper_pct Upper prediction percentile.
#' @param confidence Bootstrap confidence level.
#' @param use_bootstrap Whether to calculate bootstrap confidence intervals.
#' @param B Number of bootstrap replicates.
#' @param log_scale Use a logarithmic y-axis.
#' @param show_points Display observed concentrations.
#' @param obs_median_color Observed median line color.
#' @param sim_median_color Simulated median line color.
#' @param obs_tail_color Observed percentile line color.
#' @param sim_tail_color Simulated percentile line color.
#' @param median_linetype Median line type.
#' @param obs_tail_linetype Observed percentile line type.
#' @param sim_tail_linetype Simulated percentile line type.
#' @param point_color Observed point color.
#' @param point_shape Observed point shape.
#' @param line_width Line width.
#' @param point_size Point size.
#' @param ci_alpha Confidence interval ribbon opacity.
#' @param x_axis_label X-axis label.
#' @param y_axis_label Y-axis label.
#'
#' @return A validated list of plot settings.
#' @export
vpc_plot_settings <- function(
    strat_vars = character(),
    color_by = NULL,
    palette = "Blue",
    lower_pct = 5,
    upper_pct = 95,
    confidence = 0.95,
    use_bootstrap = FALSE,
    B = 300,
    log_scale = TRUE,
    show_points = TRUE,
    obs_median_color = "#000000",
    sim_median_color = "#2166AC",
    obs_tail_color = "#000000",
    sim_tail_color = "#D73027",
    median_linetype = "solid",
    obs_tail_linetype = "dashed",
    sim_tail_linetype = "dashed",
    point_color = "#333333",
    point_shape = 16,
    line_width = 1,
    point_size = 1.2,
    ci_alpha = 0.18,
    x_axis_label = "Time",
    y_axis_label = "Concentration"
) {
  
  check_number <- function(x, name, lower, upper) {
    if (!is.numeric(x) || length(x) != 1L ||
        is.na(x) || !is.finite(x) ||
        x < lower || x > upper) {
      stop(
        name, " must be a finite number between ",
        lower, " and ", upper, ".",
        call. = FALSE
      )
    }
  }
  
  check_number(lower_pct, "lower_pct", 1, 49)
  check_number(upper_pct, "upper_pct", 51, 99)
  
  if (lower_pct >= upper_pct) {
    stop(
      "lower_pct must be less than upper_pct.",
      call. = FALSE
    )
  }
  
  check_number(confidence, "confidence", 0.01, 0.99)
  
  if (!is.logical(use_bootstrap) ||
      length(use_bootstrap) != 1L ||
      is.na(use_bootstrap)) {
    stop("use_bootstrap must be TRUE or FALSE.", call. = FALSE)
  }
  
  if (!is.numeric(B) || length(B) != 1L ||
      is.na(B) || !is.finite(B) ||
      B != as.integer(B) || B < 100 || B > 5000) {
    stop("B must be an integer between 100 and 5000.",
         call. = FALSE)
  }
  
  if (!is.character(strat_vars) || anyNA(strat_vars)) {
    stop("strat_vars must be a character vector.",
         call. = FALSE)
  }
  
  if (!is.null(color_by) &&
      (!is.character(color_by) ||
       length(color_by) != 1L || is.na(color_by))) {
    stop("color_by must be NULL or one variable name.",
         call. = FALSE)
  }
  
  valid_linetypes <- c(
    "solid", "dashed", "dotted", "dotdash"
  )
  
  for (x in list(
    median_linetype,
    obs_tail_linetype,
    sim_tail_linetype
  )) {
    if (!is.character(x) || length(x) != 1L ||
        is.na(x) || !x %in% valid_linetypes) {
      stop("Invalid line type.", call. = FALSE)
    }
  }
  
  check_number(line_width, "line_width", 0.3, 2)
  check_number(point_size, "point_size", 0.5, 4)
  check_number(ci_alpha, "ci_alpha", 0.05, 0.5)
  
  valid_shapes <- c(16, 1, 17, 15, 3)
  
  if (!is.numeric(point_shape) ||
      length(point_shape) != 1L ||
      is.na(point_shape) ||
      !point_shape %in% valid_shapes) {
    stop("Invalid point_shape.", call. = FALSE)
  }
  
  if (!is.logical(log_scale) || length(log_scale) != 1L ||
      is.na(log_scale) ||
      !is.logical(show_points) || length(show_points) != 1L ||
      is.na(show_points)) {
    stop("log_scale and show_points must be TRUE or FALSE.",
         call. = FALSE)
  }
  
  if (!is.character(palette) || length(palette) != 1L ||
      is.na(palette) || !palette %in% names(vpc_palettes)) {
    stop(
      "Unknown palette. Available palettes: ",
      paste(names(vpc_palettes), collapse = ", "),
      call. = FALSE
    )
  }
  
  list(
    strat_vars = strat_vars,
    color_by = if (identical(color_by, "")) NULL else color_by,
    palette = palette,
    probs = c(lower_pct, upper_pct) / 100,
    confidence = confidence,
    use_bootstrap = use_bootstrap,
    B = if (use_bootstrap) as.integer(B) else 0L,
    log_scale = log_scale,
    show_points = show_points,
    obs_median_color = obs_median_color,
    sim_median_color = sim_median_color,
    obs_tail_color = obs_tail_color,
    sim_tail_color = sim_tail_color,
    median_linetype = median_linetype,
    obs_tail_linetype = obs_tail_linetype,
    sim_tail_linetype = sim_tail_linetype,
    point_color = point_color,
    point_shape = as.numeric(point_shape),
    line_width = line_width,
    point_size = point_size,
    ci_alpha = ci_alpha,
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label
  )
}
