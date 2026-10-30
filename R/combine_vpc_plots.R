#' Combine stratified VPC plots into a single plot
#'
#' Arranges every panel in a "vpc_plot_collection" (as returned by
#' plot_vpc_strata(), or found at \code{result$plots} from
#' run_vpc_analysis()) into one combined plot, optionally saving it to
#' a file. When there are more panels than \code{plots_per_panel}
#' allows, the panels are split across multiple combined plots
#' ("pages") instead, each saved to its own file.
#'
#' @param plots A "vpc_plot_collection" object.
#' @param plots_per_panel Maximum number of stratum panels to place on
#'   one combined plot. NULL (default) puts every panel on a single
#'   combined plot, matching the previous behavior. When set and the
#'   number of panels exceeds it, panels are split, in order, across
#'   as many combined plots as needed (\code{ncol}/\code{nrow} still
#'   control the layout within each individual page).
#' @param ncol Number of columns in each combined layout, or NULL to
#'   let \code{patchwork} choose automatically.
#' @param nrow Number of rows in each combined layout, or NULL to let
#'   \code{patchwork} choose automatically.
#' @param titles Whether to label each panel with its stratum
#'   combination. Defaults to TRUE.
#' @param guides How shared legends are handled: "collect" (default)
#'   merges every panel's legend into one shared legend; "keep" leaves
#'   each panel's own legend in place.
#' @param legend_position Where the collected legend is placed:
#'   "bottom" (default), "right", "top", "left", or "none". Each panel
#'   already sets its own legend to "bottom", but \code{patchwork}
#'   does not inherit that for the collected legend unless told to,
#'   so this is applied explicitly. Only affects the result when
#'   \code{guides = "collect"}.
#' @param save_path Optional file path to save the combined plot(s) to
#'   (passed to \code{ggplot2::ggsave()}; the format is inferred from
#'   the file extension, e.g. ".png" or ".pdf"). NULL (default) does
#'   not save. When \code{plots_per_panel} produces more than one
#'   page, each page is saved with "_page<N>" inserted before the file
#'   extension (e.g. "combined.png" becomes "combined_page1.png",
#'   "combined_page2.png", ...).
#' @param width,height,dpi Passed to \code{ggplot2::ggsave()} when
#'   \code{save_path} is supplied.
#'
#' @return When \code{plots_per_panel} is NULL (default), the combined
#'   plot (a \code{patchwork} object) -- matching the previous,
#'   single-plot behavior. When \code{plots_per_panel} is supplied, a
#'   list of combined plots, one per page, in the same order as the
#'   stratum panels (even if that list has only one element, so the
#'   return type stays consistent once pagination is requested).
#'   Returned invisibly when \code{save_path} is supplied.
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
#' plots <- plot_vpc_strata(data, vpc_plot_settings(strat_vars = "DOSE"))
#' combined <- combine_vpc_plots(plots)
#' combined
#' @export
combine_vpc_plots <- function(
    plots,
    plots_per_panel = NULL,
    ncol = NULL,
    nrow = NULL,
    titles = TRUE,
    guides = "collect",
    legend_position = "bottom",
    save_path = NULL,
    width = 10,
    height = 8,
    dpi = 300
) {

  if (!inherits(plots, "vpc_plot_collection") ||
      !is.list(plots$plots)) {
    stop(
      "plots must be a 'vpc_plot_collection' returned by ",
      "plot_vpc_strata() (or run_vpc_analysis()$plots).",
      call. = FALSE
    )
  }

  panel_list <- plots$plots

  if (!length(panel_list)) {
    stop("plots$plots is empty; there is nothing to combine.",
         call. = FALSE)
  }

  if (!is.logical(titles) || length(titles) != 1L || is.na(titles)) {
    stop("titles must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(guides) || length(guides) != 1L ||
      is.na(guides) || !guides %in% c("collect", "keep")) {
    stop("guides must be 'collect' or 'keep'.", call. = FALSE)
  }

  valid_legend_positions <- c("bottom", "right", "top", "left", "none")

  if (!is.character(legend_position) || length(legend_position) != 1L ||
      is.na(legend_position) ||
      !legend_position %in% valid_legend_positions) {
    stop(
      "legend_position must be one of: ",
      paste(valid_legend_positions, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.null(plots_per_panel)) {
    if (!is.numeric(plots_per_panel) || length(plots_per_panel) != 1L ||
        is.na(plots_per_panel) || !is.finite(plots_per_panel) ||
        plots_per_panel != as.integer(plots_per_panel) ||
        plots_per_panel < 1) {
      stop("plots_per_panel must be a positive integer, or NULL.",
           call. = FALSE)
    }

    plots_per_panel <- as.integer(plots_per_panel)
  }

  if (!is.null(save_path) &&
      (!is.character(save_path) || length(save_path) != 1L ||
       is.na(save_path) || !nzchar(save_path))) {
    stop("save_path must be a single file path, or NULL.", call. = FALSE)
  }

  if (isTRUE(titles)) {
    panel_list <- Map(
      function(p, nm) p + ggplot2::ggtitle(nm),
      panel_list,
      names(panel_list)
    )
  }

  build_combined <- function(sub_list) {
    patchwork::wrap_plots(
      sub_list,
      ncol = ncol,
      nrow = nrow,
      guides = guides
    ) & ggplot2::theme(legend.position = legend_position)
  }

  save_one <- function(combined, path) {
    ggplot2::ggsave(
      filename = path,
      plot = combined,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  # No pagination requested: every panel on one combined plot, exactly
  # as before plots_per_panel existed.
  if (is.null(plots_per_panel)) {
    combined <- build_combined(panel_list)

    if (!is.null(save_path)) {
      save_one(combined, save_path)
      return(invisible(combined))
    }

    return(combined)
  }

  # Pagination: split panel_list, in order, into chunks of size
  # plots_per_panel, and build one combined plot per chunk.
  chunk_index <- ceiling(seq_along(panel_list) / plots_per_panel)
  chunks <- split(panel_list, chunk_index)

  combined_pages <- lapply(chunks, build_combined)

  if (!is.null(save_path)) {
    base <- tools::file_path_sans_ext(save_path)
    ext <- tools::file_ext(save_path)
    ext <- if (nzchar(ext)) paste0(".", ext) else ""

    for (i in seq_along(combined_pages)) {
      save_one(combined_pages[[i]], paste0(base, "_page", i, ext))
    }

    return(invisible(combined_pages))
  }

  combined_pages
}
