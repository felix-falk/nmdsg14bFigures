#' Called by draw_swimmerplot to draw swimmer plot.
#'
#' @param plot_data Data frame containing
#' the data for the swimmer plot rectangles.
#' @param immune_pts A vector of patient IDs.
#' @param outcome_pts A vector of patient IDs.
#' @param treatment_pts A vector of patient IDs.
#' @param gvhd_pts A vector of patient IDs.
#' @param mrd_terminal_pts A data frame describing mrd values taken
#' at the termination date, with the columns patno, rel_term_dat,
#' y and mrd_category.
#' @param title_string The title for the swimmer plot.
#' @returns A swimmer plot ggplot object.
#' @examples
#' swimmerplot(
#' plot_data,
#' immune_pts,
#' outcome_pts,
#' treatment_pts,
#' gvhd_pts,
#' mrd_terminal_pts,
#' title_string
#' )
swimmerplot <- function(
  plot_data,
  outcome_pts,
  mrd_terminal_pts,
  title_string,
  immune_pts = NULL,
  treatment_pts = NULL,
  gvhd_pts = NULL
) {

  if (is.null(outcome_pts) || nrow(outcome_pts) == 0) {
    stop("Outcome annotations are mandatory but no outcome data was provided.")
  }

  if (is.null(mrd_terminal_pts) || nrow(mrd_terminal_pts) == 0) {
    stop("MRD annotations are mandatory but no terminal MRD data was provided.")
  }

  swimmer_plot <- ggplot2::ggplot(plot_data) +

    # Add MRD rectangles
    ggplot2::geom_rect(ggplot2::aes(
      xmin = plot_data$xmin,
      xmax = plot_data$xmax,
      ymin = plot_data$ymin - 0.2,
      ymax = plot_data$ymax + 0.2,
      fill = plot_data$mrd_category
    ),
    color = "black"
    ) +

    # Add MRD legend
    ggplot2::scale_fill_manual(
      name = "MRD category (VAF %)",
      values = c(
        "Negative (< 0.1)" = "#FFFFCC",
        "Low (0.1 - 0.5)" = "#FED976",
        "Intermediate (0.5 - 1)" = "#FD8D3C",
        "High (> 1)" = "#BD0026"
      ),
      na.value = "grey80",
      guide = ggplot2::guide_legend(order = 1)
    ) +

    # Add MRD annotations at the final recorded date
    ggplot2::geom_point(
      data = mrd_terminal_pts,
      ggplot2::aes(
        x = rel_term_dat,
        y = y,
        fill = mrd_category
      ),
      shape = 22,
      size = 1,
      color = "black"
    ) +

    ggnewscale::new_scale_fill() +

    # Add outcome annotations (MANDATORY) — text labels on the plot
    ggplot2::geom_text(
      data = outcome_pts |>
        dplyr::filter(
          .data$outcome %in% c(
            "Relapse",
            "Nonrelapse mortality",
            "Other exclusion reason"
          )
        ),
      ggplot2::aes(
        x = rel_term_dat + 5,
        y = y,
        label = dplyr::case_when(
          outcome == "Relapse" ~ "R",
          outcome == "Nonrelapse mortality" ~ "\u00D7",
          outcome == "Other exclusion reason" ~ "*",
          TRUE ~ ""
        )
      ),
      hjust = -0.2,
      show.legend = FALSE
    ) +

    # Invisible points used solely to drive the Outcome legend
    ggplot2::geom_point(
      data = outcome_pts |>
        dplyr::filter(
          .data$outcome %in% c(
            "Relapse",
            "Nonrelapse mortality",
            "Other exclusion reason"
          )
        ),
      ggplot2::aes(
        x = rel_term_dat + 5,
        y = y,
        color = outcome
      ),
      shape = NA
    ) +

    ggplot2::scale_color_manual(
      name = "Outcome",
      values = c(
        "Relapse"                = "black",
        "Nonrelapse mortality"   = "black",
        "Other exclusion reason" = "black"
      ),
      labels = c(
        "Relapse"                = "R                Relapse",
        "Nonrelapse mortality"   = "×   Nonrelapse mortality",
        "Other exclusion reason" = "*  Other exclusion reason"
      ),
      guide = ggplot2::guide_legend(
        order = 3,
        keywidth = ggplot2::unit(0, "pt")
      )
    )

  # Add immune suppression line (OPTIONAL)
  if (!is.null(immune_pts) && nrow(immune_pts) > 0) {
    swimmer_plot <- swimmer_plot +
      ggplot2::geom_segment(
        data = immune_pts,
        ggplot2::aes(
          x = interval_start,
          xend = interval_end,
          y = y + 0.3,
          yend = y + 0.3,
          linetype = "Immune suppression"
        ),
        linewidth = 1.5,
        color = "brown"
      ) +
      ggplot2::scale_linetype_manual(
        name = NULL,
        values = c("Immune suppression" = "solid"),
        guide = ggplot2::guide_legend(
          order = 2,
          override.aes = list(
            linewidth = 2.5,
            color = "brown"
          )
        )
      ) +
      ggnewscale::new_scale_fill()
  }

  # Add treatment annotations (OPTIONAL)
  if (!is.null(treatment_pts) && nrow(treatment_pts) > 0) {
    swimmer_plot <- swimmer_plot +
      ggplot2::geom_point(
        data = treatment_pts |> dplyr::filter(!is.na(.data$treatment)),
        ggplot2::aes(
          x = .data$rel_treatment_dat,
          y = .data$y - 0.3,
          fill = .data$treatment
        ),
        color = "black",
        shape = 24
      ) +
      ggplot2::scale_fill_manual(
        name = "Treatment",
        values = c(
          "DLI" = "darkgrey",
          "Azacitidine" = "white"
        ),
        guide = ggplot2::guide_legend(order = 4)
      ) +
      ggnewscale::new_scale_fill()
  }

  # Add GVHD annotations (OPTIONAL)
  if (!is.null(gvhd_pts) && nrow(gvhd_pts) > 0) {
    swimmer_plot <- swimmer_plot +
      ggplot2::geom_point(
        data = gvhd_pts |> dplyr::filter(
          .data$gvhd == "Acute GVHD",
          .data$agvhdstage %in% c(3, 4)
        ),
        ggplot2::aes(
          x = .data$rel_gvhd_dat,
          y = .data$y - 0.3,
          fill = .data$agvhdstage
        ),
        color = "black",
        shape = 23
      ) +
      ggplot2::scale_fill_manual(
        name = "Acute GVHD",
        values = c(
          "3" = "#FF8A8A",
          "4" = "#D10000"
        ),
        guide = ggplot2::guide_legend(order = 5)
      ) +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_point(
        data = gvhd_pts |> dplyr::filter(
          .data$gvhd == "Chronic GVHD",
          .data$cgvhdstage %in% c("Moderate", "Severe")
        ),
        ggplot2::aes(
          x = .data$rel_gvhd_dat,
          y = .data$y - 0.3,
          fill = .data$cgvhdstage
        ),
        color = "black",
        shape = 23
      ) +
      ggplot2::scale_fill_manual(
        name = "Chronic GVHD",
        values = c(
          "Moderate" = "#27D6F5",
          "Severe"   = "#5B27F5"
        ),
        guide = ggplot2::guide_legend(order = 6)
      )
  }

  swimmer_plot <- swimmer_plot +
    # Add graph title and axis labels
    ggplot2::labs(
      x = "Days from transplantation",
      y = "Patient",
      title = title_string
    ) +

    # Start the x-axis at 0, equivalent to the date of transplantation
    ggplot2::xlim(0, NA) +

    ggplot2::scale_y_continuous(
      breaks = unique(plot_data$y),
      labels = unique(plot_data$patno),
      expand = ggplot2::expansion(add = c(1, 1))
    ) +

    ggplot2::theme_classic()

  return(swimmer_plot)
}


#' Draw swimmer plot and export to PNG.
#'
#' @param processed A list of data frames containing the processed data
#' for general_info, treatment, mrd, gvhd, immune_intervals, and ngs.
#' @param patient_subset A vector of patient IDs.
#' @param output_folder The folder where the output plot will be saved.
#' @param output_filename The name of the output plot file.
#' @param output_format Output format string, "svg" or "pdf".
#' @returns A numeric vector.
#' @examples
#' \dontrun{
#' processed <- list(
#'   general_info = data.frame(
#'     patno = 1,
#'     rel_term_dat = 100,
#'     outcome = "Remission"
#'   ),
#'   treatment = data.frame(
#'     patno = 1,
#'     treatment = "Azacitidine",
#'     rel_treatment_dat = 20
#'   ),
#'   mrd = data.frame(
#'     patno = 1,
#'     rel_mrd_dat = 0,
#'     mrd_category = "Negative (< 0.1)",
#'     rel_term_dat = 100
#'   ),
#'   gvhd = data.frame(patno = 1),
#'   immune_intervals = data.frame(patno = 1),
#'   ngs = data.frame(patno = 1)
#' )
#' draw_swimmerplot(
#' processed,
#' patient_subset = c(1),
#' output_folder = tempdir(),
#' output_filename = "swimmerplot",
#' output_format = "svg"
#' )
#' }
draw_swimmerplot <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_filename = "swimmerplot.svg",
  output_format = c("svg", "pdf")
) {

  output_format <- match.arg(output_format)

  processed$general_info <- ensure_data_frame_columns(
    processed$general_info,
    c("patno", "rel_term_dat", "outcome")
  )
  processed$mrd <- ensure_data_frame_columns(
    processed$mrd,
    c("patno", "rel_mrd_dat", "mrd_category", "rel_term_dat")
  )
  processed$treatment <- ensure_data_frame_columns(
    processed$treatment,
    c("patno", "treatment", "rel_treatment_dat")
  )
  processed$gvhd <- ensure_data_frame_columns(
    processed$gvhd,
    c("patno", "gvhd", "agvhdstage", "cgvhdstage", "rel_gvhd_dat")
  )
  processed$immune_intervals <- ensure_data_frame_columns(
    processed$immune_intervals,
    c("patno", "interval_start", "interval_end")
  )
  processed$ngs <- ensure_data_frame_columns(
    processed$ngs,
    c("patno", "mutlist")
  )

  if (nrow(processed$general_info) == 0) {
    stop("No patients available after filtering.")
  }

  if (!is.null(patient_subset)) {

    processed$general_info <-
      processed$general_info |>
      dplyr::filter(patno %in% patient_subset)

    processed$treatment <-
      processed$treatment |>
      dplyr::filter(patno %in% patient_subset)

    processed$mrd <-
      processed$mrd |>
      dplyr::filter(patno %in% patient_subset)

    processed$gvhd <-
      processed$gvhd |>
      dplyr::filter(patno %in% patient_subset)

    processed$immune_intervals <-
      processed$immune_intervals |>
      dplyr::filter(patno %in% patient_subset)

    processed$ngs <-
      processed$ngs |>
      dplyr::filter(patno %in% patient_subset)
  }

  title_string <- "All patients"

  # --- MRD RECTANGLES ---

  mrd_base <- processed$mrd |>
    dplyr::select(
      patno,
      rel_mrd_dat,
      mrd_category,
      rel_term_dat
    ) |>
    dplyr::distinct() |>
    # Ensure MRD measurements occurring after the recorded end/relapse
    # date are not plotted. Keep rows where rel_term_dat is NA (no end
    # date) or where the MRD date is on or before rel_term_dat.
    dplyr::filter(is.na(rel_term_dat) | rel_mrd_dat <= rel_term_dat) |>
    dplyr::arrange(patno, rel_mrd_dat)

  # Perform following calculations on a per-patient basis
  mrd_rectangles <- mrd_base |>
    dplyr::group_by(patno) |>
    dplyr::mutate(
      xmin = rel_mrd_dat,
      xmax = dplyr::coalesce(
        dplyr::lead(rel_mrd_dat),
        dplyr::first(rel_term_dat)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      patno, xmin, xmax, mrd_category, rel_term_dat
    ) |>
    dplyr::bind_rows(
      mrd_base |>
        dplyr::group_by(patno) |>
        dplyr::slice(1) |>
        dplyr::transmute(
          patno,
          xmin = 0,
          xmax = rel_mrd_dat,
          mrd_category = dplyr::if_else(
            rel_mrd_dat == 0, mrd_category, NA
          ),
          rel_term_dat
        ) |>
        dplyr::ungroup()
    ) |>
    dplyr::filter(xmin != xmax) |>
    dplyr::arrange(patno, xmin) |>
    dplyr::group_by(patno) |>
    dplyr::mutate(rect_index = dplyr::row_number()) |>
    dplyr::ungroup()

  # Add patients without MRD measurements but with a transplant and end date
  # as a single long (grey) rectangle from 0 to rel_term_dat
  missing_mrd <- processed$general_info |>
    dplyr::select(patno, rel_term_dat) |>
    dplyr::filter(!is.na(rel_term_dat)) |>
    dplyr::anti_join(dplyr::distinct(mrd_rectangles, patno), by = "patno") |>
    dplyr::mutate(
      xmin = 0,
      xmax = rel_term_dat,
      mrd_category = NA
    ) |>
    dplyr::select(patno, xmin, xmax, mrd_category, rel_term_dat)

  if (nrow(missing_mrd) > 0) {
    mrd_rectangles <- dplyr::bind_rows(mrd_rectangles, missing_mrd) |>
      dplyr::arrange(patno, xmin) |>
      dplyr::group_by(patno) |>
      dplyr::mutate(rect_index = dplyr::row_number()) |>
      dplyr::ungroup()
  }

  # Calculate mrd_terminal
  mrd_terminal <- mrd_base |> dplyr::filter(
    rel_mrd_dat == rel_term_dat
  )

  # --- PLOT DATA & LOOKUP ---

  plot_data <- mrd_rectangles |>
    dplyr::group_by(patno) |>
    dplyr::mutate(max_end_event = dplyr::first(rel_term_dat)) |>
    dplyr::ungroup() |>
    dplyr::arrange(max_end_event, patno, xmin) |>
    dplyr::mutate(
      patno_factor = factor(patno, levels = unique(patno)),
      y = as.numeric(patno_factor),
      ymin = y,
      ymax = y
    )

  patient_y <- dplyr::distinct(dplyr::select(plot_data, patno, y))

  # Pre-build all annotation datasets once, outside ggplot()
  mrd_terminal_pts <- mrd_terminal |>
    dplyr::left_join(patient_y, by = "patno")
  outcome_pts <- processed$general_info |>
    dplyr::select(patno, rel_term_dat, outcome) |>
    dplyr::distinct() |>
    dplyr::left_join(patient_y, by = "patno")
  gvhd_pts <- processed$gvhd |>
    dplyr::distinct() |>
    dplyr::left_join(patient_y, by = "patno")
  treatment_pts <- processed$treatment |>
    dplyr::distinct() |>
    dplyr::left_join(patient_y, by = "patno")
  immune_pts <- processed$immune_intervals |>
    dplyr::left_join(patient_y, by = "patno")

  # --- SWIMMER PLOT FUNCTION ---

  # Run plotting function
  swimmer_plot <- swimmerplot(
    plot_data = plot_data,
    outcome_pts = outcome_pts,
    mrd_terminal_pts = mrd_terminal_pts,
    title_string = title_string,
    immune_pts = immune_pts,
    treatment_pts = treatment_pts,
    gvhd_pts = gvhd_pts
  )

  # --- EXPORT ---
  out_filename <- file.path(
    output_folder,
    paste0(tools::file_path_sans_ext(output_filename), ".", output_format)
  )

  ggplot2::ggsave(
    filename = out_filename,
    plot = swimmer_plot,
    device = output_format,
    width = 6,
    height = max(5, length(unique(plot_data$patno)) * 0.2),
    units = "in",
    dpi = 300,
    bg = "white"
  )

}
