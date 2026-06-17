#' Called by draw_swimmerplot to draw swimmer plot.
#'
#' @param plot_data Data frame containing
#' the data for the swimmer plot rectangles.
#' @param immune_pts A vector of patient IDs.
#' @param outcome_pts A vector of patient IDs.
#' @param treatment_pts A vector of patient IDs.
#' @param gvhd_pts A vector of patient IDs.
#' @param title_string The title for the swimmer plot.
#' @returns A swimmer plot ggplot object.
#' @examples
#' swimmerplot(
#' plot_data,
#' immune_pts,
#' outcome_pts,
#' treatment_pts,
#' gvhd_pts,
#' title_string
#' )
swimmerplot <- function(
  plot_data,
  immune_pts,
  outcome_pts,
  treatment_pts,
  gvhd_pts,
  mrd_terminal_pts,
  title_string
) {
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
      name = "MRD category",
      values = c(
        "Negative (< 0.1)" = "#FFFFFF",
        "Low (0.1 - 0.5)" = "#FFD65C",
        "Intermediate (0.5 - 1)" = "#FF9800",
        "High (> 1)" = "#F21C0D"
      ),
      na.value = "lightgrey",
      guide = ggplot2::guide_legend(order = 1)
    ) +

    # Add immune suppression line
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

    # Add immune suppression legend
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("Immune suppression" = "solid"),
      guide = ggplot2::guide_legend(
        order = 5,
        override.aes = list(
          linewidth = 2.5,
          color = "brown"
        )
      )
    ) +

    # Add relapse annotation
    ggplot2::geom_text(data = dplyr::filter(
      outcome_pts,
      outcome == "Relapse"
    ), ggplot2::aes(
      x = rel_term_dat + 5,
      y = y,
      label = "R"
    ),
    hjust = -0.2
    ) +

    # Add nonrelapse mortality annotation
    ggplot2::geom_text(data = dplyr::filter(
      outcome_pts,
      outcome == "Nonrelapse mortality"
    ), ggplot2::aes(
      x = rel_term_dat + 5,
      y = y,
      label = "\u2020"
    ),
    hjust = -0.2
    ) +

    # Add Other exclusion reason annotation
    ggplot2::geom_text(data = dplyr::filter(
      outcome_pts,
      outcome == "Other exclusion reason"
    ), ggplot2::aes(
      x = rel_term_dat + 5,
      y = y,
      label = "*"
    ),
    hjust = -0.2
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

    # Add treatment annotations
    ggplot2::geom_point(
      data = treatment_pts |> dplyr::filter(!is.na(treatment)),
      ggplot2::aes(
        x = rel_treatment_dat,
        y = y - 0.3,
        fill = treatment
      ),
      color = "black",
      shape = 24
    ) +

    # Add treatment legend
    ggplot2::scale_fill_manual(
      name = "Treatment",
      values = c(
        "DLI" = "darkgrey",
        "Azacitidine" = "white"
      ),
      guide = ggplot2::guide_legend(order = 2)
    ) +

    ggnewscale::new_scale_fill() +

    # Add acute GVHD annotation
    ggplot2::geom_point(
      data = gvhd_pts |> dplyr::filter(
        gvhd == "Acute GVHD",
        agvhdstage %in% c(3, 4)
      ),
      ggplot2::aes(
        x = rel_gvhd_dat,
        y = y - 0.3,
        fill = agvhdstage
      ),
      color = "black",
      shape = 23
    ) +

    # Add acute GVHD legend
    ggplot2::scale_fill_manual(
      name = "Acute GVHD",
      values = c(
        "3" = "#FF8A8A",
        "4" = "#D10000"
      ),
      guide = ggplot2::guide_legend(order = 3)
    ) +

    ggnewscale::new_scale_fill() +

    # Add chronic GVHD annotation
    ggplot2::geom_point(
      data = gvhd_pts |> dplyr::filter(
        gvhd == "Chronic GVHD",
        cgvhdstage %in% c("Moderate", "Severe")
      ),
      ggplot2::aes(
        x = rel_gvhd_dat,
        y = y - 0.3,
        fill = cgvhdstage
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
      guide = ggplot2::guide_legend(order = 4)
    ) +

    # Add graph title and axis labels
    ggplot2::labs(
      x = "Days from transplantation",
      y = "Patient",
      title = "NMDS14B Part 2",
      subtitle = title_string
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
#' @param output_folder The folder where the output PNG will be saved.
#' @param output_filename The name of the output PNG file.
#' @returns A numeric vector.
#' @examples
#' draw_swimmerplot(processed, patient_subset, "~/output", "swimmerplot.png")
draw_swimmerplot <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_filename = "swimmerplot.png"
) {

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

  print(mrd_rectangles, n = Inf, width = Inf)

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
    plot_data,
    immune_pts,
    outcome_pts,
    treatment_pts,
    gvhd_pts,
    mrd_terminal_pts,
    title_string
  )

  # --- EXPORT ---
  ggplot2::ggsave(
    filename = file.path(output_folder, output_filename),
    plot = swimmer_plot,
    width = 6,
    height = max(5, length(unique(plot_data$patno)) * 0.2),
    units = "in",
    dpi = 300,
    bg = "white"
  )

}
