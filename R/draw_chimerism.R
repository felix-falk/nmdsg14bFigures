draw_chimerism_plot_based_on_mrd_plot <- function(
  mrd_data,
  general_info_data,
  ngs_data,
  chimerism_data,
  x_range,
  y_upper,
  pat_id
) {

  plot <- ggplot2::ggplot() +

    # Add a shaded rectangle to indicate the MRD negative range (below 0.1 %)
    ggplot2::annotate("rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 0.08,
      ymax = 0.1,
      fill = "lightgrey",
      alpha = 0.4
    ) +

    # Add MRD lines, only for mutations with more than 1 data point.
    ggplot2::geom_line(
      data = mrd_data |>
        dplyr::filter(!is.na(Mutation)) |>
        dplyr::group_by(Mutation) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      ggplot2::aes(
        x = rel_mrd_dat,
        y = level_no0s,
        colour = Mutation
      )
    ) +

    # Add MRD points, including those with only one data point.
    ggplot2::geom_point(data = mrd_data, ggplot2::aes(
      x = rel_mrd_dat,
      y = level_no0s,
      colour = Mutation
    )
    ) +

    # Add CHIMERISM lines, only for those with more than 1 data point.
    ggplot2::geom_line(
      data = chimerism_data |>
        dplyr::filter(!is.na(surface_marker)) |>
        dplyr::group_by(surface_marker) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      ggplot2::aes(
        x = rel_chimerism_dat,
        y = chimerism,
        colour = surface_marker
      )
    ) +

    # Add CHIMERISM points, including those with only one data point.
    ggplot2::geom_point(data = chimerism_data, ggplot2::aes(
      x = rel_chimerism_dat,
      y = chimerism,
      colour = surface_marker
    )
    ) +

    # Set theme, adjust x and y labels, set color of MRD lines and points
    ggplot2::theme_minimal() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("VAF (%)") +
    ggplot2::scale_colour_brewer(palette = "Set1", na.translate = FALSE) +

    # Set x and y axis limits based on x_range and y_upper parameters
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::scale_y_log10(limits = c(
      0.08,
      100 # Changed from y_upper
    ), labels = scales::label_number()) +

    # Add clinical information title, based on general_info_data and ngs_data
    ggplot2::labs(title = paste0(
      "Patient: ",
      pat_id
    ),
    subtitle = paste0(
      "Diagnosis: ",
      general_info_data$mdsdiagnosis[1],
      "\nIPSS-M: ",
      general_info_data$ipssm_title[1],
      "\nKaryotype: ",
      general_info_data$karyotyp[1],
      "\nNGS: ",
      ngs_data$mutlist[1]
    )
    ) +

    # Add vertical line at the time of relapse
    geomtextpath::geom_textvline(
      data = general_info_data |>
        dplyr::filter(outcome == "Relapse"),
      ggplot2::aes(
        xintercept = rel_term_dat,
        label = "Relapse"
      )
    ) +

    # Add vertical line at the time of nonrelapse mortality
    geomtextpath::geom_textvline(
      data = general_info_data |> dplyr::filter(
        outcome == "Nonrelapse mortality"
      ),
      ggplot2::aes(
        xintercept = rel_term_dat,
        label = paste0("Death: ", deathcause)
      )
    ) +

    # Add horisontal line at the MRD positive threshold of 0.1
    geomtextpath::geom_texthline(
      yintercept = 0.1,
      label = "MRD Threshold",
      linetype = "dashed",
      color = "darkgrey",
      size = 3,
      vjust = -0.2,
      hjust = 1
    ) +

    # Define the legend position, title size and subtitle size
    ggplot2::theme(legend.position = "right",
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 9)
    )

  return(plot)

}

#' Generate chimerism plot for a given patient
#'
#' @param data A data frame containing longitudinal MRD data
#' @param x_range The range of the x-axis.
#' @param y_upper The upper limit of the y-axis.
#' @returns A ggplot object representing the MRD plot for the patient.
#' @examples
#' draw_mrd_plot(d$mrd)
draw_chimerism_plot <- function(
  mrd_data,
  chimerism_data,
  general_info_data,
  x_range,
  pat_id
) {

  # Keep only CD33 and CD34 chimerism data, change to logarithmic scale
  chimerism_data <- chimerism_data |>
    dplyr::filter(surface_marker %in% c("CD33BM", "CD34BM"))

  # Rename chimerism_data columns: surface marker -> Mutation, chimerism -> level
  chimerism_data_2 <- chimerism_data |>
    dplyr::rename(
      Mutation = surface_marker,
      level = chimerism,
      rel_mrd_dat = rel_chimerism_dat
    )

  # To achieve a log10 y axis scale, convert the 0 values in level to 0.08
  chimerism_data_2 <- chimerism_data_2 |>
    dplyr::mutate(level_no0s = ifelse(level == 0, 0.08, level))

  # Merge mrd_data and chimerism_data_2 into one data frame, using patno, Mutation and level
  mrd_chimerism_data <- mrd_data |>
    dplyr::left_join(chimerism_data_2, by = c("patno", "Mutation", "level_no0s", "rel_mrd_dat"))

  print(mrd_chimerism_data)

  plot <- ggplot2::ggplot() +

    # Add chimerism lines, only for surface markers with more than 1 data point
    ggplot2::geom_line(
      data = mrd_chimerism_data |>
        dplyr::filter(!is.na(level_no0s)) |>
        dplyr::group_by(level_no0s) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      ggplot2::aes(
        x = rel_mrd_dat,
        y = level_no0s,
        colour = Mutation
      )
    ) +

    # Add chimerism points, including those with only one data point
    ggplot2::geom_point(data = mrd_chimerism_data, ggplot2::aes(
      x = rel_mrd_dat,
      y = level_no0s,
      colour = Mutation
    )
    ) +

    # Set theme, adjust x and y labels, set color of chimerism lines and points
    ggplot2::theme_minimal() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Chimerism (%)") +
    ggplot2::scale_colour_brewer(palette = "Set2", na.translate = FALSE) +

    # Set x axis limits based on x_range
    ggplot2::scale_x_continuous(limits = x_range) +

    # Define the legend position, title size and subtitle size
    ggplot2::theme(legend.position = "right",
      plot.title = ggplot2::element_text(size = 12)
    ) +

    # Change y axis to log scale
    ggplot2::scale_y_log10()

  return(plot)

}

#' Generate MRD + Chimerism + GVHD timeline for a given patient
#'
#' @param processed A list of data frames containing the processed data
#' for general_info, treatment, mrd, gvhd, immune_intervals, chimerism and ngs.
#' @param pat_id A vector of patient IDs.
#' @returns A numeric vector.
#' @examples
#' plot_patient_timeline(processed, pat_id)
plot_chimerism_timeline <- function(processed, pat_id) {

  # Select one patient
  d <- lapply(processed, function(x) select_one_patient(x, pat_id))

  # Find the MRD graph x-axis range.
  x_range <- x_range_finder(d$general_info)

  # Find the MRD graph y-axis upper limit.
  # y_upper <- y_limit_finder(d$mrd)
  y_upper <- 100

  # ----------------------------
  # MRD plot (top)
  # ----------------------------

  # Draw MRD plot
  mrd_plot <- draw_mrd_plot(d$mrd, d$general_info, d$ngs, x_range, y_upper, pat_id)

  # Extract mrd legend
  mrd_legend <- cowplot::get_legend(mrd_plot)

  # Remove mrd legend from mrd_plot
  mrd_plot_clean <- mrd_plot + ggplot2::theme(legend.position = "none")

  # ----------------------------
  # Chimerism plot (middle)
  # ----------------------------

  # Draw chimerism plot (pass chimerism data first, then general info)
  chimerism_plot <- draw_chimerism_plot_based_on_mrd_plot(
    d$mrd,
    d$general_info,
    d$ngs,
    d$chimerism,
    x_range,
    y_upper,
    pat_id
  )

  # Extract chimerism legend
  chimerism_legend <- cowplot::get_legend(chimerism_plot)

  # Remove chimerism legend from chimerism_plot
  chimerism_plot_clean <- chimerism_plot + ggplot2::theme(legend.position = "none")

  # ----------------------------
  # GVHD / IS events plot (bottom)
  # ----------------------------

  events_plot <- draw_events_plot(
    d$gvhd,
    d$immune_intervals,
    d$treatment,
    x_range
  )

  # Combine MRD + Chimerism + events vertically
  combined_plots <- cowplot::plot_grid(
    mrd_plot_clean,
    chimerism_plot_clean,
    events_plot,
    ncol = 1,
    rel_heights = c(2, 1, 1),
    align = "v",
    axis = "tblr"
  )

  # Create GVHD dummy legends locally (so they are available in this function)
  agvhd_colours <- c("0" = "#EBEBEB",
                     "1" = "#EDC0C0",
                     "2" = "#FF7878",
                     "3" = "#D42626",
                     "4" = "#800000")
  cgvhd_colours <- c("None" = "#EBEBEB",
                     "Mild" = "#AA88BB",
                     "Moderate" = "#622BD6",
                     "Severe" = "#290088")

  agvhd_legend_grob <- make_dummy_legend(
    names(agvhd_colours), agvhd_colours, "aGVHD Stage"
  )
  cgvhd_legend_grob <- make_dummy_legend(
    names(cgvhd_colours), cgvhd_colours, "cGVHD Stage"
  )

  # Combine all legends vertically
  combined_legends <- cowplot::plot_grid(
    mrd_legend,
    chimerism_legend,
    agvhd_legend_grob,
    cgvhd_legend_grob,
    ncol = 1,
    align = "v"
  )

  # Final combined plot
  final_plot <- cowplot::plot_grid(
    combined_plots,
    combined_legends,
    ncol = 2,
    rel_widths = c(3, 1),
    align = "v"
  )
  return(final_plot)
}

#' Draw clinical course figures and export to PDF.
#'
#' @param processed A list of data frames containing the processed data
#' for general_info, treatment, mrd, gvhd, immune_intervals, and ngs.
#' @param patient_subset A vector of patient IDs.
#' @param output_folder The folder where the output PDF will be saved.
#' @param output_filename The name of the output PDF file.
#' @returns A numeric vector.
#' @examples
#' draw_clinical_course(
#' processed,
#' patient_subset,
#' "~/output",
#' "clinical_course.pdf"
#' )
draw_clinical_course_chimerism <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_filename = "clinical_course_chimerism_plots.pdf",
  output_format = c("svg", "pdf")
) {

  output_format <- match.arg(output_format)

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

    processed$chimerism <-
      processed$chimerism |>
      dplyr::filter(patno %in% patient_subset)

  }

  # To achieve a log10 y axis scale, convert the 0 values in level to 0.09
  processed$mrd <- processed$mrd |>
    dplyr::mutate(level_no0s = ifelse(level == 0, 0.08, level))

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  patient_ids <- unique(processed$general_info$patno)

  if (output_format == "svg") {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("The 'svglite' package is required to export per-patient SVGs.\nPlease run: install.packages('svglite') and try again.")
    }
    base_name <- tools::file_path_sans_ext(output_filename)
    for (p in patient_ids) {
      svg_filename <- file.path(output_folder, paste0(base_name, "_", p, ".svg"))
      svglite::svglite(file = svg_filename, width = 10, height = 6)
      print(plot_chimerism_timeline(processed, p))
      grDevices::dev.off()
    }
  } else {
    # Collect all patient plots into a single PDF
    pdf_filename <- file.path(output_folder, output_filename)
    grDevices::pdf(file = pdf_filename, width = 10, height = 6)
    for (p in patient_ids) {
      print(plot_chimerism_timeline(processed, p))
    }
    grDevices::dev.off()
  }

}