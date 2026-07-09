#' Generate MRD + chimerism plot for a given patient.
#'
#' @param mrd_data Mandatory argument:
#' a data frame containing longitudinal MRD data.
#' @param general_info_data Mandatory argument:
#' a data frame containing general patient info data.
#' @param ngs_data A data frame containing ngs data.
#' @param chimerism_data A data frame containing chimerism data.
#' @param x_range The range of the x-axis.
#' @param y_upper The upper limit of the y-axis.
#' @param pat_id The patient id of the patient you want to plot.
#' @returns A ggplot object representing the MRD plot for the patient.
#' @examples
#' draw_chimerism_plot(
#'   d$mrd,
#'   d$general_info,
#'   d$ngs_data,
#'   d$chimerism_data,
#'   x_range,
#'   y_upper,
#'   pat_id
#' )
draw_chimerism_plot <- function(
  mrd_data, # Mandatory
  general_info_data, # Mandatory
  ngs_data = NULL, # Optional
  chimerism_data, # Mandatory
  x_range, # Mandatory
  y_upper, # Mandatory
  pat_id # Mandatory
) {

  diagnosis_label <- if (
    "mdsdiagnosis" %in% names(general_info_data) &&
    length(general_info_data$mdsdiagnosis) > 0 &&
    !is.na(general_info_data$mdsdiagnosis[1])
  ) {
    general_info_data$mdsdiagnosis[1]
  } else {
    "Not available"
  }

  ipssm_label <- if (
    "ipssm_title" %in% names(general_info_data) &&
    length(general_info_data$ipssm_title) > 0 &&
    !is.na(general_info_data$ipssm_title[1])
  ) {
    general_info_data$ipssm_title[1]
  } else {
    "Not available"
  }

  karyotype_label <- if (
    "karyotyp" %in% names(general_info_data) &&
    length(general_info_data$karyotyp) > 0 &&
    !is.na(general_info_data$karyotyp[1])
  ) {
    general_info_data$karyotyp[1]
  } else {
    "Not available"
  }

  ngs_label <- if (
    !is.null(ngs_data) &&
    nrow(ngs_data) > 0 &&
    "mutlist" %in% names(ngs_data) &&
    !is.na(ngs_data$mutlist[1])
  ) {
    ngs_data$mutlist[1]
  } else {
    "Not available"
  }

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
        y = chimerism / 10, # Divide chimerism by 10.
        colour = surface_marker
      ),
      linetype = "dashed"
    ) +

    # Add CHIMERISM points, including those with only one data point.
    ggplot2::geom_point(data = chimerism_data, ggplot2::aes(
      x = rel_chimerism_dat,
      y = chimerism / 10, # Divide chimerism by 10.
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
      y_upper
    ),
    breaks = c(0.1, 1, 10, 100),
    sec.axis = ggplot2::sec_axis(
      trans = ~. * 10,
      breaks = ggplot2::derive(),
      name = "Chimerism (%)"
    ), # Multiply the second axis by 10.
    labels = scales::label_number()
    ) +

    # Add clinical information title, based on general_info_data and ngs_data
    ggplot2::labs(title = paste0(
      "Patient: ",
      pat_id
    ),
    subtitle = paste0(
      "Diagnosis: ",
      diagnosis_label,
      "\nIPSS-M: ",
      ipssm_label,
      "\nKaryotype: ",
      karyotype_label,
      "\nNGS: ",
      ngs_label
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
      hjust = 0
    ) +

    # Define the legend position, title size and subtitle size
    ggplot2::theme(legend.position = "right",
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 9)
    )

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
  y_upper <- y_limit_finder(d$mrd, d$chimerism)

  # ----------------------------
  # Chimerism plot (middle)
  # ----------------------------

  # Draw chimerism plot (pass chimerism data first, then general info)
  chimerism_plot <- draw_chimerism_plot(
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
  chimerism_plot_clean <- chimerism_plot +
    ggplot2::theme(legend.position = "none")

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
#' @param output_format Output format string, "svg" or "pdf".
#' @returns A numeric vector.
#' @examples
#' draw_clinical_course_chimerism(
#' processed,
#' patient_subset,
#' "~/output",
#' "clinical_course.pdf",
#' output_format = "pdf"
#' )
draw_clinical_course_chimerism <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_filename = "clinical_course_chimerism_plots.pdf",
  output_format = c("svg", "pdf")
) {

  output_format <- match.arg(output_format)

  processed$general_info <- ensure_data_frame_columns(
    processed$general_info,
    c(
      "patno", "rel_term_dat", "outcome", "mdsdiagnosis",
      "karyotyp", "deathcause", "ipssm_title"
    )
  )
  processed$mrd <- ensure_data_frame_columns(
    processed$mrd,
    c("patno", "rel_mrd_dat", "level", "Mutation")
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
    c(
      "patno", "interval_start", "interval_end", "drugname_standardized",
      "dose_percentage", "drugstopped"
    )
  )
  processed$ngs <- ensure_data_frame_columns(
    processed$ngs,
    c("patno", "mutlist")
  )
  processed$chimerism <- ensure_data_frame_columns(
    processed$chimerism,
    c("patno", "rel_chimerism_dat", "chimerism", "surface_marker")
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

    processed$chimerism <-
      processed$chimerism |>
      dplyr::filter(patno %in% patient_subset)

  }

  # To achieve a log10 y axis scale, convert the 0 values in level to 0.08
  processed$mrd <- processed$mrd |>
    dplyr::mutate(level_no0s = ifelse(level == 0, 0.08, level))

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  patient_ids <- unique(processed$general_info$patno)

  if (output_format == "svg") {
    base_name <- tools::file_path_sans_ext(output_filename)
    for (p in patient_ids) {
      svg_filename <- file.path(
        output_folder,
        paste0(base_name, "_", p, ".svg")
      )
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