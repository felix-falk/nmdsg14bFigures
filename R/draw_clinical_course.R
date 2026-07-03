#' Generate MRD plot for a given patient
#'
#' @param mrd_data A data frame containing longitudinal MRD data.
#' @param general_info_data A data frame containing general patient information.
#' @param ngs_data A data frame containing NGS mutation data.
#' @param x_range The range of the x-axis.
#' @param y_upper The upper limit of the y-axis.
#' @param pat_id A patient identifier.
#' @returns A ggplot object representing the MRD plot for the patient.
#' @examples
#' \dontrun{
#' mrd_data <- data.frame(
#'   rel_mrd_dat = c(0, 30),
#'   level_no0s = c(0.2, 0.1),
#'   Mutation = c("TP53", "TP53")
#' )
#' general_info_data <- data.frame(
#'   mdsdiagnosis = "MDS",
#'   ipssm_title = "High",
#'   karyotyp = "del(5q)",
#'   outcome = "Remission",
#'   rel_term_dat = 100,
#'   deathcause = NA_character_
#' )
#' ngs_data <- data.frame(mutlist = "TP53")
#' draw_mrd_plot(mrd_data, general_info_data, ngs_data, c(0, 100), 1, "P1")
#' }
draw_mrd_plot <- function(
  mrd_data,
  general_info_data,
  ngs_data,
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
      hjust = 0
    ) +

    # Define the legend position, title size and subtitle size
    ggplot2::theme(legend.position = "right",
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 9)
    )

  return(plot)

}

#' Generate events plot for a given patient
#'
#' @param gvhd_data A data frame containing longitudinal GVHD data
#' @param immune_intervals_data A data frame containing
#' immune suppression intervals
#' @param treatment_data A data frame containing treatment data
#' @param x_range The range of the x-axis.
#' @returns A ggplot object representing the events plot for the patient.
#' @examples
#' draw_events_plot(d$gvhd, d$immune_intervals, d$treatment, x_range)
draw_events_plot <- function(
  gvhd_data,
  immune_intervals_data,
  treatment_data,
  x_range
) {

  # Determine which event categories have data for this patient
  has_agvhd <- FALSE
  has_cgvhd <- FALSE
  if (!is.null(gvhd_data) && nrow(gvhd_data) > 0) {
    has_agvhd <- nrow(
      gvhd_data |> dplyr::filter(
        gvhd == "Acute GVHD" & !is.na(agvhdstage)
      )
    ) > 0
    has_cgvhd <- nrow(
      gvhd_data |> dplyr::filter(
        gvhd == "Chronic GVHD" & !is.na(cgvhdstage)
      )
    ) > 0
  }

  has_immune <- !is.null(
    immune_intervals_data
  ) && nrow(
    immune_intervals_data
  ) > 0

  has_aza <- FALSE
  has_dli <- FALSE
  if (!is.null(treatment_data) && nrow(treatment_data) > 0) {
    has_aza <- nrow(
      treatment_data |> dplyr::filter(treatment == "Azacitidine")
    ) > 0
    has_dli <- nrow(
      treatment_data |> dplyr::filter(treatment == "DLI")
    ) > 0
  }

  # Assign y positions dynamically in the desired order
  y_map <- list()
  current_y <- 1
  if (has_agvhd) {
    y_map$agvhd <- current_y
    current_y <- current_y + 1
  }
  if (has_cgvhd) {
    y_map$cgvhd <- current_y
    current_y <- current_y + 1
  }
  if (has_immune) {
    y_map$immune <- current_y
    current_y <- current_y + 1
  }
  if (has_aza) {
    y_map$aza <- current_y
    current_y <- current_y + 1
  }
  if (has_dli) {
    y_map$dli <- current_y
    current_y <- current_y + 1
  }

  # Build y-axis breaks and labels in the same order
  if (current_y == 1) {
    # No events present: provide a single empty y-axis level
    breaks <- c(1)
    labels <- c("")
    limits <- c(0.5, 1.5)
  } else {
    breaks <- seq_len(current_y - 1)
    labels <- c()
    if (has_agvhd) labels <- c(labels, "aGVHD")
    if (has_cgvhd) labels <- c(labels, "cGVHD")
    if (has_immune) labels <- c(labels, "Immune suppression")
    if (has_aza) labels <- c(labels, "Azacitidine")
    if (has_dli) labels <- c(labels, "DLI")
    limits <- c(0.5, (current_y - 1) + 0.5)
  }

  events_plot <- ggplot2::ggplot()

  # Add acute GVHD points and color scale if present
  if (has_agvhd) {
    events_plot <- events_plot +
      ggplot2::geom_point(
        data = gvhd_data |> dplyr::filter(
          gvhd == "Acute GVHD" & !is.na(agvhdstage)
        ),
        ggplot2::aes(
          x = rel_gvhd_dat,
          y = y_map$agvhd,
          colour = agvhdstage
        ),
        size = 3
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "0" = "#EBEBEB",
          "1" = "#EDC0C0",
          "2" = "#FF7878",
          "3" = "#D42626",
          "4" = "#800000"
        ),
        guide = "none"
      )
  }

  # Add chronic GVHD points and its color scale if present
  if (has_cgvhd) {
    events_plot <- events_plot +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_point(
        data = gvhd_data |> dplyr::filter(
          gvhd == "Chronic GVHD" & !is.na(cgvhdstage)
        ),
        ggplot2::aes(
          x = rel_gvhd_dat,
          y = y_map$cgvhd,
          colour = cgvhdstage
        ),
        size = 3
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "None" = "#EBEBEB",
          "Mild" = "#AA88BB",
          "Moderate" = "#622BD6",
          "Severe" = "#290088"
        ),
        guide = "none"
      )
  }

  # Immune suppression intervals (old function)
  #if (has_immune) {
  #  events_plot <- events_plot +
  #    ggplot2::geom_segment(
  #      data = immune_intervals_data,
  #      ggplot2::aes(
  #        x = interval_start,
  #        xend = interval_end,
  #        y = y_map$immune,
  #        yend = y_map$immune
  #      ),
  #      linewidth = 2,
  #      colour = "black"
  #    )
  #}

  # Immune suppression interval (new function)
  if (has_immune) {
    events_plot <- events_plot +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = immune_intervals_data$interval_start,
        xmax = immune_intervals_data$interval_end,
        ymin = immune_intervals_data$y_map$immune - 0.2,
        ymax = immune_intervals_data$y_map$immune + 0.2,
        fill = immune_intervals_data$dose_percentage
      ),
      color = "black"
      )
  }

  # Azacitidine
  if (has_aza) {
    events_plot <- events_plot +
      ggplot2::geom_point(
        data = treatment_data |> dplyr::filter(treatment == "Azacitidine"),
        ggplot2::aes(
          x = rel_treatment_dat,
          y = y_map$aza
        ),
        colour = "black",
        size = 3
      )
  }

  # DLI
  if (has_dli) {
    events_plot <- events_plot +
      ggplot2::geom_point(
        data = treatment_data |> dplyr::filter(treatment == "DLI"),
        ggplot2::aes(
          x = rel_treatment_dat,
          y = y_map$dli
        ),
        colour = "black",
        size = 3
      )
  }

  # Common labels, theme and axis settings
  events_plot <- events_plot +
    ggplot2::labs(x = "Days after transplantation", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits
    )

  return(events_plot)

}


#' Generate MRD + GVHD timeline for a given patient
#'
#' @param processed A list of data frames containing the processed data
#' for general_info, treatment, mrd, gvhd, immune_intervals, and ngs.
#' @param pat_id A vector of patient IDs.
#' @returns A numeric vector.
#' @examples
#' plot_patient_timeline(processed, pat_id)
plot_patient_timeline <- function(processed, pat_id) {

  # Select one patient
  d <- lapply(processed, function(x) select_one_patient(x, pat_id))

  # Find the MRD graph x-axis range.
  x_range <- x_range_finder(d$general_info)

  # Find the MRD graph y-axis upper limit.
  y_upper <- y_limit_finder(d$mrd)

  # ----------------------------
  # MRD plot (top)
  # ----------------------------

  # Draw MRD plot
  mrd_plot <- draw_mrd_plot(
    d$mrd,
    d$general_info,
    d$ngs,
    x_range,
    y_upper,
    pat_id
  )

  # Extract mrd legend
  mrd_legend <- cowplot::get_legend(mrd_plot)

  # Remove mrd legend from mrd_plot
  mrd_plot_clean <- mrd_plot + ggplot2::theme(legend.position = "none")

  # ----------------------------
  # GVHD / IS events plot (bottom)
  # ----------------------------

  events_plot <- draw_events_plot(
    d$gvhd,
    d$immune_intervals,
    d$ciclosporine_intervals,
    d$treatment,
    x_range
  )

  # Combine MRD + events vertically
  combined_plots <- cowplot::plot_grid(
    mrd_plot_clean,
    events_plot,
    ncol = 1,
    rel_heights = c(2, 1),
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
    rel_widths = c(4, 1),
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
#' draw_clinical_course(
#' processed,
#' patient_subset,
#' "~/output",
#' "clinical_course.pdf"
#' )
draw_clinical_course <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_filename = "clinical_course_plots.pdf",
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

  }

  # To achieve a log10 y axis scale, convert the 0 values in level to 0.09
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
      print(plot_patient_timeline(processed, p))
      grDevices::dev.off()
    }
  } else {
    # Collect all patient plots into a single PDF
    pdf_filename <- file.path(output_folder, output_filename)
    grDevices::pdf(file = pdf_filename, width = 10, height = 6)
    for (p in patient_ids) {
      print(plot_patient_timeline(processed, p))
    }
    grDevices::dev.off()
  }

}
