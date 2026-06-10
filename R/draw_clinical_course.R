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
  d <- lapply(processed, select_one_patient(df))

  # Determine the range of the x axis
  x_end <- d$general_info$rel_term_dat[1]

  # Fallback if rel_term_dat does not exist
  if (is.na(x_end) || !is.finite(x_end)) {
    x_end <- 365
  }

  # Add 10 days to the upper x-axis limit
  x_range <- c(0, x_end)

  # Determine MRD y-axis range
  if (nrow(d$mrd) == 0 ||
        all(is.na(d$mrd$level_no0s))) {
    y_upper <- 10
  } else {
    y_upper <- max(
      10,
      ceiling(max(d$mrd$level_no0s, na.rm = TRUE))
    )
  }

  # ----------------------------
  # MRD plot (top)
  # ----------------------------
  mrd_plot <- ggplot2::ggplot() +

    ggplot2::annotate("rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 0.08,
      ymax = 0.1,
      fill = "lightgrey",
      alpha = 0.4
    ) +

    ggplot2::geom_line(
      data = d$mrd |>
        dplyr::filter(!is.na(d$mrd$Mutation)) |>
        dplyr::group_by(d$mrd$Mutation) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      ggplot2::aes(
        x = d$mrd$rel_mrd_dat,
        y = d$mrd$level_no0s,
        colour = d$mrd$Mutation
      )
    ) +

    ggplot2::geom_point(data = d$mrd, ggplot2::aes(
      x = rlang::.data$rel_mrd_dat,
      y = rlang::.data$level_no0s,
      colour = rlang::.data$Mutation
    )
    ) +

    ggplot2::theme_minimal() +

    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +

    ggplot2::scale_colour_brewer(palette = "Set2", na.translate = FALSE) +

    ggplot2::scale_x_continuous(limits = x_range) +

    ggplot2::scale_y_log10(limits = c(
      0.08,
      y_upper
    ), labels = scales::label_number()) +

    # Add clinical information title
    ggplot2::labs(title = paste0(
      "Patient: ",
      pat_id
    ),
    subtitle = paste0(
      "Diagnosis: ",
      d$general_info$mdsdiagnosis,
      "\nIPSS-M: ",
      d$general_info$ipssm_title,
      "\nKaryotype: ",
      d$general_info$karyotyp,
      "\nNGS: ",
      d$ngs$mutlist
    )
    ) +

    geomtextpath::geom_textvline(
      data = d$general_info |>
        dplyr::filter(rlang::.data$outcome == "Relapse"),
      ggplot2::aes(xintercept = rlang::.data$rel_term_dat, label = "Relapse")
    ) +

    geomtextpath::geom_textvline(
      data = d$general_info |> dplyr::filter(
        rlang::.data$outcome == "Nonrelapse mortality"
      ),
      ggplot2::aes(
        xintercept = rlang::.data$rel_term_dat,
        label = paste0("Death: ", rlang::.data$deathcause)
      )
    ) +

    geomtextpath::geom_texthline(
      yintercept = 0.1,
      label = "MRD Threshold",
      linetype = "dashed",
      color = "darkgrey",
      size = 3,
      vjust = -0.2,
      hjust = 1
    ) +

    ggplot2::theme(legend.position = "right",
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 9)
    )

  # Extract mrd legend
  mrd_legend <- cowplot::get_legend(mrd_plot)

  # Remove mrd legend from mrd_plot
  mrd_plot_clean <- mrd_plot + ggplot2::theme(legend.position = "none")

  # ----------------------------
  # GVHD / IS events plot (bottom)
  # ----------------------------

  events_plot <- ggplot2::ggplot() +

    # aGVHD
    ggplot2::geom_point(
      data = d$gvhd |> dplyr::filter(
        rlang::.data$gvhd == "Acute GVHD" & !is.na(rlang::.data$agvhdstage)
      ),
      ggplot2::aes(
        x = rlang::.data$rel_gvhd_dat,
        y = 1,
        colour = rlang::.data$agvhdstage
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
    ) +

    ggnewscale::new_scale_colour() +

    # cGVHD
    ggplot2::geom_point(
      data = d$gvhd |> dplyr::filter(
        rlang::.data$gvhd == "Chronic GVHD" & !is.na(rlang::.data$cgvhdstage)
      ),
      ggplot2::aes(
        x = rlang::.data$rel_gvhd_dat,
        y = 2,
        colour = rlang::.data$cgvhdstage
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
    ) +

    # Immune suppression duration
    ggplot2::geom_segment(
      data = d$immune_intervals,
      ggplot2::aes(
        x = rlang::.data$interval_start,
        xend = rlang::.data$interval_end,
        y = 3,
        yend = 3
      ),
      linewidth = 2,
      colour = "black"
    ) +

    # Azacitidine events
    ggplot2::geom_point(
      data = d$treatment |> dplyr::filter(
        rlang::.data$treatment == "Azacitidine"
      ),
      ggplot2::aes(
        x = rlang::.data$rel_treatment_dat,
        y = 4
      ),
      colour = "black",
      size = 3
    ) +

    # DLI events
    ggplot2::geom_point(
      data = d$treatment |> dplyr::filter(rlang::.data$treatment == "DLI"),
      ggplot2::aes(
        x = rlang::.data$rel_treatment_dat,
        y = 5
      ),
      colour = "black",
      size = 3
    ) +

    ggplot2::labs(x = "Days after transplantation", y = NULL) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_text(size = 10)
    ) +

    ggplot2::scale_x_continuous(limits = x_range) +

    ggplot2::scale_y_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c(
        "aGVHD",
        "cGVHD",
        "Immune suppression",
        "Azacitidine",
        "DLI"
      ),
      limits = c(0.5, 5.5)
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
  output_filename = "clinical_course.pdf"
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

  # To achieve a log10 y axis scale, convert the 0 values in level to 0.09
  processed$mrd <- processed$mrd |>
    dplyr::mutate(level_no0s = ifelse(level == 0, 0.08, level))

  # Define GVHD colours
  agvhd_colours <- c("0" = "#EBEBEB",
                     "1" = "#EDC0C0",
                     "2" = "#FF7878",
                     "3" = "#D42626",
                     "4" = "#800000")
  cgvhd_colours <- c("None" = "#EBEBEB",
                     "Mild" = "#AA88BB",
                     "Moderate" = "#622BD6",
                     "Severe" = "#290088")

  # Create GVHD dummy legends
  agvhd_legend_grob <- make_dummy_legend(
    names(agvhd_colours),
    agvhd_colours,
    "aGVHD Stage"
  )

  cgvhd_legend_grob <- make_dummy_legend(
    names(cgvhd_colours),
    cgvhd_colours,
    "cGVHD Stage"
  )

  cat("\nProcessed objects:\n")

  print(
    sapply(
      processed,
      class_finder(x)
    )
  )

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Export the figures to a pdf
  pdf(file.path(output_folder, output_filename), width = 10, height = 6)
  purrr::walk(
    unique(processed$general_info$patno),
    \(p) print(plot_patient_timeline(processed, p))
  )
  dev.off()

}
