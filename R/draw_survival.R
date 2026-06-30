draw_survival <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_format,
  strata = NULL,
  survival_filename
) {

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

  # Check that the strata column exists
  strata %in% names(processed$general_info)
  processed$general_info[[strata]]

  # Fit survival model
  fit <- survival::survfit(
    stats::as.formula(
      paste("survival::Surv(event_time, event_status) ~", strata)
    ),
    data = processed$general_info
  )

  # Draw figure

  survplot <- survminer::ggsurvplot(
    fit,
    data = processed$general_info,
    pval = TRUE,
    palette = "nejm",
    xlab = "Days after transplantation",
    risk.table = TRUE,
    risk.table.col = "strata"
  )

  # Save figure to svg or pdf

  if (output_format == "svg") {
    ggplot2::ggsave(
      filename = "survival.svg",
      plot = survplot$plot,
      device = "svg",
      width = 8,
      height = 8,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  } else {
    ggplot2::ggsave(
      filename = "survival.pdf",
      plot = survplot$plot,
      device = "svg",
      width = 8,
      height = 8,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
}