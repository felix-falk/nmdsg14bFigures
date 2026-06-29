draw_survival <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_format,
  strata = NULL
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

  fit <- survival::survfit(
    survival::Surv(
      processed$general_info$event_time,
      processed$general_info$event_status
    ) ~ ipssr
  )

  survplot <- survminer::ggsurvplot(
    fit,
    data = processed$general_info,
    pval = TRUE,
    palette = "nejm",
    xlab = "Days after transplantation"
  )

  ggplot2::ggsave(
    filename = "survival.svg",
    plot = survplot$plot,
    device = "svg",
    width = 8,
    height = 6,
    units = "in",
    dpi = 300,
    bg = "white"
  )

}