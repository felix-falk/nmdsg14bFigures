draw_survival <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_format,
  strata_filename,
  strata_colname = NULL,
  strata_itemname = NULL,
  survival_filename
) {

  if (!is.null(patient_subset)) {

    processed$general_info <-
      processed$general_info |>
      dplyr::filter(patno %in% patient_subset)

  }

  # Get correct strata column name
  strata_name <- NULL
  if (is.null(strata_colname) && is.null(strata_itemname)) {
    strata_name <- strata_filename
  } else if (is.null(strata_itemname)) {
    strata_name <- strata_colname
  } else {
    strata_name <- strata_itemname
  }
  stopifnot(!is.null(strata_name))

  # Check that the strata column exists
  strata_name %in% names(processed$general_info)
  processed$general_info[[strata_name]]
  print(names(processed$general_info))
  print(strata_name)

  survival_data <- processed$general_info

  # Fit survival model
  fit <- eval(bquote(
    survival::survfit(
      .(stats::as.formula(
        sprintf("Surv(event_time, event_status) ~ `%s`", strata_name)
      )),
      data = processed$general_info
    )
  ))

  # Draw figure

  survplot <- survminer::ggsurvplot(
    fit,
    data = survival_data,
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