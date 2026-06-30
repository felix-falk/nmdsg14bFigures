draw_survival <- function(
  processed,
  patient_subset = NULL,
  output_folder,
  output_format,
  strata_filename,
  strata_colname = NULL,
  strata_itemname = NULL,
  survival_baseline = c(
    "transplant",
    "rel_pos_mrd_dat_0.1",
    "rel_pos_mrd_dat_0.5",
    "rel_pos_mrd_dat_1.0"
  ), # Set the baseline timepoint for survival analysis
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

  # Match the requested baseline
  survival_baseline <- match.arg(survival_baseline)

  # Rename the survival data set
  survival_data <- processed$general_info

  # Re-calculate event time relative to first positive MRD
  if (survival_baseline != "transplant") {

    survival_data <- survival_data |>
      dplyr::filter(!is.na(.data[[survival_baseline]])) |>
      dplyr::mutate(
        event_time = event_time - .data[[survival_baseline]]
      ) |>
      dplyr::filter(event_time >= 0)
  }

  # Fit survival model
  form <- stats::as.formula(
    sprintf("Surv(event_time, event_status) ~ `%s`", strata_name)
  )
  fit <- survival::survfit(form, data = survival_data)
  # Prevent survminer bug
  fit$call$formula <- form

  # Draw figure (event-free survival)

  survplot <- survminer::ggsurvplot(
    fit,
    data = survival_data,
    pval = TRUE,
    conf.int = TRUE,
    palette = "nejm",
    xlab = if (survival_baseline == "transplant") {
      "Days after transplantation"
    } else {
      sprintf("Days after %s positivity", survival_baseline)
    },
    risk.table = TRUE,
    risk.table.col = strata_name
  )

  # Save figure to svg or pdf
  if (output_format == "svg") {
    svg("survival.svg", width = 8, height = 8)
    print(survplot)
    dev.off()
  } else {
    pdf("survival.pdf", width = 8, height = 8)
    print(survplot)
    dev.off()
  }
}