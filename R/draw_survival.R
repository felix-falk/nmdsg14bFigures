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
  survival_metric = c(
    "os",
    "rfs",
    "efs"
  ), # Set metric for survival analysis
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

  }

  # Survival metric mapping
  survival_metric <- match.arg(survival_metric)
  survival_data <- processed$general_info
  metric_map <- list(
    os  = list(time = "os_time",  status = "os_status"),
    rfs = list(time = "rfs_time", status = "rfs_status"),
    efs = list(time = "event_time", status = "event_status")
  )
  time_col <- metric_map[[survival_metric]]$time
  status_col <- metric_map[[survival_metric]]$status

  strata_var <- if (!is.null(strata_colname)) {
    strata_colname
  } else {
    strata_filename
  }

  # optional filtering ONLY
  if (!is.null(strata_itemname)) {
    survival_data <- survival_data |>
      dplyr::filter(.data[[strata_var]] == strata_itemname)
  }

  # Match the requested baseline
  survival_baseline <- match.arg(survival_baseline)

  # Rename the survival data set
  survival_data <- processed$general_info

  # Re-calculate event time relative to first positive MRD
  if (survival_baseline != "transplant") {
    survival_data <- survival_data |>
      dplyr::filter(!is.na(.data[[survival_baseline]]))

    survival_data[[time_col]] <-
      survival_data[[time_col]] - survival_data[[survival_baseline]]
    survival_data <- survival_data[
      !is.na(survival_data[[time_col]]) & survival_data[[time_col]] >= 0,
      ,
      drop = FALSE
    ]
  }

  # Fit survival model
  form <- stats::as.formula(
    sprintf("Surv(%s, %s) ~ `%s`", time_col, status_col, strata_var)
  )
  fit <- survival::survfit(form, data = survival_data)
  fit$call$formula <- form # Solves ggsurvplot bug

  # Write x-axis label
  xlab_text <- paste(
    "Days after",
    if (survival_baseline == "transplant")
      "transplantation" else survival_baseline
  )

  # Write y-axis label
  ylab_text <-
    if (survival_metric == "os") "Overall survival"
    else if (survival_metric == "rfs") "Relapse free survival"
    else if (survival_metric == "efs") "Event free survival"

  # Draw figure
  survplot <- survminer::ggsurvplot(
    fit,
    data = survival_data,
    pval = TRUE,
    conf.int = TRUE,
    palette = "nejm",
    xlab = xlab_text,
    ylab = ylab_text,
    risk.table = TRUE,
    risk.table.col = "strata"
  )

  # Save figure to svg or pdf
  if (output_format == "svg") {
    grDevices::svg("survival.svg", width = 8, height = 8)
    print(survplot)
    grDevices::dev.off()
  } else {
    grDevices::pdf("survival.pdf", width = 8, height = 8)
    print(survplot)
    grDevices::dev.off()
  }
}