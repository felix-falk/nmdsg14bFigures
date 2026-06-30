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
        "{time_col}" := .data[[time_col]] - .data[[survival_baseline]]
      ) |>
      dplyr::filter(.data[[time_col]] >= 0)
  }

  # Fit survival model
  form <- stats::as.formula(
    sprintf("Surv(%s, %s) ~ `%s`", time_col, status_col, strata_name)
  )
  fit <- survival::survfit(form, data = survival_data)
  fit$call$formula <- form # Solves ggsurvplot bug

  # Write x-axis label
  xlab_text <- paste(
    "Days after",
    if (survival_baseline == "transplant") "transplantation" else survival_baseline
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