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
  ),
  survival_metric = c(
    "os",
    "rfs",
    "efs"
  ),
  survival_filename = "survival"
) {

  processed$general_info <- ensure_data_frame_columns(
    processed$general_info,
    c(
      "patno", "os_time", "os_status", "rfs_time", "rfs_status",
      "event_time", "event_status",
      "rel_pos_mrd_dat_0.1", "rel_pos_mrd_dat_0.5", "rel_pos_mrd_dat_1.0"
    )
  )
  processed$treatment <- ensure_data_frame_columns(processed$treatment, c("patno"))
  processed$mrd <- ensure_data_frame_columns(processed$mrd, c("patno"))
  processed$gvhd <- ensure_data_frame_columns(processed$gvhd, c("patno"))
  processed$immune_intervals <- ensure_data_frame_columns(
    processed$immune_intervals,
    c("patno")
  )
  processed$ngs <- ensure_data_frame_columns(processed$ngs, c("patno"))

  if (!is.null(patient_subset)) {
    processed$general_info <- processed$general_info |>
      dplyr::filter(patno %in% patient_subset)

    processed$treatment <- processed$treatment |>
      dplyr::filter(patno %in% patient_subset)

    processed$mrd <- processed$mrd |>
      dplyr::filter(patno %in% patient_subset)

    processed$gvhd <- processed$gvhd |>
      dplyr::filter(patno %in% patient_subset)

    processed$immune_intervals <- processed$immune_intervals |>
      dplyr::filter(patno %in% patient_subset)

    processed$ngs <- processed$ngs |>
      dplyr::filter(patno %in% patient_subset)
  }

  survival_data <- processed$general_info
  if (nrow(survival_data) == 0) {
    stop("No patients available after filtering.", call. = FALSE)
  }

  # Survival metric mapping
  survival_metric <- match.arg(survival_metric)
  metric_map <- list(
    os = list(time = "os_time", status = "os_status"),
    rfs = list(time = "rfs_time", status = "rfs_status"),
    efs = list(time = "event_time", status = "event_status")
  )
  time_col <- metric_map[[survival_metric]]$time
  status_col <- metric_map[[survival_metric]]$status

  # Build strata variable; if none is provided, analyse all patients as one group.
  if (is.null(strata_colname) && is.null(strata_filename)) {
    strata_var <- ".all_patients"
    survival_data[[strata_var]] <- "All"
  } else {
    strata_var <- if (!is.null(strata_colname)) strata_colname else strata_filename
    if (!strata_var %in% names(survival_data)) {
      stop(
        sprintf("Strata column '%s' not found in survival data.", strata_var),
        call. = FALSE
      )
    }
  }

  # Optional filtering ONLY
  if (!is.null(strata_itemname)) {
    survival_data <- survival_data |>
      dplyr::filter(.data[[strata_var]] == strata_itemname)
  }

  # Match the requested baseline
  survival_baseline <- match.arg(survival_baseline)

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

  if (nrow(survival_data) == 0) {
    stop("No patients available after applying survival baseline/strata filters.")
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
    if (survival_baseline == "transplant") "transplantation" else survival_baseline
  )

  # Write y-axis label
  ylab_text <-
    if (survival_metric == "os") "Overall survival"
    else if (survival_metric == "rfs") "Relapse free survival"
    else "Event free survival"

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

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  out_file <- file.path(
    output_folder,
    paste0(tools::file_path_sans_ext(survival_filename), ".", output_format)
  )

  # Save figure to svg or pdf
  if (output_format == "svg") {
    grDevices::svg(out_file, width = 8, height = 8)
    print(survplot)
    grDevices::dev.off()
  } else {
    grDevices::pdf(out_file, width = 8, height = 8)
    print(survplot)
    grDevices::dev.off()
  }
}
