#' Run analysis workflow
#'
#' @param general_info_file Excel file with general patient clinical information
#' @param mrd_file Excel file with MRD measurements
#' @param dli_file Excel file with DLI treatment information
#' @param aza_file Excel file with Azacitidine treatment information
#' @param immune_file Excel file with immune suppression treatment information
#' @param gvhd_file Excel file with GVHD events information
#' @param ngs_file Excel file with NGS information
#' @param immune_filter_file CSV file with immune suppression filter
#' @param output_folder Folder for generated figures
#' @param plot_type Either "swimmerplot" or "clinical_course"
#' @param filters Optional filter list or path to JSON file
#'
#' @export
nmdsFiguresMain <- function(
  general_info_file,
  mrd_file,
  dli_file,
  aza_file,
  immune_file,
  gvhd_file,
  ngs_file,
  immune_filter_file,
  output_folder,
  plot_type = c("swimmerplot", "clinical_course"),
  filters = NULL
) {

  plot_type <- match.arg(plot_type)

  required_args <- list(
    general_info_file = general_info_file,
    mrd_file = mrd_file,
    dli_file = dli_file,
    aza_file = aza_file,
    immune_file = immune_file,
    gvhd_file = gvhd_file,
    ngs_file = ngs_file,
    immune_filter_file = immune_filter_file,
    output_folder = output_folder
  )

  missing_args <- names(required_args)[
    vapply(required_args, is.null, logical(1))
  ]

  if (length(missing_args) > 0) {
    stop(
      sprintf(
        "Missing required arguments: %s",
        paste(missing_args, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Load filters
  # ----------------------------------------------------------

  user_filters <- NULL

  if (!is.null(filters)) {

    if (is.character(filters) && length(filters) == 1) {

      user_filters <- jsonlite::fromJSON(filters)

      message(
        sprintf(
          "Loaded filter file: %s",
          filters
        )
      )

    } else {

      user_filters <- filters
    }
  }

  # ----------------------------------------------------------
  # Preprocess data
  # ----------------------------------------------------------

  message("Preprocessing data...")

  processed <- preprocess_data(
    general_info_file = general_info_file,
    mrd_file = mrd_file,
    dli_file = dli_file,
    aza_file = aza_file,
    immune_file = immune_file,
    gvhd_file = gvhd_file,
    ngs_file = ngs_file,
    immune_filter_file = immune_filter_file
  )

  # ----------------------------------------------------------
  # Optional filtering
  # ----------------------------------------------------------

  selected_patients <- NULL

  if (!is.null(user_filters)) {

    message("Applying patient filters...")

    filter_results <- apply_filters(
      processed = processed,
      filters = user_filters
    )

    selected_patients <- filter_results$patient_ids

    message(
      sprintf(
        "%d patients matched filter criteria",
        filter_results$n_patients
      )
    )
  }

  # ----------------------------------------------------------
  # Plot generation
  # ----------------------------------------------------------

  if (plot_type == "swimmerplot") {

    message("Drawing swimmer plot...")

    draw_swimmerplot(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = output_folder
    )

  } else if (plot_type == "clinical_course") {

    message("Drawing clinical course plots...")

    draw_clinical_course(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = output_folder
    )
  }

  message("Finished.")

  invisible(
    list(
      processed = processed,
      selected_patients = selected_patients
    )
  )
}