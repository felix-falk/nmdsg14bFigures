#' Run analysis using the nmdsFiguresMain function
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
#' @examples
#' nmds_figures_main(
#' general_info_file = "~/general.xlsx",
#' mrd_file = "~/mrd.xlsx",
#' dli_file = "~/dli.xlsx",
#' aza_file = "~/aza.xlsx",
#' immune_file = "~/immune.xlsx",
#' gvhd_file = "~/gvhd.xlsx",
#' ngs_file = "~/ngs.xlsx",
#' immune_filter_file = "~/immune_filter.csv",
#' output_folder = "~/output",
#' plot_type = "clinical_course",
#' filters = NULL
#' )
nmds_figures_main <- function(
  general_info_file,
  mrd_file,
  dli_file = NULL,
  aza_file = NULL,
  immune_file = NULL,
  gvhd_file = NULL,
  ngs_file = NULL,
  chimerism_file = NULL,
  immune_filter_file = NULL,
  strata = NULL,
  output_folder,
  plot_type = c(
    "swimmerplot",
    "clinical_course",
    "clinical_course_chimerism",
    "survival"
  ),
  filters = NULL,
  output_format = c("svg", "pdf")
) {

  plot_type <- match.arg(plot_type)
  output_format <- match.arg(output_format)

  required_args <- list(
    general_info_file = general_info_file,
    mrd_file = mrd_file,
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

    # If a single character string that points to an existing file,
    # treat it as a JSON filepath. Otherwise, accept an R list or
    # named/vector form and normalize to a list.
    if (is.character(filters) && length(filters) == 1 && file.exists(filters)) {
      user_filters <- jsonlite::fromJSON(filters)

      message(
        sprintf(
          "Loaded filter file: %s",
          filters
        )
      )

    } else {
      if (is.character(filters) && !is.list(filters)) {
        user_filters <- as.list(filters)
      } else {
        user_filters <- filters
      }
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
    immune_filter_file = immune_filter_file,
    chimerism_file = chimerism_file,
    strata = strata
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
      output_folder = output_folder,
      output_format = output_format
    )

    message(paste0(
      "Swimmer plot saved to: ",
      file.path(output_folder, paste0("swimmerplot.", output_format))
    ))

  } else if (plot_type == "clinical_course") {

    message("Drawing clinical course plots...")

    draw_clinical_course(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = output_folder,
      output_format = output_format
    )

    if (output_format == "svg") {
      message(paste0(
        "Clinical course plots saved to: ",
        output_folder,
        " (one .svg file per patient, pattern: <base>_<patno>.svg)"
      ))
    } else {
      message(paste0(
        "Clinical course plots saved to: ",
        file.path(output_folder, "clinical_course_plots.pdf")
      ))
    }

  } else if (plot_type == "clinical_course_chimerism") {

    message("Drawing clinical course chimerism plots...")

    draw_clinical_course_chimerism(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = output_folder,
      output_format = output_format
    )

    if (output_format == "svg") {
      message(paste0(
        "Clinical course chimerism plots saved to: ",
        output_folder,
        " (one .svg file per patient, pattern: <base>_<patno>.svg)"
      ))
    } else {
      message(paste0(
        "Clinical course chimerism plots saved to: ",
        file.path(output_folder, "clinical_course_chimerism_plots.pdf")
      ))
    }

  } else if (plot_type == "survival") {

    message("Drawing survival plot...")

    draw_survival(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = output_folder,
      output_format = output_format
    )

    if (output_format == "svg") {
      message(paste0(
        "Survival plot saved to: ",
        file.path(output_folder, "survival.svg")
      ))
    } else {
      message(paste0(
        "Survival plot saved to: ",
        file.path(output_folder, "survival.pdf")
      ))
    }

  }

  invisible(
    list(
      processed = processed,
      selected_patients = selected_patients
    )
  )
}