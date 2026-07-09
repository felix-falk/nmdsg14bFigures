#' Run analysis using the nmds_figures_main function.
#'
#' @param general_info_file Excel file with general patient clinical
#' information. Required for all plot types.
#' @param mrd_file Excel file with MRD measurements. Required for all
#' plot types.
#' @param dli_file Excel file with DLI treatment information
#' @param aza_file Excel file with Azacitidine treatment information
#' @param immune_file Excel file with immune suppression treatment information
#' @param gvhd_file Excel file with GVHD events information
#' @param ngs_file Excel file with NGS information
#' @param chimerism_file Excel file with chimerism information.
#' Required when `plot_type = "clinical_course_chimerism"`.
#' @param immune_filter_file CSV file with immune suppression filter
#' @param output_folder Folder for generated figures
#' @param strata_filename Optional file name or data source used for strata.
#' @param strata_colname Optional column name for strata.
#' @param strata_itemname Optional strata value to keep.
#' @param survival_baseline Baseline for survival analysis,
#' one of "transplant", "rel_pos_mrd_dat_0.1",
#' "rel_pos_mrd_dat_0.5", or "rel_pos_mrd_dat_1.0".
#' @param survival_metric Survival metric, one of "os", "rfs", or "efs".
#' @param plot_type Either "swimmerplot", "clinical_course",
#' "clinical_course_chimerism", or "survival"
#' @param filters Optional filter list
#' @param output_format Output format, either "svg" or "pdf"
#'
#' @export
#' @examples
#' nmds_figures_main(
#'  general_info_file = "general.xlsx",
#'  mrd_file = "mrd.xlsx",
#'  dli_file = "dli.xlsx",
#'  aza_file = "aza.xlsx",
#'  immune_file = "immune.xlsx",
#'  gvhd_file = "gvhd.xlsx",
#'  ngs_file = "ngs.xlsx",
#'  chimerism_file = "chimerism.xlsx",
#'  immune_filter_file = "immune_filter.csv",
#'  output_folder = "output",
#'  plot_type = "clinical_course",
#'  filters = list(
#'    genes = "TP53",
#'    outcomes = "Relapse"
#'  )
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
  strata_filename = NULL,
  strata_colname = NULL,
  strata_itemname = NULL,
  survival_baseline = NULL,
  survival_metric = NULL,
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

  required_files <- list(
    general_info_file = general_info_file,
    mrd_file = mrd_file
  )

  if (plot_type == "clinical_course_chimerism") {
    required_files$chimerism_file <- chimerism_file
  }

  missing_files <- names(required_files)[
    vapply(required_files, is.null, logical(1))
  ]

  if (length(missing_files) > 0) {
    stop(
      sprintf(
        "Missing required input file arguments for plot_type '%s': %s",
        plot_type,
        paste(missing_files, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  non_existing_files <- names(required_files)[
    !vapply(
      required_files,
      function(path) file.exists(path.expand(path)),
      logical(1)
    )
  ]

  if (length(non_existing_files) > 0) {
    stop(
      sprintf(
        "Required input files do not exist for plot_type '%s': %s",
        plot_type,
        paste(non_existing_files, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (is.null(output_folder)) {
    stop("Missing required argument: output_folder", call. = FALSE)
  }

  # ----------------------------------------------------------
  # Load filters
  # ----------------------------------------------------------

  user_filters <- NULL

  if (!is.null(filters)) {

    # If filters is not empty, control that it is formatted as a list.

    if (!is.list(filters)) {
      stop(
        "filters must be supplied as a list.",
        call. = FALSE
      )
    }

    user_filters <- filters

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
    strata_filename = strata_filename,
    strata_colname = strata_colname,
    strata_itemname = strata_itemname
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

    message(
      sprintf(
        "Patient IDs matching filter criteria: %s",
        paste(filter_results$patient_ids, collapse = ", ")
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
      output_format = output_format,
      strata_filename = strata_filename,
      strata_colname = strata_colname,
      strata_itemname = strata_itemname,
      survival_baseline = survival_baseline,
      survival_metric = survival_metric
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