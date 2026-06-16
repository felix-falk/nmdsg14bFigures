#' Manual data filtering ahead of plotting.
#'
#' @param processed A list of processed data frames.
#' @param filters A list of filter settings.
#' @returns A list of filtered data frames.
#' @examples
#' apply_filters(processed, filters)
apply_filters <- function(processed, filters) {

  general_info <- processed$general_info
  treatment <- processed$treatment
  mrd_all <- processed$mrd
  immune <- processed$immune
  ngs_processed <- processed$ngs

  # Allow `filters` to be a list, a named vector, or a JSON filepath.
  if (!is.null(filters) && !is.list(filters)) {
    if (is.character(filters) && length(filters) == 1 && file.exists(filters)) {
      filters <- jsonlite::fromJSON(filters)
    } else {
      filters <- as.list(filters)
    }
  }

  filter_settings <- list(
    genes = NULL,
    outcomes = NULL,
    treatments = NULL,
    mrd_positive = NULL,
    immune_suppression = NULL
  )

  if (!is.null(filters)) {
    filter_settings[names(filters)] <- filters
  }

  # Normalize filter settings: treat empty values (empty vector/string/NA)
  # as NULL => do not apply that filter
  normalize_filter <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(NULL)
    if (is.character(x) && all(nzchar(x) == FALSE)) return(NULL)
    if (all(is.na(x))) return(NULL)
    x
  }

  filter_settings <- lapply(filter_settings, normalize_filter)

  # Create a list of all patients in the study
  patients <- general_info |>
    dplyr::pull(patno) |>
    unique()

  # Filter patients based on genes in the NGS data
  if (!is.null(filter_settings$genes) && length(filter_settings$genes) > 0) {
    gene_patients <- ngs_processed |>
      dplyr::filter(Gen %in% filter_settings$genes) |>
      dplyr::distinct(patno, Gen) |>
      dplyr::count(patno) |>
      dplyr::filter(n == length(filter_settings$genes)) |>
      dplyr::pull(patno)
    patients <- intersect(patients, gene_patients)
  }

  # Filter patients based on outcome in the general info data
  if (
    !is.null(
      filter_settings$outcomes
    ) && length(
      filter_settings$outcomes
    ) > 0
  ) {
    outcome_patients <- general_info |>
      dplyr::filter(outcome %in% filter_settings$outcomes) |>
      dplyr::pull(patno)
    patients <- intersect(patients, outcome_patients)
  }

  # Filter patients based on treatments in the treatment data
  if (
    !is.null(
      filter_settings$treatments
    ) && length(
      filter_settings$treatments
    ) > 0
  ) {
    treatment_patients <- treatment |>
      dplyr::filter(treatment %in% filter_settings$treatments) |>
      dplyr::distinct(patno, treatment) |>
      dplyr::count(patno) |>
      dplyr::filter(n == length(filter_settings$treatments)) |>
      dplyr::pull(patno)
    patients <- intersect(patients, treatment_patients)
  }

  # Filter patients based on MRD positivity in the MRD data
  if (!is.null(filter_settings$mrd_positive)) {
    mrd_positive_patients <- mrd_all |>
      dplyr::group_by(patno) |>
      dplyr::summarise(
        mrd_positive = any(level >= 0.1, na.rm = TRUE),
        .groups = "drop"
      )
    selected_patients <- mrd_positive_patients |>
      dplyr::filter(
        selected_patients$mrd_positive == filter_settings$mrd_positive
      ) |>
      dplyr::pull(patno)
    patients <- intersect(patients, selected_patients)
  }

  # Filter patients based on immune suppression in the immune data
  if (!is.null(filter_settings$immune_suppression)) {
    immune_patients <- unique(immune$patno)
    if (filter_settings$immune_suppression) {
      patients <- intersect(patients, immune_patients)
    } else {
      patients <- setdiff(patients, immune_patients)
    }
  }

  # Final list of filtered patients
  filtered_patients <- sort(unique(patients))

  # Message about the number of patients matching the criteria
  cat(
    "\nNumber of selected patients after filtering:",
    length(filtered_patients),
    "\n\n"
  )

  # Message about the IDs of patients matching the criteria
  cat(
    "\nIDs of selected patients after filtering:",
    filtered_patients,
    "\n\n"
  )

  return(
    list(
      patient_ids = filtered_patients,
      n_patients = length(filtered_patients)
    )
  )

  filtered_patients
}