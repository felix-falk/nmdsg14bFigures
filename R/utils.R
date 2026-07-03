#' Called by the preprocess_data function to identify
#' immune suppression intervals, based on the immune suppression data frame.
#'
#' @param df Immune suppression data frame.
#' @returns A data frame with identified immune suppression intervals.
#' @examples
#' \dontrun{
#' interval_finder(immune)
#' }
interval_finder <- function(df) {

  df |>
    dplyr::arrange(patno, drugname_standardized, rel_immune_dat) |>
    dplyr::group_by(patno, drugname_standardized) |>
    dplyr::mutate(
      interval_no = dplyr::row_number(),
      interval_start = dplyr::lag(rel_immune_dat, default = 0),
      interval_end = rel_immune_dat
    ) |>
    dplyr::select(
      interval_no,
      interval_start,
      interval_end,
      patno,
      drugname_standardized,
      drugdose,
      dose_percentage,
      drugstopped
    ) |>
    dplyr::ungroup()
}

#' Called by the preprocess_data function to standardize
#' drug names in the immune suppression data frame,
#' based on a provided mapping data frame.
#'
#' @param drug Drug name to standardize.
#' @param mapping_df Mapping data frame with patterns and standardized names.
#' @returns The standardized drug name or NA if no match is found.
#' @examples
#' \dontrun{
#' standardize_drug("Drug A", immune_suppression_filter)
#' }
standardize_drug <- function(drug, mapping_df) {
  match_idx <- which(
    purrr::map_lgl(
      mapping_df$pattern,
      ~ stringr::str_detect(drug, stringr::regex(.x, ignore_case = TRUE))
    )
  )
  if (length(match_idx) > 0) {
    return(mapping_df$standardized_name[match_idx[1]])
  }
  return(NA_character_)
}

#' Called by the draw_clinical_course function to create GVHD dummy legends.
#'
#' @param levels The levels of the legend.
#' @param colours The colours for the legend.
#' @param title The title of the legend.
#' @returns A legend grob.
#' @examples
#' \dontrun{
#' make_dummy_legend(c("0", "1", "2"), c("red", "blue", "green"), "GVHD Stage")
#' }
make_dummy_legend <- function(levels, colours, title) {
  df <- data.frame(stage = factor(levels, levels = levels), x = 1, y = 1)
  cowplot::get_legend(
    ggplot2::ggplot(df, ggplot2::aes(x, y, colour = stage)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_colour_manual(name = title, values = colours) +
      ggplot2::theme_void() + ggplot2::theme(legend.position = "right")
  )
}

#' Called by the draw_clinical_course function to select one patient per graph.
#'
#' @param df Data frame containing patient information with a "patno" column.
#' @param pat_id The patno number of the patient.
#' @returns Filtered data frame for a specific patient,
#' or the original data frame if it does not meet the criteria.
#' @examples
#' \dontrun{
#' select_one_patient(df)
#' }
select_one_patient <- function(df, pat_id = NULL) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!is.data.frame(df)) {
    return(df)
  }
  if (!"patno" %in% names(df)) {
    return(df)
  }
  if (is.null(pat_id)) {
    return(df)
  }
  dplyr::filter(df, patno == pat_id)
}

#' Called by the draw_clinical_course function to
#' find the upper limit of the MRD and/or chimerism y-axis.
#' @param mrd_data A data frame containing MRD data.
#' @param chimerism_data A data frame containing chimerism data.
#' @returns A numeric value indicating the upper y-axis limit.
#' @examples
#' \dontrun{
#' y_limit_finder(d$mrd)
#' }
y_limit_finder <- function(
  mrd_data,
  chimerism_data = NULL
) {

  # Helper to get max from a data frame/column safely
  safe_max <- function(df, col) {
    if (
      is.null(df) ||
        nrow(df) == 0 ||
        !(col %in% names(df)) ||
        all(is.na(df[[col]]))
    ) {
      return(NA_real_)
    }
    return(max(df[[col]], na.rm = TRUE))
  }

  if (!is.null(chimerism_data)) {
    max_mrd <- safe_max(mrd_data, "level_no0s")
    max_chim <- safe_max(chimerism_data, "chimerism")

    # Determine the upper y-axis limit: at least 10, or the highest value
    observed_max <- max(c(max_mrd, max_chim), na.rm = TRUE)
  } else {
    max_mrd <- safe_max(mrd_data, "level_no0s")
    # Determine the upper y-axis limit: at least 10, or the highest value
    observed_max <- max(max_mrd, na.rm = TRUE)
  }

  if (is.infinite(observed_max) || is.na(observed_max)) {
    y_upper <- 10
  } else {
    y_upper <- max(10, ceiling(observed_max))
  }

  return(y_upper)

}

#' Called by the draw_clinical_course function to
#' find the range of MRD x-axis values.
#'
#' @param general_info_data A data frame containing the
#' termination date of the patient, relative to the transplantation date.
#' @returns A numeric vector indicating the range of the x-axis.
#' @examples
#' \dontrun{
#' x_range_finder(d$general_info)
#' }
x_range_finder <- function(general_info_data) {

  # Determine the range of the x axis
  x_end <- general_info_data$rel_term_dat[1]

  # Fallback if rel_term_dat does not exist
  if (is.na(x_end) || !is.finite(x_end)) {
    x_end <- 365
  }

  # Add 10 days to the upper x-axis limit
  x_range <- c(0, x_end)

  return(x_range)

}

#' Called by preprocess_data to check that all
#' required columns are present in the provided file.
#'
#' @param df Data frame with columns you want to check.
#' @param required_columns List of required columns.
#' @returns
#' Stops the function and raises a message if columns are missing, else passes.
#' @examples
#' \dontrun{
#' column_check(dli_raw, c("patno", "dlidat"))
#' }
column_check <- function(
  df = NULL,
  required_columns = NULL
) {
  missing_cols <- setdiff(required_columns, names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Creates treatment data frame based on dli and aza data frames.
#' @param aza_raw Data frame containing patno,
#' azacitstart and transpldt columns.
#' @param dli_raw Data frame containing patno, dlidat and transpldt columns.
#' @param end_date_df Data frame containing patno, transpldt
#' and rel_term_dat columns.
#' @returns Data frame containing azacitidine and dli treatment timepoints.
#' @examples
#' \dontrun{
#' create_treatment_df(aza_raw, dli_raw, end_date_df)
#' }
create_treatment_df <- function(
  aza_raw = NULL,
  dli_raw = NULL,
  end_date_df = NULL
) {
  # Calculate relative aza dates, remove dli after rel_term_dat
  aza <- aza_raw |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_aza_dat = as.numeric(difftime(
        as.Date(azacitstart),
        as.Date(transpldt),
        units = "days"
      ))
    ) |>
    dplyr::filter(rel_aza_dat <= rel_term_dat)

  # Calculate relative dli dates, remove dli after rel_term_dat
  dli <- dli_raw |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_dli_dat = as.numeric(difftime(
        as.Date(dlidat),
        as.Date(transpldt),
        units = "days"
      ))
    ) |>
    dplyr::filter(rel_dli_dat <= rel_term_dat)

  # Merge dli and aza data frames
  treatment <- dplyr::bind_rows(
    dli |>
      dplyr::select(patno, rel_treatment_dat = rel_dli_dat) |>
      dplyr::mutate(treatment = "DLI"),
    aza |>
      dplyr::select(patno, rel_treatment_dat = rel_aza_dat) |>
      dplyr::mutate(treatment = "Azacitidine")
  )

  return(treatment)

}

#' Creates a gvhd data frame based on gvhd_raw and end_date_df, that contains
#' gvhd events occuring at and in between regular checkups.
#' @param gvhd_raw Data frame containing patno, agvhdstage, gvhddate,
#' agvhdmaxstage, agvhdmaxdt, cgvhdstage, cgvhdmaxstage, cgvhdmaxdt columns.
#' @param end_date_df Data frame containing patno, transpldt
#' and rel_term dat columns.
#' @returns Data frame containing patno, gvhd, rel_gvhd_dat,
#' agvhdstage, cgvhdstage columns.
#' @examples
#' \dontrun{
#' create_gvhd_df(gvhd_raw, end_date_df)
#' }
create_gvhd_df <- function(
  gvhd_raw = NULL,
  end_date_df = NULL
) {
  # Calculate agvhd_raw_merged and cgvhd_raw_merged
  agvhd_raw_merged <- dplyr::bind_rows(
    gvhd_raw |>
      dplyr::select(patno, agvhdstage, gvhddate),
    gvhd_raw |>
      dplyr::select(patno, agvhdstage = agvhdmaxstage, gvhddate = agvhdmaxdt)
  ) |>
    dplyr::filter(!is.na(agvhdstage), !is.na(gvhddate)) |>
    dplyr::distinct() |>
    dplyr::mutate(gvhd = "Acute GVHD")

  cgvhd_raw_merged <- dplyr::bind_rows(
    gvhd_raw |>
      dplyr::select(patno, cgvhdstage, gvhddate),
    gvhd_raw |>
      dplyr::select(patno, cgvhdstage = cgvhdmaxstage, gvhddate = cgvhdmaxdt)
  ) |>
    dplyr::filter(!is.na(cgvhdstage), !is.na(gvhddate)) |>
    dplyr::distinct() |>
    dplyr::mutate(gvhd = "Chronic GVHD")

  # Calculate gvhd_raw_merged
  gvhd_processed <- dplyr::bind_rows(cgvhd_raw_merged, agvhd_raw_merged) |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(rel_gvhd_dat = as.numeric(difftime(
      as.Date(gvhddate), 
      as.Date(transpldt), units = "days"
    ))) |>
    dplyr::filter(rel_gvhd_dat <= rel_term_dat) |>
    dplyr::mutate(cgvhdstage = dplyr::if_else(
      cgvhdstage %in% c("N/A", "N/K", ".", "N/D"),
      NA,
      cgvhdstage
    ))

  return(gvhd_processed)
}

#' Creates ngs data frame based on ngs_raw.
#' @param ngs_raw Data frame with patno, Gen and cDNA forandring columns.
create_ngs_df <- function(
  ngs_raw
) {
  # NGS Data filtering
  gene_lists <-
    ngs_raw |>
    dplyr::filter(!is.na(patno)) |>
    dplyr::summarise(
      mutlist = paste(unique(Gen), collapse = ", "),
      .by = patno
    )

  ngs <-
    ngs_raw |>
    dplyr::left_join(gene_lists, by = "patno") |>
    dplyr::mutate(
      mutname = paste0(Gen, "_", "cDNA forandring")
    )

  return(ngs)
}

#' Creates a gvhd data frame based on general_info_raw and mrd_raw,
#' that contains patno, transpldt and rel_term_dat
#' @param general_info_raw Data frame containing patno,
#' termindat and transpldt columns.
#' @param mrd_raw Data frame containing patno and MRDdat columns.
#' @returns Data frame containing patno, transpldt
#' and rel_term dat columns.
#' @examples
#' \dontrun{
#' create_end_date_df(general_info_raw, mrd_raw)
#' }
create_end_date_df <- function(
  general_info_raw = NULL,
  mrd_raw = NULL
) {
  # Create end_date_df based on general_info_raw and mrd_raw
  end_date_df <- general_info_raw |>
    dplyr::select(patno, termindat, transpldt) |>
    dplyr::left_join(
      mrd_raw |>
        dplyr::group_by(patno) |>
        dplyr::summarise(MRDdat = max(MRDdat, na.rm = TRUE), .groups = "drop"),
      by = "patno"
    ) |>
    dplyr::mutate(
      end_date = dplyr::coalesce(termindat, MRDdat),
      rel_term_dat = as.numeric(
        difftime(as.Date(end_date), as.Date(transpldt), units = "days")
      )
    ) |>
    dplyr::transmute(patno, transpldt, rel_term_dat)

  return(end_date_df)
}

#' Adds strata for survival analysis to the general_info data frame.
#' @param general_info Data frame with patno column.
#' @param strata_filename Name of data frame with strata of interest.
#' @param strata_colname Name of column with strata of interest.
#' @param strata_itemname Name of item with strata of interest.
#' @returns general_info data frame with an additional strata column.
#' @examples
#' \dontrun{
#' add_strata(general_info, strata_filename = "ngs",
#' strata_colname = "Gen", strata_itemname = "TP53")
#' }
add_strata <- function(
  general_info,
  strata_filename = NULL,
  strata_colname = NULL,
  strata_itemname = NULL
) {

  if (is.null(strata_filename)) return(general_info)

  strata_df <- get(strata_filename, envir = parent.frame())

  # column name MUST be stable
  new_colname <- if (!is.null(strata_colname))
    strata_colname
  else
    strata_filename

  if (new_colname %in% names(general_info)) {
    message("No new strata column added.")
    return(general_info)
  }

  if (is.null(strata_colname)) {

    strata_patnos <- unique(strata_df$patno)

    general_info[[new_colname]] <-
      general_info$patno %in% strata_patnos

  } else if (is.null(strata_itemname)) {

    general_info[[new_colname]] <-
      strata_df[[strata_colname]][
        match(general_info$patno, strata_df$patno)
      ]

  } else {

    # filtered presence
    strata_patnos <- unique(
      strata_df$patno[strata_df[[strata_colname]] == strata_itemname]
    )

    general_info[[new_colname]] <-
      general_info$patno %in% strata_patnos
  }

  return(general_info)
}

#' Creates a chimerism data frame based on chimerism_raw and end_date_df,
#' that contains patno, chimerism, rel_chimerism_dat and surface_marker columns.
#' @param chimerism_raw Data frame containing patno, chimerism, chimbmdt and
#' columns starting with CD that contain chimerims values
#' for each surface marker.
#' @param end_date_df Data frame containing patno, transpldt and
#' rel_term_dat columns.
#' @returns Data frame containing patno, surface_marker, chimerism, and
#' rel_chimerism_dat columns.
#' @examples
#' \dontrun{
#' create_chimerism_df(chimerism_raw, end_date_df)
#' }
create_chimerism_df <- function(
  chimerism_raw,
  end_date_df
) {
  chimerism <- chimerism_raw |>
    tidyr::pivot_longer(
      dplyr::starts_with("CD"),
      names_to = "surface_marker",
      values_to = "chimerism"
    ) |>
    dplyr::filter(!is.na(chimerism)) |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_chimerism_dat = as.numeric(difftime(
        as.Date(chimbmdt),
        as.Date(transpldt),
        units = "days"
      ))
    ) |>
    dplyr::filter(rel_chimerism_dat <= rel_term_dat) |>
    dplyr::filter(surface_marker %in% c("CD33BM", "CD34BM"))

  return(chimerism)
}

#' Called by preprocess_data to find overlapping intervals
#' in an immune intervals data frame.
#' @oaram interval_df Data frame containing patno, interval_start and
#' interval_end columns.
#' @returns Data frame containing patno, overlap_group, interval_start and
#' interval_end columns, where overlapping intervals are consolidated
#' into a single interval.
#' @example overlapping_interval_finder(interval_df)
overlapping_interval_finder <- function(interval_df) {
  overlapping_interval_df <- interval_df |>
    dplyr::arrange(patno, interval_start, interval_end) |>
    dplyr::group_by(patno) |>
    dplyr::mutate(
      running_max_end = cummax(interval_end),
      new_group = interval_start > dplyr::lag(
        running_max_end,
        default = dplyr::first(interval_end)
      ),
      overlap_group = cumsum(new_group) + 1
    ) |>
    dplyr::group_by(patno, overlap_group) |>
    dplyr::summarise(
      interval_start = min(interval_start),
      interval_end = max(interval_end),
      .groups = "drop"
    )
  return(overlapping_interval_df)
}

#' Called by preprocess_data to calculate the percentage of the maximum dose
#' for each drug in the immune suppression data frame.
#' @param immune_df Data frame containing patno, drugname_standardized and
#' drugdose columns.
#' @returns Data frame containing patno, drugname_standardized, drugdose and
#' dose_percentage columns, where dose_percentage is the percentage of the
#' maximum dose for each drug.
#' @example calc_immune_percentage_dose(immune_df)
calc_immune_percentage_dose <- function(immune_df) {
  immune_df <- immune_df |>
    dplyr::group_by(patno, drugname_standardized) |>
    dplyr::mutate(
      max_dose = max(drugdose, na.rm = TRUE),
      dose_percentage = dplyr::if_else(
        max_dose > 0,
        (drugdose / max_dose) * 100,
        NA_real_
      )
    ) |>
    dplyr::select(-max_dose) |>
    dplyr::ungroup()

  return(immune_df)
}
