#' Called by the preprocess_data function to identify
#' immune suppression intervals, based on the immune suppression data frame.
#'
#' @param df Immune suppression data frame.
#' @returns A data frame with identified immune suppression intervals.
#' @examples
#' interval_finder(immune)
interval_finder <- function(df) {

  df |>
    dplyr::arrange(patno, drugname_standardized, rel_immune_dat) |>
    dplyr::group_by(patno, drugname_standardized) |>
    dplyr::group_modify(~ {

      stop_idx <- which(.x$drugstopped == "Yes")

      if (length(stop_idx) == 0) {
        return(tibble::tibble(
          interval_no = 1,
          interval_start = min(.x$rel_immune_dat, na.rm = TRUE),
          interval_end = max(.x$rel_term_dat, na.rm = TRUE)
        ))
      }

      starts <- c(1, stop_idx[-length(stop_idx)] + 1)
      ends <- stop_idx

      tibble::tibble(
        interval_no = seq_along(starts),
        interval_start = .x$rel_immune_dat[starts],
        interval_end = .x$rel_immune_dat[ends]
      )
    }) |>
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
#' standardize_drug("Drug A", immune_suppression_filter)
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
#' make_dummy_legend(c("0", "1", "2"), c("red", "blue", "green"), "GVHD Stage")
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
#' @returns Filtered data frame for a specific patient,
#' or the original data frame if it does not meet the criteria.
#' @examples
#' select_one_patient(df)
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
#' y_limit_finder(d$mrd)
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
#' x_range_finder(d$general_info)
x_range_finder <- function(general_info_data){

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
#' column_check(dli_raw, c("patno", "dlidat"))
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
#' @examples create_treatment_df(aza_raw, dli_raw, end_date_df)
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
#' @ param end_date_df Data frame containing patno, transpldt
#' and rel_term dat columns.
#' @returns Data frame containing patno, gvhd, rel_gvhd_dat,
#' agvhdstage, cgvhdstage columns.
#' @example create_gvhd_df(gvhd_raw, end_date_df)
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
#' @param
create_ngs_df <- function(ngs_raw){
  # NGS Data filtering
  gene_lists <-
    ngs_raw |>
    dplyr::filter(!is.na(Studienummer)) |>
    dplyr::summarise(
      mutlist = paste(unique(Gen), collapse = ", "),
      .by = Studienummer
    )

  ngs <-
    ngs_raw |>
    dplyr::left_join(gene_lists, by = "Studienummer") |>
    dplyr::mutate(
      mutname = paste0(Gen, "_", "cDNA förändring"),
      patno = as.double(Studienummer)
    ) |>
    dplyr::select(-Studienummer)

  return(ngs)
}

#' Creates a gvhd data frame based on general_info_raw and mrd_raw,
#' that contains patno, transpldt and rel_term_dat
#' @param general_info_raw Data frame containing patno,
#' termindat and transpldt columns.
#' @ param mrd_raw Data frame containing patno and MRDdat columns.
#' @returns Data frame containing patno, transpldt
#' and rel_term dat columns.
#' @example create_end_date_df(general_info_raw, mrd_raw)
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
#' @strata_filename Name of data frame with strata of interest.
#' @strata_colname Name of column with strata of interest.
#' @strata_itemname Name of item with strata of interest.
#' @returns general_info data frame with an additional strata column.
#' @example add_strata(general_info, strata_filename = "ngs",
#' strata_colname = "Gen", strata_itemname = "TP53")
add_strata <- function(
  general_info,
  strata_filename = NULL,
  strata_colname = NULL,
  strata_itemname = NULL
) {

  if (is.null(strata_filename)) {
    return(general_info)
  }

  # Get the data frame by name
  strata_df <- get(strata_filename)

  if (is.null(strata_colname) && is.null(strata_itemname)) {
    # Strata is whether the patient appears in the file

    strata_patnos <- unique(strata_df$patno)

    general_info[[strata_filename]] <-
      general_info$patno %in% strata_patnos

  } else if (!is.null(strata_colname) && is.null(strata_itemname)) {
    # Strata is the value of a column

    general_info[[strata_colname]] <-
      strata_df[[strata_colname]][
        match(general_info$patno, strata_df$patno)
      ]

  } else if (!is.null(strata_colname) && !is.null(strata_itemname)) {
    # Strata is whether the patient has a specific item in the column

    strata_patnos <- unique(
      strata_df$patno[strata_df[[strata_colname]] == strata_itemname]
    )

    general_info[[strata_itemname]] <-
      general_info$patno %in% strata_patnos
  }

  return(general_info)
}