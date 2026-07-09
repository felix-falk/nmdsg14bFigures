#' Preprocess data ahead of manual filtering and plotting and.
#'
#' @param general_info_file Path to the general info Excel file.
#' @param mrd_file Path to the MRD Excel file.
#' @param dli_file Path to the DLI Excel file.
#' @param aza_file Path to the Azacitidine Excel file.
#' @param immune_file Path to the immune suppression Excel file.
#' @param gvhd_file Path to the GVHD events Excel file.
#' @param ngs_file Path to the NGS Excel file.
#' @param immune_filter_file Path to the immune suppression filter CSV file.
#' @param chimerism_file Path to the chimerism Excel file.
#' @param strata_filename Name of the file containing the strata of interest.
#' @param strata_colname Name of the column containing the strata of interest.
#' @param strata_itemname Name of the item of the strata of interest.
#' @returns A list of processed data frames.
#' @examples
#' preprocess_data(
#' general_info_file = "general_info.xlsx",
#' mrd_file = "mrd.xlsx",
#' dli_file = "dli.xlsx",
#' aza_file = "aza.xlsx",
#' immune_file = "immune.xlsx",
#' gvhd_file = "gvhd.xlsx",
#' ngs_file = "ngs.xlsx",
#' immune_filter_file = "immune_filter.csv"
#' )
preprocess_data <- function(
  general_info_file,
  mrd_file,
  dli_file,
  aza_file,
  immune_file,
  gvhd_file,
  ngs_file,
  immune_filter_file,
  chimerism_file,
  strata_filename = list(
    "general_info",
    "aza",
    "dli",
    "ngs",
    "chimerism",
    "mrd",
    "immune"
  ),
  strata_colname = NULL,
  strata_itemname = NULL
) {

  # Only general_info_file and mrd_file are strictly required.
  if (is.null(general_info_file) || !file.exists(general_info_file)) {
    stop("Missing required input file: general_info_file")
  }
  if (is.null(mrd_file) || !file.exists(mrd_file)) {
    stop("Missing required input file: mrd_file")
  }

  ### Read files (optional files are tolerated)

  # Read the mandatory excel files
  general_info_raw <- readxl::read_excel(general_info_file)
  mrd_raw <- readxl::read_excel(mrd_file)

  # Check that required columns are present
  column_check(general_info_raw, c(
    "patno",
    "transpldt",
    "termindat",
    "eosreason",
    "mdsdiagnosis",
    "karyotyp",
    "deathcause"
  ))
  column_check(mrd_raw, c(
    "patno",
    "MRDdat",
    "mutname",
    "level"
  ))

  if (is.null(dli_file) || !file.exists(dli_file)) {
    dli_raw <- tibble::tibble(patno = double(), dlidat = as.Date(character()))
  } else {
    dli_raw <- readxl::read_excel(dli_file)
    column_check(dli_raw, c("patno", "dlidat"))
  }

  if (is.null(aza_file) || !file.exists(aza_file)) {
    aza_raw <- tibble::tibble(
      patno = double(), azacitstart = as.Date(character())
    )
  } else {
    aza_raw <- readxl::read_excel(aza_file)
    column_check(aza_raw, c("patno", "azacitstart"))
  }

  if (is.null(immune_file) || !file.exists(immune_file)) {
    immune_raw <- tibble::tibble(
      patno = double(),
      drugdt = as.Date(character()),
      drugname = character(),
      drugstopped = character()
    )
  } else {
    immune_raw <- readxl::read_excel(immune_file)
    column_check(immune_raw, c("patno", "drugname", "drugdt", "drugstopped", "drugdose"))
  }

  if (is.null(gvhd_file) || !file.exists(gvhd_file)) {
    gvhd_raw <- tibble::tibble(
      patno = double(),
      agvhdstage = character(),
      gvhddate = as.Date(character()),
      agvhdmaxstage = character(),
      agvhdmaxdt = as.Date(character()),
      cgvhdstage = character(),
      cgvhdmaxstage = character(),
      cgvhdmaxdt = as.Date(character())
    )
  } else {
    gvhd_raw <- readxl::read_excel(gvhd_file)
    column_check(gvhd_raw, c(
      "patno",
      "gvhddate",
      "agvhdstage",
      "cgvhdstage",
      "agvhdmaxstage",
      "agvhdmaxdt",
      "cgvhdmaxstage",
      "cgvhdmaxdt"
    ))
  }

  if (is.null(ngs_file) || !file.exists(ngs_file)) {
    ngs_raw <- tibble::tibble(patno = double(), Gen = character())
  } else {
    ngs_raw <- readxl::read_excel(ngs_file)
    column_check(ngs_raw, c("patno", "Gen"))
  }

  if (is.null(chimerism_file) || !file.exists(chimerism_file)) {
    chimerism_raw <- tibble::tibble(
      patno = double(),
      chimbmdt = as.Date(character()),
      CD1 = numeric()
    )
  } else {
    chimerism_raw <- readxl::read_excel(chimerism_file)
    column_check(chimerism_raw, c("patno", "chimbmdt"))
  }

  # Read immune suppresion filter file
  if (is.null(immune_filter_file) || !file.exists(immune_filter_file)) {
    immune_suppression_filter <- tibble::tibble(
      pattern = character(),
      standardized_name = character(),
      exclude = logical()
    )
  } else {
    immune_suppression_filter <- read.csv(
      immune_filter_file,
      header = TRUE,
      sep = ";"
    )
    column_check(immune_suppression_filter, c(
      "pattern",
      "standardized_name",
      "exclude"
    ))
  }

  # --- FILTERING ---

  # Create end_date_df based on general_info_raw and mrd_raw
  end_date_df <- create_end_date_df(general_info_raw, mrd_raw)

  # Create treatment data frame based on aza_raw, dli_raw and end_date_df
  treatment <- create_treatment_df(aza_raw, dli_raw, end_date_df)

  # Create processed gvhd data frame based on ghvd_raw and end_date_df
  gvhd_processed <- create_gvhd_df(gvhd_raw, end_date_df)

  # Add mrd_category column to mrd_raw, calculate rel_mrd_dat
  # Calculate first dates with positive MRD at three levels
  mrd_all <- mrd_raw |>
    dplyr::mutate(mrd_category = dplyr::case_when(
      level < 0.1 ~ "Negative (< 0.1)",
      level < 0.5 ~ "Low (0.1 - 0.5)",
      level < 1.0 ~ "Intermediate (0.5 - 1)",
      TRUE        ~ "High (> 1)"
    )) |>
    dplyr::group_by(patno, MRDdat) |>
    dplyr::slice_max(order_by = level, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_mrd_dat = as.numeric(
        difftime(as.Date(MRDdat), as.Date(transpldt), units = "days")
      )
    ) |>
    dplyr::filter(rel_mrd_dat >= 0) |>
    dplyr::group_by(patno) |>
    dplyr::mutate(
      `rel_pos_mrd_dat_0.1` = if (any(level >= 0.1))
        min(rel_mrd_dat[level >= 0.1])
      else
        NA_real_,
      `rel_pos_mrd_dat_0.5` = if (any(level >= 0.5))
        min(rel_mrd_dat[level >= 0.5])
      else
        NA_real_,
      `rel_pos_mrd_dat_1.0` = if (any(level >= 1.0))
        min(rel_mrd_dat[level >= 1.0])
      else
        NA_real_
    ) |>
    dplyr::ungroup()

  # Create a list of patno which have level >= 10 at the last measurement, these
  # also count as relapses.
  mrd_relapse_cases <- mrd_all |>
    dplyr::arrange(patno, rel_mrd_dat) |>
    dplyr::group_by(patno) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::filter(level >= 10) |>
    dplyr::pull(patno)

  # Change name of mutname column in mrd_all from mutname to Mutation
  mrd <- mrd_all |> dplyr::rename(Mutation = mutname)

  immune <- immune_raw |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_immune_dat = as.numeric(
        difftime(as.Date(drugdt), as.Date(transpldt), units = "days")
      ),
      drugname = tolower(drugname),
      drugname_standardized = purrr::map_chr(
        drugname, standardize_drug, mapping_df = immune_suppression_filter
      )
    ) |>
    dplyr::filter(rel_immune_dat > 0, rel_immune_dat <= rel_term_dat) |>
    dplyr::left_join(
      immune_suppression_filter |>
        dplyr::distinct(standardized_name, exclude),
      by = c("drugname_standardized" = "standardized_name")
    ) |>
    dplyr::filter(!dplyr::coalesce(exclude, FALSE)) |>
    dplyr::select(
      patno,
      immunsupptreatm,
      drugname,
      drugstopped,
      rel_term_dat,
      rel_immune_dat,
      drugname_standardized,
      drugdose
    ) |>
    dplyr::distinct()

  # Create mrd_pos_dates data frame for adding to general_info
  mrd_pos_dates <- mrd_all |>
    dplyr::distinct(
      patno,
      `rel_pos_mrd_dat_0.1`,
      `rel_pos_mrd_dat_0.5`,
      `rel_pos_mrd_dat_1.0`
    )

  # Calulate outcomes based on general_info and mrd_relapse_cases
  general_info <- general_info_raw |>
    dplyr::mutate(outcome = dplyr::case_when(
      eosreason == "Death" ~ "Nonrelapse mortality",
      eosreason == "Full hematological relapse" ~ "Relapse",
      eosreason == "Consent withdrawal" ~ "Other exclusion reason",
      eosreason == "Patients wish" ~ "Other exclusion reason",
      eosreason == "Other (please use comment)" ~ "Other exclusion reason",
      eosreason == "Other reason" ~ "Other exclusion reason",
      eosreason == "Relapse" ~ "Relapse",
      eosreason == "Two years passed" ~ "Remission",
      is.na(eosreason) & patno %in% mrd_relapse_cases ~ "Relapse",
      TRUE ~ "Remission"
    )) |>
    dplyr::left_join(
      end_date_df |>
        dplyr::select(patno, rel_term_dat),
      by = "patno"
    ) |>
    dplyr::left_join(
      mrd_pos_dates,
      by = "patno"
    )

  # Add ipssm_title column to general_info if ipssm is present
  if ("ipssm" %in% colnames(general_info)) {
    general_info <- general_info |>
      dplyr::mutate(ipssm_title = dplyr::case_when(
        .data$ipssm < -1.5 ~ "Very Low",
        .data$ipssm >= -1.5 & .data$ipssm < -0.5 ~ "Low",
        .data$ipssm >= -0.5 & .data$ipssm < 0 ~ "Moderate Low",
        .data$ipssm >= 0 & .data$ipssm < 0.5 ~ "Moderate High",
        .data$ipssm >= 0.5 & .data$ipssm < 1.5 ~ "High",
        .data$ipssm >= 1.5 ~ "Very High"
      ))
  }

  # NGS Data filtering
  ngs <- create_ngs_df(ngs_raw)

  # Calculate relative immune suppression drug concentrations
  immune <- calc_immune_percentage_dose(immune)

  # Create immune intervals data frame
  interval_df <- interval_finder(immune)

  # Find overlapping immune suppression intervals
  #overlapping_interval_df <- overlapping_interval_finder(interval_df)

  # Create ciclosporine intervals data frame
  #ciclosporine_interval_df <- interval_finder(
  #  immune |>
  #    dplyr::filter(drugname_standardized == "ciclosporin")
  #)

  # Find overlapping immune suppression intervals for ciclosporin
  #overlapping_ciclosporin_interval_df <- overlapping_interval_finder(
  #  ciclosporine_interval_df
  #)

  # --- IMMUNE RECTANGLES ---

  # Based on gvhd, compute earliest relevant GVHD event per patient:
  # - aGVHD grade III-IV
  # - cGVHD severe
  # - cGVHD moderate (only if the cGVHD moderate event falls inside an
  #   overlapping immune suppression interval)

  # Acute GVHD grade 3-4
  agvhd_events <- gvhd_processed |>
    dplyr::filter(gvhd == "Acute GVHD") |>
    dplyr::mutate(
      stage_norm = toupper(as.character(agvhdstage)),
      stage_num = suppressWarnings(as.numeric(stage_norm))
    ) |>
    dplyr::filter(
      stage_norm %in% c(3, 4) |
        (!is.na(stage_num) & stage_num >= 3)
    ) |>
    dplyr::select(patno, rel_gvhd_dat) |>
    dplyr::mutate(event_type = "aGVHD 3-4")

  # Chronic GVHD severe
  cgvhd_severe_events <- gvhd_processed |>
    dplyr::filter(gvhd == "Chronic GVHD") |>
    dplyr::mutate(stage_norm = tolower(as.character(cgvhdstage))) |>
    dplyr::filter(grepl("Severe", stage_norm)) |>
    dplyr::select(patno, rel_gvhd_dat) |>
    dplyr::mutate(event_type = "cGVHD severe")

  # Chronic GVHD moderate but only keep those overlapping immune intervals
  cgvhd_moderate_events <- gvhd_processed |>
    dplyr::filter(gvhd == "Chronic GVHD") |>
    dplyr::mutate(stage_norm = tolower(as.character(cgvhdstage))) |>
    dplyr::filter(grepl("Moderate", stage_norm)) |>
    dplyr::left_join(interval_df, by = "patno") |>
    dplyr::filter(
      !is.na(interval_start) &
        rel_gvhd_dat >= interval_start &
        rel_gvhd_dat <= interval_end
    ) |>
    dplyr::select(patno, rel_gvhd_dat) |>
    dplyr::mutate(event_type = "cGVHD Moderate (overlaps immune)")

  # Combine and pick earliest event per patient
  gvhd_events <- dplyr::bind_rows(
    agvhd_events,
    cgvhd_severe_events,
    cgvhd_moderate_events
  ) |>
    dplyr::arrange(patno, rel_gvhd_dat) |>
    dplyr::group_by(patno) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # Calculate events based on general_info and earliest GVHD events.
  # If a GVHD event (as computed in `gvhd_events`) occurs before the
  # censoring/termination time (`rel_term_dat`), register that as the
  # event (time and status = 1). Otherwise fall back to the previous
  # rules based on `outcome` (death/relapse/other).

  # Also, add OS and RFS events.
  general_info <- general_info |>
    dplyr::left_join(
      gvhd_events |>
        dplyr::select(
          patno,
          gvhd_event_time = rel_gvhd_dat,
          gvhd_event_type = event_type
        ),
      by = "patno"
    ) |>
    dplyr::mutate(

      # Composite GVHD endpoint
      event_time = dplyr::case_when(
        !is.na(gvhd_event_time) &
          gvhd_event_time <= rel_term_dat ~ gvhd_event_time,
        eosreason %in% c(
          "Death",
          "Full hematological relapse",
          "Other reason",
          "Consent withdrawal",
          "2 years post HCT"
        ) ~ rel_term_dat,
        TRUE ~ NA_real_
      ),

      event_status = dplyr::case_when(
        !is.na(gvhd_event_time) &
          gvhd_event_time <= rel_term_dat ~ 1,
        eosreason %in% c(
          "Death",
          "Full hematological relapse"
        ) ~ 1,
        eosreason %in% c(
          "Other reason",
          "Consent withdrawal",
          "2 years post HCT"
        ) ~ 0,
        TRUE ~ NA_real_
      ),

      # Overall survival (death only)
      os_time = rel_term_dat,
      os_status = dplyr::case_when(
        eosreason == "Death" ~ 1,
        eosreason %in% c(
          "Full hematological relapse",
          "Other reason",
          "Consent withdrawal",
          "2 years post HCT"
        ) ~ 0,
        TRUE ~ NA_real_
      ),

      # Relapse-free survival (relapse or death)
      rfs_time = rel_term_dat,
      rfs_status = dplyr::case_when(
        eosreason %in% c(
          "Death",
          "Full hematological relapse"
        ) ~ 1,
        eosreason %in% c(
          "Other reason",
          "Consent withdrawal",
          "2 years post HCT"
        ) ~ 0,
        TRUE ~ NA_real_
      )
    )

  # Transpose chimerism data, calculate relative chimerism dates
  # keep only CD33, CD34.
  chimerism <- create_chimerism_df(chimerism_raw, end_date_df)

  # Add dli, aza or ngs strata column to general_info (optional)

  # Add optional strata column to general_info
  general_info <- add_strata(
    general_info,
    strata_filename,
    strata_colname,
    strata_itemname
  )

  print(interval_df |> dplyr::filter(patno == "1207"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "1408"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "1411"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "1504"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2004"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2017"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2023"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2033"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2035"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "2037"), width = Inf)
  print(interval_df |> dplyr::filter(patno == "4007"), width = Inf)

  return(
    list(
      general_info = general_info,
      treatment = treatment,
      mrd = mrd,
      gvhd = gvhd_processed,
      gvhd_events = gvhd_events,
      ngs = ngs,
      immune_events = immune,
      immune_intervals = interval_df,
      chimerism = chimerism
    )
  )
}
