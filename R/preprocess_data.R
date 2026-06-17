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
  immune_filter_file
) {

  # Check that required files are present
  required_files <- c(
    general_info_file,
    mrd_file,
    dli_file,
    aza_file,
    immune_file,
    gvhd_file,
    ngs_file,
    immune_filter_file
  )

  missing_files <- required_files[
    !file.exists(required_files
    )
  ]

  if (length(missing_files) > 0) {

    stop(
      paste(
        "Missing input files:",
        paste(missing_files,
              collapse = ", ")
      )
    )
  }

  ### Read files

  general_info_raw <- readxl::read_excel(general_info_file)
  mrd_raw <- readxl::read_excel(mrd_file)
  dli_raw <- readxl::read_excel(dli_file)
  aza_raw <- readxl::read_excel(aza_file)
  immune_raw <- readxl::read_excel(immune_file)
  gvhd_raw <- readxl::read_excel(gvhd_file)
  ngs_raw <- readxl::read_excel(ngs_file)

  # Make column names ASCII-safe
  names(ngs_raw) <- iconv(
    names(ngs_raw), from = "UTF-8", to = "ASCII//TRANSLIT"
  )
  names(ngs_raw) <- gsub(" ", "_", names(ngs_raw), fixed = TRUE)
  immune_suppression_filter <- read.csv(
    immune_filter_file,
    header = TRUE,
    sep = ";"
  )

  ### FILTERING FOR SWIMMERPLOT

  # --- FILTERING ---

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

  # Transpose aza data frame,
  # calculate relative aza dates,
  # remove aza after rel_term_dat
  aza <- aza_raw |>
    tidyr::pivot_longer(
      dplyr::starts_with("azacitstdat"),
      names_to = "timepoint",
      values_to = "azacitstartdat"
    ) |>
    dplyr::select(patno, azacitstartdat) |>
    dplyr::filter(!is.na(azacitstartdat)) |>
    dplyr::distinct() |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_aza_dat = as.numeric(difftime(
        as.Date(azacitstartdat),
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

  # Add mrd_category column to mrd_raw, calculate rel_mrd_dat
  mrd_all <- mrd_raw |>
    dplyr::mutate(mrd_category = dplyr::case_when(
      level <  0.1 ~ "Negative (< 0.1)",
      level <  0.5 ~ "Low (0.1 - 0.5)",
      level <  1   ~ "Intermediate (0.5 - 1)",
      level >= 1   ~ "High (> 1)"
    )) |>
    dplyr::group_by(patno, MRDdat) |>
    dplyr::slice_max(order_by = level, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(end_date_df, by = "patno") |>
    dplyr::mutate(
      rel_mrd_dat = as.numeric(
        difftime(as.Date(MRDdat),
                 as.Date(MRDdat),
                 as.Date(transpldt),
                 units = "days")
      )
    ) |>
    dplyr::filter(rel_mrd_dat >= 0)

  # Create a list of patno which have level >= 10 at the last measurement, these
  # also count as relapses.
  mrd_relapse_cases <- mrd_all |>
    dplyr::arrange(patno, rel_mrd_dat) |>
    dplyr::group_by(patno) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::filter(level >= 10) |>
    dplyr::pull(patno)

  # Change name of mutname column in p2_mrd from mutname to Mutation
  mrd_all <- mrd_all |> dplyr::rename(Mutation = mutname)

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
      drugname_standardized
    ) |>
    dplyr::distinct()

  # Calulate outcomes based on general_info and mrd_relapse_cases
  general_info <- general_info_raw |>
    dplyr::mutate(outcome = dplyr::case_when(
      eosreason == "Death" ~ "Nonrelapse mortality",
      eosreason == "Full hematological relapse" ~ "Relapse",
      eosreason == "Consent withdrawal" ~ "Other reason",
      is.na(eosreason) & patno %in% mrd_relapse_cases ~ "Relapse",
      TRUE ~ "Remission"
    )) |>
    dplyr::mutate(ipssm_title = dplyr::case_when(
      ipssm < -1.5 ~ "Very Low",
      ipssm >= -1.5 & ipssm < -0.5 ~ "Low",
      ipssm >= -0.5 & ipssm < 0 ~ "Moderate Low",
      ipssm >= 0 & ipssm < 0.5 ~ "Moderate High",
      ipssm >= 0.5 & ipssm < 1.5 ~ "High",
      ipssm >= 1.5 ~ "Very High"
    )) |>
    dplyr::left_join(
      end_date_df |> dplyr::select(patno, rel_term_dat), by = "patno"
    )

  # NGS Data filtering
  gene_lists <-
    ngs_raw |>
    dplyr::filter(!is.na(Studienummer)) |>
    dplyr::summarise(
      mutlist = paste(unique(Gen), collapse = ", "),
      .by = Studienummer
    )

  ngs_processed <-
    ngs_raw |>
    dplyr::left_join(gene_lists, by = "Studienummer") |>
    dplyr::mutate(
      mutname = paste0(Gen, "_", "cDNA förändring"),
      patno = as.double(Studienummer)
    ) |>
    dplyr::select(-Studienummer)

  interval_df <- interval_finder(immune)

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

  return(
    list(
      general_info = general_info,
      treatment = treatment,
      mrd = mrd_all,
      gvhd = gvhd_processed,
      ngs = ngs_processed,
      immune_events = immune,
      immune_intervals = overlapping_interval_df

    )
  )
}
