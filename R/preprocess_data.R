

#data_directory   <- "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data"
#output_directory <- "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_processed_data"

# Import

preprocess_data <- function(
  processed_folder,
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
  
  # Create processed_folder if it does not exist
  if (!dir.exists(processed_folder)) {
    dir.create(
      processed_folder,
      recursive = TRUE
    )
  }
  
  ### Read files
  
  general_info_raw <- read_excel(general_info_file) # NMDS14B-Inkl-screen-EoS.xlsx
  mrd_raw <- read_excel(mrd_file) # NMDS14B_MRD.XLSX
  dli_raw <- read_excel(dli_file) # NMDS14B_dlitrt.xlsx
  aza_raw <- read_excel(aza_file) # NMDS14B_azacitkurer.xlsx
  immune_raw <- read_excel(immune_file) # NMDS14B_immunsupptrtm.xlsx
  gvhd_raw <- read_excel(gvhd_file) # NMDS14B_gvhddat.xlsx
  ngs_raw <- read_excel(ngs_file) # NGS_lista_NMDSG14B2.xlsx
  immune_suppression_filter <- read.csv(immune_filter_file, header = TRUE, sep = ";") # immune_suppression_filter.csv
  
  ### FILTERING FOR SWIMMERPLOT
  
  # --- FILTERING ---
  
  # Create end_date_df based on general_info_raw and mrd_raw
  end_date_df <- general_info_raw %>%
    select(patno, termindat, transpldt) %>%
    left_join(
      mrd_raw %>%
        group_by(patno) %>%
        summarise(MRDdat = max(MRDdat, na.rm = TRUE), .groups = "drop"),
      by = "patno"
    ) %>%
    mutate(end_date = coalesce(termindat, MRDdat)) %>%
    mutate(rel_term_dat = as.numeric(difftime(as.Date(end_date), as.Date(transpldt), units = "days"))) %>%
    select(patno, transpldt, rel_term_dat)
  
  # Transpose aza data frame, calculate relative aza dates, remove aza after rel_term_dat
  aza <- aza_raw %>%
    pivot_longer(starts_with("azacitstdat"), names_to = "timepoint", values_to = "azacitstartdat") %>%
    select(patno, azacitstartdat) %>%
    filter(!is.na(azacitstartdat)) %>%
    distinct() %>%
    left_join(end_date_df, by = "patno") %>%
    mutate(rel_aza_dat = as.numeric(difftime(as.Date(azacitstartdat), as.Date(transpldt), units = "days"))) %>%
    filter(rel_aza_dat <= rel_term_dat)
  
  # Calculate relative dli dates, remove dli after rel_term_dat
  dli <- dli_raw %>%
    left_join(end_date_df, by = "patno") %>%
    mutate(rel_dli_dat = as.numeric(difftime(as.Date(dlidat), as.Date(transpldt), units = "days"))) %>%
    filter(rel_dli_dat <= rel_term_dat)
  
  # Merge dli and aza data frames
  treatment <- bind_rows(
    dli %>% select(patno, rel_treatment_dat = rel_dli_dat) %>% mutate(treatment = "Donor Lymphocyte Infusion"),
    aza %>% select(patno, rel_treatment_dat = rel_aza_dat) %>% mutate(treatment = "Azacitidine")
  )
  
  # Calculate agvhd_raw_merged and cgvhd_raw_merged
  agvhd_raw_merged <- bind_rows(
    gvhd_raw %>% select(patno, agvhdstage, gvhddate),
    gvhd_raw %>% select(patno, agvhdstage = agvhdmaxstage, gvhddate = agvhdmaxdt)
  ) %>%
    filter(!is.na(agvhdstage), !is.na(gvhddate)) %>%
    distinct() %>%
    mutate(gvhd = "Acute GVHD")
  
  cgvhd_raw_merged <- bind_rows(
    gvhd_raw %>% select(patno, cgvhdstage, gvhddate),
    gvhd_raw %>% select(patno, cgvhdstage = cgvhdmaxstage, gvhddate = cgvhdmaxdt)
  ) %>%
    filter(!is.na(cgvhdstage), !is.na(gvhddate)) %>%
    distinct() %>%
    mutate(gvhd = "Chronic GVHD")
  
  # Calculate gvhd_raw_merged
  gvhd_processed <- bind_rows(cgvhd_raw_merged, agvhd_raw_merged) %>%
    left_join(end_date_df, by = "patno") %>%
    mutate(rel_gvhd_dat = as.numeric(difftime(as.Date(gvhddate), as.Date(transpldt), units = "days"))) %>%
    filter(rel_gvhd_dat <= rel_term_dat) %>%
    mutate(cgvhdstage = if_else(cgvhdstage %in% c("N/A", "N/K", ".", "N/D"), NA, cgvhdstage))
  
  # Add mrd_category column to mrd_raw, calculate rel_mrd_dat
  mrd_all <- mrd_raw %>%
    mutate(mrd_category = case_when(
      level <  0.1 ~ "Negative (< 0.1)",
      level <  0.5 ~ "Low (0.1 - 0.5)",
      level <  1   ~ "Intermediate (0.5 - 1)",
      level >= 1   ~ "High (> 1)"
    )) %>%
    group_by(patno, MRDdat) %>%
    slice_max(order_by = level, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(end_date_df, by = "patno") %>%
    mutate(
      rel_mrd_dat = as.numeric(
        difftime(as.Date(MRDdat),
                 as.Date(transpldt),
                 units = "days")
      )
    ) %>%
    filter(rel_mrd_dat >= 0)
  
  # Create a list of patno which have level >= 10 at the last measurement, these
  # also count as relapses.
  mrd_relapse_cases <- mrd_all %>%
    arrange(patno, rel_mrd_dat) %>%
    group_by(patno) %>%
    slice_tail(n = 1) %>%
    filter(level >= 10) %>%
    pull(patno)
  
  # Change name of mutname column in p2_mrd from mutname to Mutation
  mrd_all <- mrd_all %>% rename(Mutation = mutname)
  
  # Filter immune suppression data
  standardize_drug <- function(drug, mapping_df) {
    match_idx <- which(
      map_lgl(mapping_df$pattern,
              ~ str_detect(drug, regex(.x, ignore_case = TRUE)))
    )
    if(length(match_idx) > 0) {
      return(mapping_df$standardized_name[match_idx[1]])
    }
    return(NA_character_)
  }
  
  immune <- immune_raw %>%
    left_join(end_date_df, by = "patno") %>%
    mutate(
      rel_immune_dat = as.numeric(
        difftime(as.Date(drugdt),
                 as.Date(transpldt),
                 units = "days")
      )
    ) %>%
    filter(rel_immune_dat <= rel_term_dat) %>%
    filter(rel_immune_dat > 0) %>%
    mutate(
      drugname = tolower(drugname),
      drugname_standardized = map_chr(
        drugname,
        standardize_drug,
        mapping_df = immune_suppression_filter
      )
    ) %>%
    left_join(
      immune_suppression_filter %>%
        distinct(standardized_name, exclude),
      by = c("drugname_standardized" = "standardized_name")
    ) %>%
    filter(is.na(exclude) | !exclude) %>%
    select(
      patno,
      immunsupptreatm,
      drugname,
      drugstopped,
      rel_term_dat,
      rel_immune_dat,
      drugname_standardized
    ) %>%
    distinct()
  
  # Calulate outcomes based on general_info and mrd_relapse_cases
  general_info <- general_info_raw %>%
    mutate(outcome = case_when(
      eosreason == "Death" ~ "Nonrelapse mortality",
      eosreason == "Full hematological relapse" ~ "Relapse",
      eosreason == "Consent withdrawal" ~ "Other reason",
      is.na(eosreason) & patno %in% mrd_relapse_cases ~ "Relapse",
      TRUE ~ "Remission")) %>%
    mutate(ipssm_title = case_when(ipssm < -1.5 ~ "Very Low",
                                   ipssm >= -1.5 & ipssm < -0.5 ~ "Low",
                                   ipssm >= -0.5 & ipssm < 0 ~ "Moderate Low",
                                   ipssm >= 0 & ipssm < 0.5 ~ "Moderate High",
                                   ipssm >= 0.5 & ipssm < 1.5 ~ "High",
                                   ipssm >= 1.5 ~ "Very High")) %>%
    left_join(end_date_df %>% select(patno, rel_term_dat), by = "patno")
  
  # NGS Data filtering
  # Add column with list of genes for eacn patno in ngs_raw
  # 1) Create a table of unique gene lists per patient
  gene_lists <- ngs_raw %>%
    filter(!is.na(Studienummer)) %>%
    group_by(Studienummer) %>%
    summarise(mutlist = paste(unique(Gen), collapse = ", "), .groups = "drop")
  
  # 2) Join it back to the original data
  ngs_processed <- ngs_raw %>%
    left_join(gene_lists, by = "Studienummer") %>%
    mutate(mutname = paste0(Gen, "_", `cDNA förändring`))
  
  # Change name of Studienummer column in ngs_processed from Studienummer to patno
  ngs_processed <- ngs_processed %>% rename(patno = Studienummer)
  
  # Change ngs_processed$patno to double
  ngs_processed$patno <- as.double(ngs_processed$patno)
  
  # --- IMMUNE SUPPRESSION INTERVALS ---
  
  interval_finder <- function(df) {
    
    df %>%
      arrange(patno, drugname_standardized, rel_immune_dat) %>%
      group_by(patno, drugname_standardized) %>%
      group_modify(~ {
        
        stop_idx <- which(.x$drugstopped == "Yes")
        
        if (length(stop_idx) == 0) {
          return(tibble(
            interval_no = 1,
            interval_start = min(.x$rel_immune_dat),
            interval_end = max(.x$rel_term_dat)
          ))
        }
        
        starts <- c(1, stop_idx[-length(stop_idx)] + 1)
        ends <- stop_idx
        
        tibble(
          interval_no = seq_along(starts),
          interval_start = .x$rel_immune_dat[starts],
          interval_end = .x$rel_immune_dat[ends]
        )
      }) %>%
      ungroup()
  }
  
  interval_df <- interval_finder(immune)
  
  overlapping_interval_df <- interval_df %>%
    arrange(patno, interval_start, interval_end) %>%
    group_by(patno) %>%
    mutate(
      running_max_end = cummax(interval_end),
      new_group = interval_start > lag(running_max_end, default = first(interval_end)),
      overlap_group = cumsum(new_group) + 1
    ) %>%
    group_by(patno, overlap_group) %>%
    summarise(
      interval_start = min(interval_start),
      interval_end = max(interval_end),
      .groups = "drop"
    )
  
  ### EXPORT FINAL CSV FILES
  
  # Export treatment_processed.csv file
  write.table(
    treatment,
    file.path(processed_folder, "treatment_processed.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
  )
  
  # Export gvhd_processed.csv file
  write.table(
    gvhd_processed,
    file.path(processed_folder, "gvhd_processed.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
  )
  
  # Export immune_supp_intervals.csv file
  write.table(
    overlapping_interval_df,
    file.path(processed_folder, "immune_supp_intervals.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
  )
  
  # Export ngs_processed.csv file
  write.table(
    ngs_processed,
    file.path(processed_folder, "ngs_processed.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
  )
  
  # Export mrd_processed.csv file
  write.table(
    mrd_all,
    file.path(processed_folder, "mrd_processed.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
  )
  
  # Export general_info_processed.csv file
  write.table(
    general_info,
    file.path(processed_folder, "general_info_processed.csv"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";"
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
