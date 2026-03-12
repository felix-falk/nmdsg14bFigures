# Load required packages

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(geomtextpath)
library(ggnewscale)

# Set working directory
setwd("/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data")

# Import datasets
p2_clinical <- read_excel("NMDS14B-Inkl-screen-EoS.xlsx")
p2_mrd <- read_excel("NMDS14B_MRD.XLSX")
p2_gvhd <- read_excel("NMDS14B_gvhd.xlsx")
p2_aza <- read_excel("NMDS14B_azacittrt_lang.xlsx")
p2_is <- read_excel("NMDS14B_immunsupptrtm.xlsx")
p2_dli <- read_excel("NMDS14B_dlitrt.xlsx")

# Add transpldt to each data frame, by patno
p2_mrd <- p2_mrd %>% left_join(p2_clinical[,c("patno", "transpldt")], by = "patno", keep = FALSE)
p2_gvhd <- p2_gvhd %>% left_join(p2_clinical[,c("patno", "transpldt")], by = "patno", keep = FALSE)
p2_aza <- p2_aza %>% left_join(p2_clinical[,c("patno", "transpldt")], by = "patno", keep = FALSE)
p2_is <- p2_is %>% left_join(p2_clinical[,c("patno", "transpldt")], by = "patno", keep = FALSE)
p2_dli <- p2_dli %>% left_join(p2_clinical[,c("patno", "transpldt")], by = "patno", keep = FALSE)

# Calculate relative dates for each data frame
p2_clinical$Death <- as.Date(p2_clinical$deathdat) - as.Date(p2_clinical$transpldt)
p2_clinical$Relapse <- as.Date(p2_clinical$relapsedat) - as.Date(p2_clinical$transpldt)
p2_mrd$rel_mrd_dat <- as.Date(p2_mrd$MRDdat) - as.Date(p2_mrd$transpldt)
p2_gvhd$rel_gvhd_dat <- as.Date(p2_gvhd$gvhddate) - as.Date(p2_gvhd$transpldt)
p2_gvhd$rel_max_c_gvhd_dat <- as.Date(p2_gvhd$cgvhdmaxdt) - as.Date(p2_gvhd$transpldt)
p2_gvhd$rel_max_a_gvhd_dat <- as.Date(p2_gvhd$agvhdmaxdt) - as.Date(p2_gvhd$transpldt)
p2_is$is_stop_dat <- as.Date(p2_is$drugdt) - as.Date(p2_is$transpldt)
p2_aza$rel_aza_dat <- as.Date(p2_aza$azacitstartdat) - as.Date(p2_aza$transpldt)
p2_dli$rel_dli_dat <- as.Date(p2_dli$dlidat) - as.Date(p2_dli$transpldt)

# Change name of mutname column in p2_mrd from mutname to Mutation
colnames(p2_mrd)[6] <- "Mutation"

# Helper function to clean GVHD data
clean_gvhd <- function(df, rel_max_col, stage_col, max_stage_col, type) {
  df %>%
    select(patno, rel_gvhd_dat, !!sym(rel_max_col), !!sym(stage_col), !!sym(max_stage_col)) %>%
    mutate(
      merged_stage = if_else(
        !is.na(!!sym(max_stage_col)) & !!sym(max_stage_col) >= !!sym(stage_col),
        !!sym(max_stage_col),
        !!sym(stage_col)
      )
    ) %>%
    pivot_longer(
      cols = c(rel_gvhd_dat, !!sym(rel_max_col)),
      names_to = "date_type",
      values_to = "relative_date"
    ) %>%
    filter(!is.na(relative_date)) %>%
    transmute(
      patno,
      relative_date,
      agvhd_stage = if(type == "a") merged_stage else NA_character_,
      cgvhd_stage = if(type == "c") merged_stage else NA_character_,
      source_dataframe = paste0(type, "GVHD"),
      Mutation = NA_character_,
      level = NA_real_,
      Event = NA_character_
    )
}

# Clean acute and chronic GVHD
p2_a_gvhd <- clean_gvhd(p2_gvhd, "rel_max_a_gvhd_dat", "agvhdstage", "agvhdmaxstage", "a")
p2_c_gvhd <- clean_gvhd(p2_gvhd, "rel_max_c_gvhd_dat", "cgvhdstage", "cgvhdmaxstage", "c")

# Clean IS
p2_is_events <- p2_is %>%
  filter(drugstopped == "Yes") %>%
  group_by(patno) %>%
  slice_max(order_by = is_stop_dat, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    patno,
    relative_date = is_stop_dat,
    source_dataframe = "IS Stop",
    agvhd_stage = NA_character_,
    cgvhd_stage = NA_character_,
    Mutation = NA_character_,
    level = NA_real_,
    Event = NA_character_
  )

# Clean MRD
p2_mrd_events <- p2_mrd %>%
  transmute(
    patno,
    relative_date = rel_mrd_dat,
    source_dataframe = "MRD",
    agvhd_stage = NA_character_,
    cgvhd_stage = NA_character_,
    Mutation,
    level,
    Event = NA_character_
  )

# Clean clinical events
p2_clinical_events <- p2_clinical %>%
  pivot_longer(
    cols = c(Relapse, Death),
    names_to = "Event",
    values_to = "relative_date"
  ) %>%
  filter(!is.na(relative_date)) %>%
  transmute(
    patno,
    relative_date,
    source_dataframe = "Outcome",
    agvhd_stage = NA_character_,
    cgvhd_stage = NA_character_,
    Mutation = NA_character_,
    level = NA_real_,
    Event
  )

# Clean p2_aza data frame
p2_aza_events <- p2_aza %>%
  transmute(
    patno,
    relative_date = rel_aza_dat,
    source_dataframe = "Azacitidine",
    agvhd_stage = NA_character_,
    cgvhd_stage = NA_character_,
    Mutation = NA_character_,
    level = NA_real_,
    Event = NA_character_
  ) %>% 
  filter(!is.na(relative_date))

# Clean p2_dli data frame
p2_dli_events <- p2_dli %>%
  transmute(
    patno,
    relative_date = rel_dli_dat,
    source_dataframe = "DLI",
    agvhd_stage = NA_character_,
    cgvhd_stage = NA_character_,
    Mutation = NA_character_,
    level = NA_real_,
    Event = NA_character_
  )

# Combine everything
p2_merged <- bind_rows(
  p2_a_gvhd,
  p2_c_gvhd,
  p2_is_events,
  p2_mrd_events,
  p2_clinical_events, 
  p2_aza_events,
  p2_dli_events
) %>%
  arrange(patno, relative_date)

# Replace "N/D", "N/K" or "." in agvhd_stage and cgvhd_stage with NA
p2_merged$agvhd_stage <- if_else(p2_merged$agvhd_stage == "N/A", NA, p2_merged$agvhd_stage)
p2_merged$cgvhd_stage <- if_else(p2_merged$cgvhd_stage == "N/A", NA, p2_merged$cgvhd_stage)
p2_merged$agvhd_stage <- if_else(p2_merged$agvhd_stage == "N/K", NA, p2_merged$agvhd_stage)
p2_merged$cgvhd_stage <- if_else(p2_merged$cgvhd_stage == "N/K", NA, p2_merged$cgvhd_stage)
p2_merged$agvhd_stage <- if_else(p2_merged$agvhd_stage == ".", NA, p2_merged$agvhd_stage)
p2_merged$cgvhd_stage <- if_else(p2_merged$cgvhd_stage == ".", NA, p2_merged$cgvhd_stage)
p2_merged$agvhd_stage <- if_else(p2_merged$agvhd_stage == "N/D", NA, p2_merged$agvhd_stage)
p2_merged$cgvhd_stage <- if_else(p2_merged$cgvhd_stage == "N/D", NA, p2_merged$cgvhd_stage)

# Remove GVHD rows with no data
p2_merged <- p2_merged %>%
  filter(
    !(
      (source_dataframe == "aGVHD" & is.na(agvhd_stage)) |
        (source_dataframe == "cGVHD" & is.na(cgvhd_stage))
    )
  )

# Define a function to generate MRD + GVHD timeline for a given patient
plot_patient_timeline <- function(data, pat_id) {
  
  df <- data %>% filter(patno == pat_id)
  
  # Determine x-axis range
  x_range <- range(df$relative_date, na.rm = TRUE)
  
  # Prepare GVHD columns
  df2 <- df %>%
    mutate(
      agvhd_plot = if_else(source_dataframe == "aGVHD", agvhd_stage, NA_character_),
      cgvhd_plot = if_else(source_dataframe == "cGVHD", cgvhd_stage, NA_character_),
      source_dataframe = factor(
        source_dataframe,
        levels = c(
          "IS Stop",
          "Azacitidine",
          "DLI",
          "aGVHD",
          "cGVHD"
        )
      )
    ) %>%
    filter(!is.na(source_dataframe))
  
  # MRD plot
  mrd_plot <- ggplot(df %>% filter(source_dataframe == "MRD")) +
    geom_line(aes(x = relative_date, y = log10(level), colour = Mutation), linewidth = 1) +
    geom_point(aes(x = relative_date, y = log10(level)), colour = "darkgrey") +
    theme_minimal() +
    xlab(NULL) +
    ylab("log10(MRD)") +
    scale_colour_brewer(palette = "Set2") +
    scale_x_continuous(limits = x_range) +
    ggtitle(paste0("Patient: ", pat_id)) +
    geom_textvline(
      data = df %>% filter(source_dataframe == "Outcome"),
      aes(xintercept = relative_date, label = Event)
    )
  
  # GVHD / IS events plot
  events_plot <- ggplot(
    df2 %>% filter(!source_dataframe %in% c("MRD", "Outcome")),
    aes(x = relative_date, y = source_dataframe)
  ) +
    
    # aGVHD points
    geom_point(
      data = df2 %>% filter(source_dataframe == "aGVHD" & !source_dataframe %in% c("MRD", "Outcome")),
      aes(colour = agvhd_stage),
      size = 3
    ) +
    scale_colour_manual(
      name = "aGVHD stage",
      values = c(
        "0" = "#FCDCDC",
        "1" = "#FFB5B5",
        "2" = "#FF7878",
        "3" = "#FF3D3D",
        "4" = "#FF2800"
      )
    ) +
    
    ggnewscale::new_scale_colour() +
    
    # cGVHD points
    geom_point(
      data = df2 %>% filter(source_dataframe == "cGVHD" & !source_dataframe %in% c("MRD", "Outcome")),
      aes(colour = cgvhd_stage),
      size = 3
    ) +
    scale_colour_manual(
      name = "cGVHD stage",
      values = c(
        "None" = "#EEEBFF",
        "Mild" = "#B3A3FF",
        "Moderate" = "#704FFF",
        "Severe" = "#2800FF"
      )
    ) +
    
    # other events
    geom_point(
      data = df2 %>% filter(!source_dataframe %in% c("aGVHD", "cGVHD") & !source_dataframe %in% c("MRD", "Outcome")),
      colour = "black",
      size = 3
    ) +
    
    labs(x = "Days after transplantation", y = NULL) +
    theme_minimal() +
    scale_x_continuous(limits = x_range)
  
  # Combine vertically
  combined_plot <- mrd_plot / events_plot + patchwork::plot_layout(heights = c(2, 1))
  
  combined_plot
}

# Plot one example patient
plotA <- plot_patient_timeline(p2_merged, "1004")
plotA

# Export all patients to a multi-page PDF
pdf("NMDS14B_p2_Patient_Timelines.pdf", width = 10, height = 6)

for(p in unique(p2_merged$patno)) {
  print(plot_patient_timeline(p2_merged, p))
}

dev.off()






