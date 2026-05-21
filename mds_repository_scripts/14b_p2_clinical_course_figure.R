# The script creates a figure explaining the clinical course after transplantation, 
# based on patient data from the NMDS14B Part 2 study. 

# Load required packages

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(patchwork)
library(geomtextpath)
library(ggnewscale)
library(scales)
library(cowplot)
library(stringr)

# Set working directory
setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data")

# Import datasets
p2_clinical <- read_excel("NMDS14B-Inkl-screen-EoS.xlsx")
p2_mrd <- read_excel("NMDS14B_MRD.XLSX")
p2_gvhd <- read_excel("NMDS14B_gvhd.xlsx")
p2_aza <- read_excel("NMDS14B_azacitkurer.xlsx")
p2_is <- read_excel("NMDS14B_immunsupptrtm.xlsx")
p2_dli <- read_excel("NMDS14B_dlitrt.xlsx")
p2_ngs <- read_excel("NGS lista NMDSG14B2.xlsx")

# Transpose and filter the p2_aza file
p2_aza <- p2_aza %>% pivot_longer(cols = starts_with("azacitstdat"), names_to = "timepoint", values_to = "azacitstartdat") %>% select(patno, azacitstartdat) %>% filter(!is.na(azacitstartdat)) %>% unique()

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
p2_gvhd$rel_max_c_gvhd_dat <- as.Date(p2_gvhd$cgvhdmaxdt) - as.Date(p2_gvhd$transpldt)
p2_gvhd$rel_max_a_gvhd_dat <- as.Date(p2_gvhd$agvhdmaxdt) - as.Date(p2_gvhd$transpldt)
p2_is$is_stop_dat <- as.Date(p2_is$drugdt) - as.Date(p2_is$transpldt)
p2_aza$rel_aza_dat <- as.Date(p2_aza$azacitstartdat) - as.Date(p2_aza$transpldt)
p2_dli$rel_dli_dat <- as.Date(p2_dli$dlidat) - as.Date(p2_dli$transpldt)

# Add row with mutname to p2_ngs
p2_ngs$mutname <- paste0(p2_ngs$Gen, "_", p2_ngs$`cDNA förändring`)

# Add column with list of genes for eacn patno in p2_ngs
# 1) Create a table of unique gene lists per patient
gene_lists <- p2_ngs %>%
  filter(!is.na(Studienummer)) %>%
  group_by(Studienummer) %>%
  summarise(mutlist = paste(unique(Gen), collapse = ", "), .groups = "drop")

# 2) Join it back to the original data
p2_ngs <- p2_ngs %>%
  left_join(gene_lists, by = "Studienummer")

# Change name of Studienummer column in p2_ngs from Studienummer to patno
colnames(p2_ngs)[2] <- "patno"

# Change p2_ngs$patno to double
p2_ngs$patno <- as.double(p2_ngs$patno)

# Change name of mutname column in p2_mrd from mutname to Mutation
colnames(p2_mrd)[6] <- "Mutation"

# Calculate IPSS-M title from score
p2_clinical <- p2_clinical %>% mutate(ipssm_title = case_when(ipssm < -1.5 ~ "Very Low",
                                               ipssm >= -1.5 & ipssm < -0.5 ~ "Low",
                                               ipssm >= -0.5 & ipssm < 0 ~ "Moderate Low",
                                               ipssm >= 0 & ipssm < 0.5 ~ "Moderate High",
                                               ipssm >= 0.5 & ipssm < 1.5 ~ "High",
                                               ipssm >= 1.5 ~ "Very High"))

# Helper function to clean GVHD data
clean_gvhd <- function(df, rel_max_col, stage_col, max_stage_col, type) {
  df %>%
    select(patno, !!sym(rel_max_col), !!sym(stage_col), !!sym(max_stage_col)) %>%
    mutate(
      merged_stage = if_else(
        !is.na(!!sym(max_stage_col)) & !!sym(max_stage_col) >= !!sym(stage_col),
        !!sym(max_stage_col),
        !!sym(stage_col)
      )
    ) %>%
    pivot_longer(
      cols = c(!!sym(rel_max_col)),
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
      Event = NA_character_,
      drugname = NA_character_
    )
}

# Clean acute and chronic GVHD
p2_a_gvhd <- clean_gvhd(p2_gvhd, "rel_max_a_gvhd_dat", "agvhdstage", "agvhdmaxstage", "a")
p2_c_gvhd <- clean_gvhd(p2_gvhd, "rel_max_c_gvhd_dat", "cgvhdstage", "cgvhdmaxstage", "c")

# Filter IS data
p2_is <- p2_is %>%
  filter(str_detect(drugname, regex("Ci|Cy|Sa|Ta|Si", ignore_case = TRUE)))
p2_is <- p2_is %>%
  filter(!drugname %in% c("Azitromycin", 
                          "Dexametasone", 
                          "hydrocortisonbutyrat (cutaneous)", 
                          "jorvesza (orodispersible)",
                          "jorveza (orodispersible)",
                          "Photopheresis"))

# Clean IS
p2_is_events <- p2_is %>%
  filter(drugstopped == "Yes") %>%
  group_by(patno, drugname) %>%
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
    Event = NA_character_,
    drugname = drugname
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
    Event = NA_character_,
    drugname = NA_character_
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
    Event,
    drugname = NA_character_
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
    Event = NA_character_,
    drugname = NA_character_
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
    Event = NA_character_,
    drugname = NA_character_
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

# Add column to p2_merged with mdsdiagnosis, ipssm, deathcause and karyotyp, from p2_clinical, according to patno
p2_merged <- p2_merged %>%
  left_join(
    p2_clinical %>%
      select(patno, mdsdiagnosis, ipssm_title, karyotyp, deathcause),
    by = "patno"
  )

# Add column to p2_merged with mutlist from p2_ngs, according to patno
p2_merged <- p2_merged %>%
  left_join(
    p2_ngs %>%
      select(patno, mutlist),
    by = "patno"
  )

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

# To achieve a log10 y axis scale, convert the 0 values in level to 0.08
p2_merged <- p2_merged %>% mutate(level_no0s = ifelse(level == 0, 0.08, level))

# Create dummy GVHD legends

agvhd_legend <- data.frame(stage = factor(c("0","1","2","3","4"), levels = c("0","1","2","3","4")), x=1, y=1)
cgvhd_legend <- data.frame(stage = factor(c("None","Mild","Moderate","Severe"), levels = c("None","Mild","Moderate","Severe")), x=1, y=1)

agvhd_legend_grob <- get_legend(
  ggplot(agvhd_legend, aes(x=x, y=y, colour=stage)) +
    geom_point(size=3) +
    scale_colour_manual(name="aGVHD stage", values=c("0"="#EBEBEB","1"="#EDC0C0","2"="#FF7878","3"="#D42626","4"="#800000")) +
    theme_void() +
    theme(legend.position="right")
)

cgvhd_legend_grob <- get_legend(
  ggplot(cgvhd_legend, aes(x=x, y=y, colour=stage)) +
    geom_point(size=3) +
    scale_colour_manual(name="cGVHD stage", values=c("None"="#EBEBEB","Mild"="#AA88BB","Moderate"="#622BD6","Severe"="#290088")) +
    theme_void() +
    theme(legend.position="right")
)

# Define a function to generate MRD + GVHD timeline for a given patient

plot_patient_timeline <- function(data, pat_id) {

  df <- p2_merged %>% 
    filter(patno == pat_id)
  
  df2 <- p2_merged %>% 
    filter(patno == pat_id) %>%
    filter(!source_dataframe %in% c("MRD", "Outcome")) %>%
    distinct(source_dataframe, relative_date, agvhd_stage, cgvhd_stage, drugname, .keep_all = TRUE) %>%
    mutate(
      source_dataframe = factor(
        source_dataframe,
        levels = c(
          "aGVHD",
          "cGVHD",
          "IS Stop",
          "Azacitidine",
          "DLI"
        )
      )
    )
  
  # Determine x-axis range
  x_range <- range(as.numeric(df$relative_date), na.rm = TRUE)
  
  # ----------------------------
  
  # CLEAN MRD DATA (fix core issue)
  
  # ----------------------------
  
  mrd_df <- df %>%
    filter(
      source_dataframe == "MRD",
      !is.na(Mutation),
      Mutation != "(Only one mutation)",
      Mutation != "(only one mutation)",
      !is.na(relative_date),
      !is.na(level_no0s),
      level_no0s > 0   # required for log scale
    ) %>%
    group_by(Mutation) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Dynamically determine upper limit of y axis
  mrd_values <- df$level_no0s[df$source_dataframe == "MRD"]
  upper_limit <- 10  # default
  if (length(mrd_values) > 0 && any(!is.na(mrd_values))) {
    max_mrd <- max(mrd_values, na.rm = TRUE)
    if (is.finite(max_mrd) && max_mrd > 10) {
      upper_limit <- NA  # allow full expansion
    }
  }

  # ----------------------------
  # MRD plot (top)
  # ----------------------------
  mrd_plot <- ggplot() +
    geom_line(data = df %>% filter(source_dataframe == "MRD", !is.na(Mutation), Mutation != "(Only one mutation)", Mutation != "(only one mutation)") %>% droplevels(), aes(x=relative_date, y=level_no0s, colour=Mutation)) +
    geom_point(data = df %>% filter(source_dataframe == "MRD", !is.na(Mutation), Mutation != "(Only one mutation)", Mutation != "(only one mutation)") %>% droplevels(), aes(x=relative_date, y=level_no0s, colour=Mutation)) +
    theme_minimal() +
    xlab(NULL) + 
    ylab(NULL) +
    scale_colour_brewer(palette="Set2", na.translate = FALSE) +
    scale_x_continuous(limits = x_range) +
    scale_y_log10(limits = c(NA, upper_limit), labels = label_number()) +
    labs(title = paste0("Patient: ", pat_id),
         subtitle = paste0("Diagnosis: ", df$mdsdiagnosis, "\nIPSS-M: ", df$ipssm_title, "\nKaryotype: ", df$karyotyp, "\nNGS: ", df$mutlist)) +
    geom_textvline(
      data = df%>% filter(source_dataframe == "Outcome" & Event == "Relapse"),
      aes(xintercept = relative_date, label = Event)
    ) +
    geom_textvline(
      data = df%>% filter(source_dataframe == "Outcome" & Event == "Death"),
      aes(xintercept = relative_date, label = paste0(Event, ", ", deathcause))
    ) +
    theme(legend.position="right",
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 9)) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 0.08,  # lower visible bound
      ymax = 0.1,
      alpha = 0.25,
      fill = "grey50"
    )
  
  # safer horizontal line
  if (nrow(mrd_df) > 0) {
    mrd_plot <- mrd_plot +
      geom_texthline(
        yintercept = 0.1,
        label = "MRD Threshold",
        vjust = -0.2, 
        hjust = 1,
        colour = "darkgrey",
        linetype = "dashed"
      )
  }
  
  
  # Extract mrd legend
  mrd_legend <- get_legend(mrd_plot) 
  
  # Remove mrd legend from mrd_plot
  mrd_plot_clean <- mrd_plot + theme(legend.position="none")
  
  # ----------------------------
  # GVHD / IS events plot (bottom)
  # ----------------------------
  
  events_plot <- ggplot(df2 %>% filter(!source_dataframe %in% c("MRD","Outcome")),
                        aes(x = relative_date, y = source_dataframe)) +
    
    # aGVHD
    geom_point(
      data = df2 %>% filter(source_dataframe == "aGVHD" & !is.na(agvhd_stage)),
      aes(colour = agvhd_stage),
      size = 3
    ) +
    scale_colour_manual(
      values = c("0"="#EBEBEB","1"="#EDC0C0","2"="#FF7878","3"="#D42626","4"="#800000"),
      guide = "none"
    ) +
    
    ggnewscale::new_scale_colour() +
    
    # cGVHD
    geom_point(
      data = df2 %>% filter(source_dataframe == "cGVHD" & !is.na(cgvhd_stage)),
      aes(colour = cgvhd_stage),
      size = 3
    ) +
    scale_colour_manual(
      values = c("None"="#EBEBEB","Mild"="#AA88BB","Moderate"="#622BD6","Severe"="#290088"),
      guide = "none"
    ) +
    
    # other events first (IS stop, Azacitidine, DLI)
    geom_point(
      data = df2 %>% filter(source_dataframe %in% c("IS Stop","Azacitidine","DLI")),
      colour = "black",
      size = 3
    ) +
    
    # Add text for IS drug name
    geom_text(data = df2 %>% filter(source_dataframe == "IS Stop"), 
              aes(label = drugname),
              hjust = -0.1, 
              vjust = 0.5,
              size = 3) +
    
    labs(x = "Days after transplantation", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_continuous(limits = x_range)
  
  # ----------------------------
  # Combine MRD + events vertically
  # ----------------------------
  combined_plots <- plot_grid(
    mrd_plot_clean,
    events_plot,
    ncol=1,
    rel_heights=c(2,1),
    align="v",
    axis="tblr"
  )
  
  # ----------------------------
  # Combine all legends vertically
  # ----------------------------
  combined_legends <- plot_grid(mrd_legend, agvhd_legend_grob, cgvhd_legend_grob, ncol=1, align="v")
  
  # ----------------------------
  # Final combined plot
  # ----------------------------
  final_plot <- plot_grid(combined_plots, combined_legends, ncol=2, rel_widths=c(4,1), align="v")
  return(final_plot)
}

# Plot one example patient
# plot_patient_timeline(p2_merged, "1002")

# Export patients to pdf
pdf("NMDS14B_p2_Patient_Timelines.pdf", width = 10, height = 6)

for(p in unique(p2_merged$patno)) {
  print(plot_patient_timeline(p2_merged, p))
}

dev.off()
  






