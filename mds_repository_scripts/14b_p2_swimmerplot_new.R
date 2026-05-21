# NMDS14B Part 2 Swimmer plot

# Load required packages
library(readxl)
library(swimplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)
library(testthat) # For test-driven development

# Set working directory
setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data")

# Import datasets
general_info_p2 <- read_excel("NMDS14B-Inkl-screen-EoS.xlsx")
mrd_p2 <- read_excel("NMDS14B_MRD.XLSX")
dli_p2 <- read_excel("NMDS14B_dlitrt.xlsx")
aza_p2 <- read_excel("NMDS14B_azacitkurer.xlsx")
immune_p2 <- read_excel("NMDS14B_immunsupptrtm.xlsx")
gvhd_p2 <- read_excel("NMDS14B_gvhddat.xlsx")

### FILTERING

aza_p2 <- aza_p2 %>%
  pivot_longer(cols = starts_with("azacitstdat"),
               names_to = "timepoint",
               values_to = "azacitstartdat") %>%
  select(patno, azacitstartdat) %>%
  filter(!is.na(azacitstartdat)) %>%
  distinct()

gvhd_p2 <- gvhd_p2 %>%
  select(patno, 
         gvhddate, 
         agvhdstage, 
         cgvhdstage, 
         agvhdmaxstage, 
         agvhdmaxdt, 
         cgvhdmaxstage, 
         cgvhdmaxdt) %>%
  distinct()

agvhdmax_p2 <- gvhd_p2 %>%
  select(patno, 
         agvhdstage = agvhdmaxstage, 
         gvhddate = agvhdmaxdt) %>%
  filter(!is.na(agvhdstage)) %>%
  filter(!is.na(gvhddate)) %>%
  distinct()  %>% 
  mutate(gvhd = "Acute GVHD")

cgvhdmax_p2 <- gvhd_p2 %>%
  select(patno, 
         cgvhdstage = cgvhdmaxstage, 
         gvhddate = cgvhdmaxdt) %>%
  filter(!is.na(cgvhdstage)) %>%
  filter(!is.na(gvhddate)) %>%
  distinct()  %>% 
  mutate(gvhd = "Chronic GVHD")

mrd_p2 <- mrd_p2 %>%
  mutate(mrd_category = case_when(
    level < 0.1 ~ "Negative (< 0.1)",
    level < 0.5 ~ "Low (0.1 - 0.5)",
    level < 1 ~ "Intermediate (0.5 - 1)",
    level >= 1 ~ "High (> 1)",
    is.na(level) ~ NA
  ))

general_info_p2 <- general_info_p2 %>%
  filter(!is.na(mdsdiagndat))

immune_p2 <- immune_p2 %>%
  filter(drugstopped == "Yes") %>%
  filter(!drugname %in% c("Azitromycin","Dexametasone","hydrocortisonbutyrat (cutaneous)",
                          "jorvesza (orodispersible)","jorveza (orodispersible)",
                          "Photopheresis","Cellcept","ATG","Budesonid","caps. budesonid",
                          "CellCept","ECP","Entocort po","Medrol","Vedolizumab",
                          "Solu-Medrol","Ruxolitinib","Dexamethasone","IV  methotrexate",
                          "IV Methotrexat","IV Methrotrexate","IV solumedrol","Merdol",
                          "Methotrexate","Methylprednisolon","Methylprednisolone",
                          "Metotrexat","Metotrexat ing","Metotrexat iv","Metotrexate iv",
                          "Metylprednisolon","tbl ruxolitinib","Ruksolitinib",
                          "Prednisolone","Prednisolon po","Prednisolon",
                          "Mykofenolsyra, oral","Mykofenolsyra, iv",
                          "Mycophenolatmofetil (MMF)","Mycophenolate mofetil po",
                          "Mycophenolate mofetil","Mycofenolsyre","Mycofenolat",
                          "MTX","MMF","Metylprednisolon iv","Metotrexat inj")) %>%
  group_by(patno) %>%
  slice_max(order_by = drugdt, n = 1, with_ties = TRUE) %>%
  ungroup()

### MERGING

# Create merged agvhd data frame

agvhd_p2 <- gvhd_p2 %>% 
  select(patno, 
         gvhddate, 
         agvhdstage) %>% 
  mutate(gvhd = "Acute GVHD")

agvhd_p2_merged <- bind_rows(agvhd_p2, agvhdmax_p2) %>% distinct()

agvhd_p2_merged <- agvhd_p2_merged %>% filter(agvhdstage %in% c(3, 4))

# Create merged cgvhd data frame

cgvhd_p2 <- gvhd_p2 %>% 
  select(patno, 
         gvhddate, 
         cgvhdstage) %>% 
  mutate(gvhd = "Chronic GVHD")

cgvhd_p2_merged <- bind_rows(cgvhd_p2, cgvhdmax_p2) %>% distinct()

cgvhd_p2_merged <- cgvhd_p2_merged %>% filter(cgvhdstage == "Severe")

dli_p2 <- dli_p2 %>% select(patno, date = dlidat) %>% mutate(treatment = "dli")
aza_p2 <- aza_p2 %>% select(patno, date = azacitstartdat) %>% mutate(treatment = "aza")
immune_p2 <- immune_p2 %>% select(patno, date = drugdt) %>% mutate(treatment = "immune_stop")

treatment_p2 <- bind_rows(dli_p2, aza_p2, immune_p2)

gvhd_p2_merged <- bind_rows(agvhd_p2_merged, cgvhd_p2_merged) %>% distinct()

mrd_p2 <- mrd_p2 %>%
  left_join(general_info_p2, by = "patno") %>%
  left_join(treatment_p2, by = "patno") %>%
  left_join(gvhd_p2_merged, by = "patno") %>%
  select(patno, MRDdat, mutname, level, mrd_category,
         mdsdiagndat, transpldt, termindat, eosreason,
         relapsedat, deathdat, date, treatment, gvhddate, agvhdstage, cgvhdstage, gvhd)

### DERIVED VARIABLES

mrd_p2 <- mrd_p2 %>%
  mutate(
    rel_mrd_dat = as.numeric(difftime(as.Date(MRDdat), as.Date(transpldt), units = "days")),
    rel_treatment_dat = as.numeric(difftime(as.Date(date), as.Date(transpldt), units = "days")),
    rel_gvhd_dat = as.numeric(difftime(as.Date(gvhddate), as.Date(transpldt), units = "days"))
  ) %>%
  filter(rel_mrd_dat >= 0)

mrd_p2 <- mrd_p2 %>%
  mutate(outcome = case_when(
    eosreason == "2 years post HCT" ~ "Remission",
    eosreason == "Consent withdrawal" ~ "Other reason",
    eosreason == "Death" ~ "Nonrelapse mortality",
    eosreason == "Full hematological relapse" ~ "Relapse",
    TRUE ~ "Remission"
  ))

mrd_p2 <- mrd_p2 %>%
  group_by(patno) %>%
  mutate(outcome = ifelse(any(level > 10 & rel_mrd_dat > 0, na.rm = TRUE),
                          "Relapse", outcome)) %>%
  ungroup()

mrd_p2 <- mrd_p2 %>% 
  group_by(patno) %>%
  mutate(end_event = as.numeric(max(rel_mrd_dat))) %>% 
  ungroup()

# Filter out treatments after end_event
mrd_p2 <- mrd_p2 %>%
  filter(!rel_treatment_dat > end_event)

mrd_p2_filtered <- distinct(mrd_p2)

### OPTIONAL FILTERING STEP

#mrd_p2_filtered <- mrd_p2_filtered %>% filter(outcome == "Death")
#mrd_p2_filtered <- mrd_p2_filtered %>% filter(outcome == "Relapse")
#mrd_p2_filtered <- mrd_p2_filtered %>%
#  group_by(patno) %>%
#  filter(any(mrd_category %in% c("High (> 1)", "Intermediate (0.5 - 1)", "Low (0.1 - 0.5)") & rel_mrd_dat > 0)) %>%
#  ungroup()

# Make swimmer plot

swimmer_plot <- ggplot() +
  
  geom_col(
    data = mrd_p2_filtered %>%
      select(patno, end_event) %>%
      distinct(),
    aes(
      x = reorder(patno, end_event),
      y = end_event
    ), 
    colour = "black",
    fill = "white"
  ) +
  
  # Relapse annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, end_event, outcome) %>%
              filter(outcome == "Relapse") %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = end_event, 
              label = "R"),
            hjust = -0.5) +
  
  # Nonrelapse mortality annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, end_event, outcome) %>%
              filter(outcome == "Nonrelapse mortality") %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = end_event, 
              label = "\u2020"),
            hjust = -0.5) +
  
  # Azacitidine annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, rel_treatment_dat, treatment, end_event) %>%
              filter(treatment == "aza") %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = rel_treatment_dat, 
              label = "A")) +
  
  # DLI annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, rel_treatment_dat, treatment, end_event) %>%
              filter(treatment == "dli") %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = rel_treatment_dat, 
              label = "D")) +
  
  # Stop of immune suppression annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, rel_treatment_dat, treatment, end_event) %>%
              filter(treatment == "immune_stop") %>%
              filter(rel_treatment_dat > 0) %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = rel_treatment_dat, 
              label = "IS")) +
  
  # Chronic GVHD annotation
  
  geom_text(data = mrd_p2_filtered %>%
              select(patno, rel_gvhd_dat, gvhd, cgvhdstage, end_event) %>%
              filter(gvhd == "Chronic GVHD") %>%
              filter(rel_gvhd_dat <= end_event) %>%
              distinct(),
            aes(
              x = reorder(patno, end_event), 
              y = rel_gvhd_dat, 
              label = "\u00D7")) +
  
  # Acute GVHD annotation
  
  geom_point(data = mrd_p2_filtered %>%
               select(patno, rel_gvhd_dat, gvhd, agvhdstage, end_event) %>%
               filter(gvhd == "Acute GVHD") %>%
               filter(rel_gvhd_dat <= end_event) %>%
               distinct(),
             aes(
               x = reorder(patno, end_event), 
               y = rel_gvhd_dat, 
               color = agvhdstage),
             shape = 17) +
  
  # Custom acute GVHD colors
  
  scale_color_manual(values = c(
    "3" = "#27D6F5",
    "4" = "#5B27F5"
  )) +
  
  # MRD annotation
  
  geom_point(data = mrd_p2_filtered %>%
               select(patno, rel_mrd_dat, mrd_category, end_event) %>%
               filter(!is.na(mrd_category)) %>%
               distinct(),
             aes(
               x = reorder(patno, end_event), 
               y = rel_mrd_dat, 
               fill = mrd_category),
             shape = 22) +
  
  # Custom MRD colors
  
  scale_fill_manual(values = c(
    "Negative (< 0.1)" = "#FFFFFF",
    "Low (0.1 - 0.5)" = "#FFC107",
    "Intermediate (0.5 - 1)" = "#FF9800",
    "High (> 1)" = "#F44336"
  )) +
  
  labs(x = "Patient ID",
       y = "Days from transplantation",
       title = "NMDS14B Part 2") +
  
  coord_flip() +
  
  theme_classic()

swimmer_plot

# Export patients to pdf
pdf("NMDS14B_p2_Swimmerplot.pdf", 
    width = 10, 
    height = 20)

swimmer_plot

dev.off()
