# 14b_p2_fish_plot

# Creates a fish plot based on the NMDS14B Part 2 NGS Data

# Based on: https://bioconductor.org/packages/release/bioc/html/timescape.html

# Load required packages

library(timescape)
library(dplyr)
library(readxl)

# Set working directory
setwd("/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data")

# Import datasets
p2_ngs <- read_excel("NGS lista NMDSG14B2.xlsx")

# Exclude patients U24 and U26, with weird `Provtagning (Diagnos/Inklusion/Relaps/Uppföljning)` values
p2_ngs <- p2_ngs %>% filter(!ScreeningID %in% c("U24", "U26"))

# Change p2_ngs$`VAF (%)` to character
p2_ngs$`VAF (%)` = as.character(p2_ngs$`VAF (%)`)

# Replace strings containing "<" in p2_ngs$`VAF (%)` with 0
p2_ngs$`VAF (%)` <- ifelse(grepl("<", p2_ngs$`VAF (%)`), 0, p2_ngs$`VAF (%)`)

# Replace strings containing "%" in p2_ngs$`VAF (%)` with 0
p2_ngs$`VAF (%)` <- ifelse(grepl("%", p2_ngs$`VAF (%)`), 0, p2_ngs$`VAF (%)`)

# Replace strings containing "-" in p2_ngs$`VAF (%)` with NA
p2_ngs$`VAF (%)` <- if_else(p2_ngs$`VAF (%)` == "-", NA, p2_ngs$`VAF (%)`)

# Change p2_ngs$`VAF (%)` to numeric
p2_ngs$`VAF (%)` = as.numeric(p2_ngs$`VAF (%)`)

# Add column with the sum of each VAF, per sampling timepoint
p2_ngs <- p2_ngs %>% group_by(Studienummer, `Provtagning (Diagnos/Inklusion/Relaps/Uppföljning)`) %>%
  mutate(sum_vaf = sum(`VAF (%)`)) %>% ungroup()

# Add column with the clonal prevalence (out of 1) of each VAF, per patient, sampling timepoint and mutation
p2_ngs <- p2_ngs %>% group_by(Studienummer, 
                              `Provtagning (Diagnos/Inklusion/Relaps/Uppföljning)`, 
                              Gen, 
                              Transkriptnummer, 
                              `cDNA förändring`, 
                              Proteinförändring) %>%
  mutate(clonal_prev = `VAF (%)`/sum_vaf) %>% ungroup()

# Add column with merged "Gen" and "`cDNA förändring`" names
p2_ngs <- p2_ngs %>% 
  mutate(clone_id = paste0(Gen, "_", `Proteinförändring`))

# Change name of `Provtagning (Diagnos/Inklusion/Relaps/Uppföljning)` column to timepoint
colnames(p2_ngs)[4] <- "timepoint"

# Focus only on relapse cases
p2_ngs <- p2_ngs %>%
  group_by(Studienummer) %>%
  filter(any(grepl("Relaps", timepoint))) %>% 
  ungroup()

# Change names of Diagnos, Relaps and Inklusion
p2_ngs$timepoint[p2_ngs$timepoint == "Diagnos"] <- "A. Diagnosis"
p2_ngs$timepoint[p2_ngs$timepoint == "Inklusion"] <- "B. Inclusion"
p2_ngs$timepoint[p2_ngs$timepoint == "Relaps"] <- "C. Relapse"

# Change name of `VAF (%)` column to VAF
colnames(p2_ngs)[9] <- "VAF"

# Create basic tree_edges data frame for each patient, where the largest
# initial clone is assumed to be the founder mutation. 

phylogeny_generator <- function(df, patno) {
  df_filtered <- df %>% dplyr::filter(Studienummer == patno)
  df_diagnosis <- df_filtered %>% dplyr::filter(timepoint == "A. Diagnosis")
  
  # valid founder mutations
  valid_prev <- df_diagnosis %>% dplyr::filter(!is.na(clonal_prev))
  if (nrow(valid_prev) == 0) return(data.frame(source=character(), target=character()))
  
  founder_mutation <- valid_prev %>%
    dplyr::filter(clonal_prev == max(clonal_prev)) %>%
    dplyr::pull(clone_id) %>%
    sample(1)
  
  other_mutations <- df_filtered %>%
    dplyr::filter(!clone_id %in% founder_mutation) %>%
    dplyr::distinct(clone_id) %>%
    dplyr::pull(clone_id)
  
  if (length(other_mutations) == 0) {
    # no edges for single clone
    return(data.frame(source=character(), target=character()))
  }
  
  data.frame(
    source = rep(founder_mutation, length(other_mutations)),
    target = other_mutations,
    stringsAsFactors = FALSE
  )
}

pdf("NMDS14B_p2_Fishplot.pdf", width = 10, height = 6)

for (p in unique(p2_ngs$Studienummer)) {
  tree_edges <- phylogeny_generator(p2_ngs, p)
  
  # skip patients with no edges
  if (nrow(tree_edges) == 0) next
  
  a <- timescape(clonal_prev = p2_ngs %>% filter(Studienummer == p) %>% arrange(timepoint), 
                 tree_edges = tree_edges, 
                 mutations = "NA", 
                 clone_colours = "NA",
                 xaxis_title = "Time Point", 
                 yaxis_title = paste0("Clonal Prevalence, Patient: ", p),
                 phylogeny_title = "Clonal Phylogeny", 
                 alpha = 50,
                 genotype_position = "stack", 
                 perturbations = "NA", 
                 sort = FALSE,
                 show_warnings = TRUE, 
                 width = 900, 
                 height = NULL)
  
  print(a)
}

dev.off()



