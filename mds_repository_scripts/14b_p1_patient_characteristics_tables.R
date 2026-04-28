# Patient characteristics table, NMDSG14B Part 1

# Load libraries
library(readxl)
library(swimplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(survival)
library(tableone)
library(stringr)
library(gtsummary)
library(fuzzyjoin)
library(lubridate)
library(gt)
library(survminer)
library(ggpubr)

# Set working directory
setwd("/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p1_data")

# 1. Import datasets
general_info_p1 <- read_excel("general_information_2024-01-17_march_2026.xlsx", na = "NA")
mrd_results <- read_excel("mrd_results_2026.xlsx", na = "NA")
mrd_background <- read_excel("Assay list for supplement table.xlsx")
ngs_p1 <- read_excel("NGS_Uppsala_2024-01-17.xlsx")
sct_parameters_p1 <- read_excel("SCT_parameters_2026_02_20.xlsx")
general_info_p1_joel <- read_excel("mds_data_20240930.xlsx") # General info document from Joel

# Add mutation_suffix column to mrd_results
mrd_results <- mrd_results %>%
  # Split into prefix and suffix first
  mutate(
    mut_prefix = str_extract(Mutation, "^[^_\\- ]+"),
    mut_suffix = str_sub(Mutation, str_locate(Mutation, "(_|-| )")[,1] + 1))

# Add the mut_suffix and `background noise` columns from mrd_background to mrd_results, and match by the mut_suffix columns
mrd_results <- mrd_results %>%
  fuzzy_left_join(
    mrd_background %>% select(mut_suffix, `background noise`),
    by = c("mut_suffix" = "mut_suffix"),
    match_fun = ~ str_detect(.x, fixed(.y))
  ) %>%
  rename(mut_suffix_bg = mut_suffix.y) %>%
  select(-mut_suffix_bg)

# Merge general_info_p1 and mrd_results by `Study number`
mrd_results <- mrd_results %>% left_join(general_info_p1, by = "Study number")

# Rename study_number column in sct_parameters_p1 to `Study number`
sct_parameters_p1$`Study number` <- sct_parameters_p1$study_number

# Merge sct_parameters_p1 and mrd_results by `Study number`
mrd_results <- mrd_results %>% left_join(sct_parameters_p1, by = "Study number")

# Replace c_gvhd Y with TRUE and N with FALSE
mrd_results$c_gvhd <- ifelse(mrd_results$c_gvhd == "Y", TRUE, FALSE)

# Replace NA in `Reason for termination` column with "Remission"
mrd_results$`Reason for termination` <- ifelse(is.na(mrd_results$`Reason for termination`), "Remission", mrd_results$`Reason for termination`)

# Replace "Two years passed" in `Reason for termination` column with "Remission"
mrd_results$`Reason for termination` <- ifelse(mrd_results$`Reason for termination` == "Two years passed", "Remission", mrd_results$`Reason for termination`)

# Focus ONLY on patients "Included_in_MRD_analysis"
mrd_results <- mrd_results %>% filter(`Included in MRD analysis` == TRUE)

# Create lists of TP53, KDM6A, NRAS, KRAS, RUNX1 and DNMT3A positive patients respectively, based on NGS >3% VAF
tp53_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "TP53" & ngs_p1$allele_fraction_percent > 3 & !(ngs_p1$sample_type %in% c("post SCT", 
                                                                                                                                         "Post SCT", 
                                                                                                                                         "Post-SCT",
                                                                                                                                         "Relapse"))])

tp53_cases <- tp53_cases[!is.na(tp53_cases)]
KDM6A_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "KDM6A" & ngs_p1$allele_fraction_percent > 3])
KDM6A_cases <- KDM6A_cases[!is.na(KDM6A_cases)]
NRAS_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "NRAS" & ngs_p1$allele_fraction_percent > 3])
NRAS_cases <- NRAS_cases[!is.na(NRAS_cases)]
KRAS_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "KRAS" & ngs_p1$allele_fraction_percent > 3])
KRAS_cases <- KRAS_cases[!is.na(KRAS_cases)]
RUNX1_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "RUNX1" & ngs_p1$allele_fraction_percent > 3])
RUNX1_cases <- RUNX1_cases[!is.na(RUNX1_cases)]
DNMT3A_cases <- unique(ngs_p1$study_number[ngs_p1$gene_symbol == "DNMT3A" & ngs_p1$allele_fraction_percent > 3])
DNMT3A_cases <- DNMT3A_cases[!is.na(DNMT3A_cases)]

# Add boolean column for TP53, KDM6A, NRAS, KRAS, RUNX1 and DNMT3A mutations
mrd_results$TP53 <- ifelse(mrd_results$`Study number` %in% tp53_cases, TRUE, FALSE)
mrd_results$KDM6A <- ifelse(mrd_results$`Study number` %in% KDM6A_cases, TRUE, FALSE)
mrd_results$NRAS <- ifelse(mrd_results$`Study number` %in% NRAS_cases, TRUE, FALSE)
mrd_results$KRAS <- ifelse(mrd_results$`Study number` %in% KRAS_cases, TRUE, FALSE)
mrd_results$RUNX1 <- ifelse(mrd_results$`Study number` %in% RUNX1_cases, TRUE, FALSE)
mrd_results$DNMT3A <- ifelse(mrd_results$`Study number` %in% DNMT3A_cases, TRUE, FALSE)

# Calculate net_mrd 
mrd_results$net_mrd <- as.numeric(mrd_results$Level) - as.numeric(mrd_results$`background noise`)

# Create net_mrd_boolean column
mrd_results$net_mrd_boolean <- mrd_results$net_mrd > 0

# Calculate MRD relative to transplantation
mrd_results$relative_mrd_date <- as.Date(mrd_results$Date) - as.Date(mrd_results$`Date of transplantation`)

# Create any_mrd_above_background_0_40 column
mrd_results$any_mrd_above_background_0_40 <-
  mrd_results$net_mrd_boolean &
  mrd_results$relative_mrd_date > 0 &
  mrd_results$relative_mrd_date < 41

# Create any_mrd_above_background_100 column
mrd_results$any_mrd_above_background_100 <-
  mrd_results$net_mrd_boolean &
  mrd_results$relative_mrd_date > 100

# Group data by mutation and patno, and calculate mean MRD level the first 40 days after transplantation
mrd_results <- mrd_results %>%
  group_by(`Study number`, Mutation) %>%
  mutate(
    mean_level_first_40_days = mean(
      Level[relative_mrd_date > 0 & relative_mrd_date < 40],
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Group data by mutation and patno, and calculate mean MRD level 100+ days after transplantation
mrd_results <- mrd_results %>%
  group_by(`Study number`, Mutation) %>%
  mutate(
    mean_level_after_100_days = mean(
      Level[relative_mrd_date > 100],
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Calculate background_no_0s that won't become Infinite when used as a divisor
mrd_results <- mrd_results %>%
  mutate(background_no_0s = ifelse(as.numeric(`background noise`) == 0, 0.001, as.numeric(`background noise`)))

# Calculate mrd_ratio for each recorded level
mrd_results <- mrd_results %>%
  mutate(mrd_ratio = as.numeric(Level) / as.numeric(background_no_0s))

# Group data by mutation and patno, and calculate mean MRD level 100+ days after transplantation
mrd_results <- mrd_results %>%
  group_by(`Study number`, Mutation) %>%
  mutate(
    mean_level_after_100_days = mean(
      Level[relative_mrd_date > 100],
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Keep only any_mrd_above_background_0_40 TRUE row in cases with both TRUE and FALSE
mrd_results <- mrd_results %>%
  group_by(`Study number`) %>%
  filter(
    any(any_mrd_above_background_0_40 == TRUE) &
      any_mrd_above_background_0_40 == TRUE |
      !any(any_mrd_above_background_0_40 == TRUE)
  ) %>%
  ungroup()

# Keep only any_mrd_above_background_100 TRUE row in cases with both TRUE and FALSE
mrd_results <- mrd_results %>%
  group_by(`Study number`) %>%
  filter(
    any(any_mrd_above_background_100 == TRUE) &
      any_mrd_above_background_100 == TRUE |
      !any(any_mrd_above_background_100 == TRUE)
  ) %>%
  ungroup()

# Replace TRUE or FALSE with NA in any_mrd_above_background_0_40, if the corresponding row in mean_level_first_40 days is Nan
mrd_results$any_mrd_above_background_0_40 <- ifelse(mrd_results$mean_level_first_40_days == "NaN", NA, mrd_results$any_mrd_above_background_0_40)

# Replace TRUE or FALSE with NA in any_mrd_above_background_100, if the corresponding row in mean_level_after_100_days is Nan
mrd_results$any_mrd_above_background_100 <- ifelse(mrd_results$mean_level_after_100_days == "NaN", NA, mrd_results$any_mrd_above_background_100)

# Replace Yes with TRUE and NA with FALSE in the ICT, HMA and No treatment columns
mrd_results$ICT <- ifelse(is.na(mrd_results$ICT), FALSE, TRUE)
mrd_results$HMA <- ifelse(is.na(mrd_results$HMA), FALSE, TRUE)
mrd_results$`No treatment` <- ifelse(is.na(mrd_results$`No treatment`), FALSE, TRUE)

# Replace "Very High" with "Very high" in the `IPSS-R diagnosis` column
mrd_results$`IPSS-R diagnosis` <- ifelse(mrd_results$`IPSS-R diagnosis` == "Very High", "Very high", mrd_results$`IPSS-R diagnosis`)

# Replace N/A with NA in the `IPSS-R diagnosis` column
mrd_results$`IPSS-R diagnosis` <- ifelse(mrd_results$`IPSS-R diagnosis` == "N/A", NA, mrd_results$`IPSS-R diagnosis`)

# Replace N/A with NA in the `IPSS-R cytogenetic risk group` column
mrd_results$`IPSS-R cytogenetic risk group` <- ifelse(mrd_results$`IPSS-R cytogenetic risk group` == "N/A", NA, mrd_results$`IPSS-R cytogenetic risk group`)

# Make marrow blasts numeric
mrd_results$`Marrow blasts` <- as.numeric(mrd_results$`Marrow blasts`)

# Make age_donor numeric
mrd_results$age_donor <- as.numeric(mrd_results$age_donor)

# Rename study_nr to `Study number` in general_info_p1_joel
colnames(general_info_p1_joel)[1] <- "Study number"

# Add dur_tx_relapsedeath_years, cens_RFS, Age columns from general_info_p1_joel to general_info_p1, by `Study number`
mrd_results <- mrd_results %>% left_join(general_info_p1_joel %>% select(`Study number`, cens_RFS, dur_tx_relapsedeath_years, Age), by = "Study number")

# Table 1 ####

# verluis JCO 2024 

# Create patient characteristics graph for all patients, by outcome

all_tab_outcome <- mrd_results |>
  distinct(`Study number`, .keep_all = TRUE) |>
  filter(`Reason for termination` != "Patients wish") |>
  filter(`Reason for termination` != "Other (please use comment)") |>
  filter(!is.na(`Reason for termination`)) |>
  tbl_summary(
    by = `Reason for termination`,
    include = c(Age,
                Gender,
                `IPSS-R diagnosis`,
                `IPSS-R cytogenetic risk group`,
                `Marrow blasts`,
                ICT, 
                HMA,
                `No treatment`,
                c_gvhd,
                age_donor,
                TP53,
                mean_level_first_40_days,
                any_mrd_above_background_0_40,
                mean_level_after_100_days,
                any_mrd_above_background_100),
    type = list(all_continuous() ~ "categorical",
                `Marrow blasts` ~ "continuous",
                age_donor ~ "continuous",
                mean_level_first_40_days ~ "continuous",
                mean_level_after_100_days ~ "continuous",
                Age ~ "continuous"),
    label = list(mean_level_first_40_days ~ "Mean MRD first 40 days",
                 any_mrd_above_background_0_40 ~ "Any MRD above background first 40 days after dx",
                 mean_level_after_100_days ~ "Mean MRD after 100 days",
                 any_mrd_above_background_100 ~ "Any MRD above background more than 100 days after dx",
                 c_gvhd ~ "Chronic GVHD",
                 age_donor ~ "Donor age",
                 Age ~ "Age")
  ) |>
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Outcome**") |>
  add_p()

gtsave(as_gt(all_tab_outcome), filename = "all_tab_outcome.html", inline_css = TRUE)

# Create patient characteristics graph for all patients, by TP53 mutation

all_tab_tp53 <- mrd_results |>
  distinct(`Study number`, .keep_all = TRUE) |>
  filter(`Reason for termination` != "Patients wish") |>
  filter(`Reason for termination` != "Other (please use comment)") |>
  filter(!is.na(`Reason for termination`)) |>
  tbl_summary(
    by = TP53,
    include = c(`Reason for termination`,
                Age,
                Gender,
                `IPSS-R diagnosis`,
                `IPSS-R cytogenetic risk group`,
                `Therapy related`,
                `Marrow blasts`,
                ICT, 
                HMA,
                `No treatment`,
                c_gvhd,
                age_donor,
                RUNX1,
                DNMT3A,
                mean_level_first_40_days,
                any_mrd_above_background_0_40,
                mean_level_after_100_days,
                any_mrd_above_background_100),
    type = list(all_continuous() ~ "categorical",
                `Marrow blasts` ~ "continuous",
                age_donor ~ "continuous",
                mean_level_first_40_days ~ "continuous",
                mean_level_after_100_days ~ "continuous",
                Age ~ "continuous"),
    label = list(mean_level_first_40_days ~ "Mean MRD first 40 days",
                 any_mrd_above_background_0_40 ~ "Any MRD above background first 40 days",
                 mean_level_after_100_days ~ "Mean MRD after 100 days",
                 any_mrd_above_background_100 ~ "Any MRD above background after 100 days",
                 c_gvhd ~ "Chronic GVHD",
                 age_donor ~ "Donor age",
                 `Reason for termination` ~ "Outcome")
  ) |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**TP53 Mutation**") |>
  add_p()

gtsave(as_gt(all_tab_tp53), filename = "all_tab_tp53.html", inline_css = TRUE)

# Create patient characteristics graph for TP53 patients only, by outcome

tp53_tab_outcome <- mrd_results |>
  distinct(`Study number`, .keep_all = TRUE) |>
  filter(TP53 == TRUE) |>
  filter(`Reason for termination` != "Patients wish") |>
  filter(`Reason for termination` != "Other (please use comment)") |>
  filter(!is.na(`Reason for termination`)) |>
  tbl_summary(
    by = `Reason for termination`,
    include = c(Age,
                Gender,
                `IPSS-R diagnosis`,
                `IPSS-R cytogenetic risk group`,
                `Therapy related`,
                `Marrow blasts`,
                ICT, 
                HMA,
                `No treatment`,
                a_gvhd,
                c_gvhd,
                age_donor,
                mean_level_first_40_days,
                any_mrd_above_background_0_40,
                mean_level_after_100_days,
                any_mrd_above_background_100),
    type = list(all_continuous() ~ "categorical",
                `Marrow blasts` ~ "continuous",
                age_donor ~ "continuous",
                mean_level_first_40_days ~ "continuous",
                mean_level_after_100_days ~ "continuous",
                Age ~ "continuous"),
    label = list(mean_level_first_40_days ~ "Mean MRD first 40 days",
                 any_mrd_above_background_0_40 ~ "Any MRD above background first 40 days",
                 mean_level_after_100_days ~ "Mean MRD after 100 days",
                 any_mrd_above_background_100 ~ "Any MRD above background after 100 days",
                 c_gvhd ~ "Chronic GVHD",
                 age_donor ~ "Donor age")
  ) |>
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Outcome**") |>
  add_p()

gtsave(as_gt(tp53_tab_outcome), filename = "tp53_tab_outcome.html", inline_css = TRUE)

# Create per patient table for the TP53 patients

tp53_pat_tab <- mrd_results |> 
  distinct(`Study number`, .keep_all = TRUE) |>
  filter(TP53 == TRUE) |>
  subset(select = c(`Study number`,
                    `Reason for termination`,
                    Age,
                    Gender,
                    `IPSS-R diagnosis`,
                    `IPSS-R cytogenetic risk group`,
                    Karyotype,
                    `Therapy related`,
                    `Marrow blasts`,
                    ICT, 
                    HMA,
                    `No treatment`,
                    a_gvhd,
                    c_gvhd,
                    mean_level_first_40_days,
                    any_mrd_above_background_0_40,
                    mean_level_after_100_days,
                    any_mrd_above_background_100)) |>
  gt(rowname_col = "Study number") 

gtsave(tp53_pat_tab, filename = "tp53_pat_tab.html", inline_css = TRUE)

### Kaplan Meier curves ####

xvars <- c("c_gvhd", 
           "a_gvhd", 
           "complex_karyotype",
           "any_mrd_above_background_0_40", 
           "any_mrd_above_background_100",
           "TP53",
           "NRAS", 
           "KRAS", 
           "DNMT3A", 
           "RUNX1", 
           "KDM6A")

pdf("14b_p1_survival_all.pdf")

for (varname in xvars) {
  
  formula <- as.formula(
    paste("Surv(dur_tx_relapsedeath_years, cens_RFS) ~", varname)
  )
  
  fit <- survfit(formula, data = mrd_results)
  
  fit$call$formula <- formula
  
  print(
    ggsurvplot(
      fit,
      data = mrd_results,
      legend = "right",
      pval = TRUE
    )
  )
}

dev.off()

# Create PDF of Kaplan-Meier curves for TP53 only cases

vars <- c("c_gvhd", 
          "a_gvhd", 
          "any_mrd_above_background_0_40", 
          "any_mrd_above_background_100",
          "NRAS", 
          "KRAS", 
          "DNMT3A", 
          "KDM6A", 
          "complex_karyotype")

pdf("14b_p1_survival_tp53_joel.pdf")

data_sub <- mrd_results |> 
  filter(TP53 == TRUE)

for (varname in vars) {
  
  formula <- as.formula(
    paste("Surv(dur_tx_relapsedeath_years, cens_RFS) ~", varname)
  )
  
  fit <- survfit(formula, data = data_sub)
  
  fit$call$formula <- formula
  
  print(
    ggsurvplot(
      fit,
      data = data_sub,
      legend = "right",
      pval = TRUE
    )
  )
}

dev.off()

# Create graph to visualize the % of MRD above background in the first 40 days, per TP53 or non-TP53
ggplot(data = mrd_results %>% select(`Study number`, TP53, any_mrd_above_background_0_40) %>% distinct(), aes(x = TP53, fill = any_mrd_above_background_0_40)) +
  geom_bar(position = "fill") +
  xlab("TP53") +
  ylab("Proportion") +
  theme_minimal()

ggplot(data = mrd_results %>% select(`Study number`, TP53, any_mrd_above_background_100) %>% distinct(), aes(x = TP53, fill = any_mrd_above_background_100)) +
  geom_bar(position = "fill") +
  xlab("TP53") +
  ylab("Count") +
  theme_minimal()

# Chi-square test, comparing the categories TP53 and any_mrd_above_background_0_40
test <- chisq.test(table(mrd_results$TP53, mrd_results$any_mrd_above_background_100))
test
test$p.value

# Create mrd_ratio against TP53 plot
ggplot(data = mrd_results %>% filter(relative_mrd_date > 0 & relative_mrd_date < 40) %>% select(`Study number`, TP53, mrd_ratio) %>% distinct(), aes(x = TP53, y = mrd_ratio)) +
  geom_violin() +
  geom_boxplot() +
  xlab("TP53") +
  theme_minimal() +
  scale_y_log10()

summary(aov(mrd_ratio ~ TP53, data = mrd_results %>% filter(relative_mrd_date > 0 & relative_mrd_date < 40)))
summary(aov(Level ~ TP53, data = mrd_results %>% filter(relative_mrd_date > 0 & relative_mrd_date < 40)))


# install.packages("ggalluvial")
library(ggalluvial)

ggplot(data = mrd_results %>% filter(TP53 == TRUE) %>% distinct(`Study number`, any_mrd_above_background_0_40, any_mrd_above_background_100, TP53),
       aes(axis1 = any_mrd_above_background_0_40, axis2 = any_mrd_above_background_100)) +
  geom_alluvium(aes(fill = any_mrd_above_background_0_40)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  theme_void() +
  labs(title = "MRD above background, change between days 0-40 and 100+, TP53 only")

ggplot(data = mrd_results %>% distinct(`Study number`, any_mrd_above_background_0_40, any_mrd_above_background_100),
       aes(axis1 = any_mrd_above_background_0_40, axis2 = any_mrd_above_background_100)) +
  geom_alluvium(aes(fill = any_mrd_above_background_0_40)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  theme_void() +
  labs(title = "MRD above background, change between days 0-40 and 100+")
