---
title: "README.md"
author: "Felix Falk"
date: "2026-06-08"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Installation

In R: 

```r
knitr::opts_chunk$set(echo = TRUE)
```

# Usage

## Run the run_analysis.R script from the terminal

Set directory: 
```bash
cd ~/nmds_figures_project
```

```bash
Rscript run_analysis.R \
  --general_info_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx \
  --mrd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_MRD.XLSX \
  --dli_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_dlitrt.xlsx \
  --aza_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_azacitkurer.xlsx \
  --immune_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_immunsupptrtm.xlsx \
  --gvhd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_gvhddat.xlsx \
  --ngs_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx \
  --immune_filter_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/immune_suppression_filter.csv \
  --output_folder /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_output \
  --plot_type swimmerplot \
  --filters /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/nmdsFigures/filters/test_filters.json
```

## Run the script in R

```r
nmds_figures_main(
  general_info_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx",
  mrd_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_MRD.XLSX",
  dli_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_dlitrt.xlsx",
  aza_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_azacitkurer.xlsx",
  immune_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_immunsupptrtm.xlsx",
  gvhd_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_gvhddat.xlsx",
  ngs_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx",
  immune_filter_file = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/immune_suppression_filter.csv",
  output_folder = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_output",
  plot_type = "swimmerplot", 
  filters = c(genes = "TP53", outcomes = "Remission")
)
```

## Optional filtering

Pass either (1) a .json filepath or (2) an R-compatible list to the filters option, containing your filters of interest. Either work, but using an R list is probably the more user-friendly option. 

### .json filtering

Below is the test_filters.json file that is included in the package and found in the filters directory. It selects patients with the TP53 mutation as per the NGS data set, and the Relapse outcome as per the general information and MRD data sets. 

```json
{
  "genes": ["TP53"],
  "outcomes": ["Relapse"],
  "treatments": [],
  "mrd_positive": null,
  "immune_suppression": null
}
```

### R list filtering 

```r
filters = c(genes = c("TP53", "ASXL1"), outcomes = c("Remission", "Relapse", "Nonrelapse mortality"), treatments = c("Azacitidine", "Donor lymphocyte infusion"), mrd_positive = true, immune_suppresison = true)
```

## Install from GitHub

```r
# Install remotes package
install.packages("remotes")

# Install nmdsg14bFigures GitHub repository
remotes::install_github("felix-falk/nmdsg14bFigures")

# Verify that nmdsg14bFigures is installed
library(nmdsFigures)
packageVersion("nmdsFigures")
```
