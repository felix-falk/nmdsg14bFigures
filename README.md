---
title: "README.md"
author: "Felix Falk"
date: "2026-06-08"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Run the run_analysis.R script from the terminal

Set directory: 
cd /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_repository/mds_repository_scripts/nmds_figures_project

Run run_analysis.R script: 
Rscript run_analysis.R \
  --general_info_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx \
  --mrd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_MRD.XLSX \
  --dli_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_dlitrt.xlsx \
  --aza_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_azacitkurer.xlsx \
  --immune_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_immunsupptrtm.xlsx \
  --gvhd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_gvhddat.xlsx \
  --ngs_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx \
  --immune_filter_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/immune_suppression_filter.csv \
  --processed_folder processed \
  --output_folder /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_output \
  --plot_type clinical_course \
  --filters filters/example_filters.json
  
Rscript run_analysis.R \
  --general_info_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx \
  --mrd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_MRD.XLSX \
  --dli_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_dlitrt.xlsx \
  --aza_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_azacitkurer.xlsx \
  --immune_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_immunsupptrtm.xlsx \
  --gvhd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_gvhddat.xlsx \
  --ngs_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx \
  --immune_filter_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/immune_suppression_filter.csv \
  --processed_folder processed \
  --output_folder /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_output \
  --plot_type swimmerplot \
  --filters filters/example_filters.json

## Run the script in R

Rscript run_analysis.R \
  --general_info_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx \
  --mrd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_MRD.XLSX \
  --dli_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_dlitrt.xlsx \
  --aza_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_azacitkurer.xlsx \
  --immune_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_immunsupptrtm.xlsx \
  --gvhd_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NMDS14B_gvhddat.xlsx \
  --ngs_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx \
  --immune_filter_file /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_data/immune_suppression_filter.csv \
  --processed_folder processed \
  --output_folder /Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project/NMDS14B_p2_output \
  --plot_type clinical_course \
  --filters filters/example_filters.json

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

    filters = "/Users/felix.falk/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/nmdsFigures/filters/test_filters.json"
)

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.