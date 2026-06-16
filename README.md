# nmdsg14bFigures

**Version: 0.1.0**

**Author: [Felix Falk](https://github.com/felix-falk)**

## About

An R package that draws a swimmer plot or per-patient clinical course figures based on the data sets collected in the NMDSG14B part 2 clinical study.

## Installation

In R: 

```r
# Install remotes package
install.packages("remotes")

# Install nmdsg14bFigures GitHub repository
remotes::install_github("felix-falk/nmdsg14bFigures")

# Verify that nmdsg14bFigures is installed
library(nmdsFigures)
packageVersion("nmdsFigures")
```

# Usage

## Precautions

1. The package cannot read "\\" in file paths, which is the default on Windows. Therefore, replace any "\\" with "/" in your file paths. 

2. If any excel files that are used in the package are open in the background, the excel files cannot be read by the package. Therefore, close any excel files before running the package. 

## Run the run_analysis.R script from the terminal

Move to the directory containing your data files: 

```bash
cd ~/mds_project
```

```bash
Rscript run_analysis.R \
  --general_info_file general_info.xlsx \
  --mrd_file dli.xlsx \
  --dli_file mrd.xlsx \
  --aza_file aza.xlsx \
  --immune_file immune.xlsx \
  --gvhd_file gvhd.xlsx \
  --ngs_file ngs.xlsx \
  --immune_filter_file immune_suppression_filter.csv \
  --output_folder ~/NMDS14B_p2_output \
  --plot_type swimmerplot \
  --filters ~/nmdsg14bFigures/filters/test_filters.json
```

## Run the package in R

```r
nmds_figures_main(
  general_info_file = "general_info.xlsx",
  mrd_file = "mrd.xlsx",
  dli_file = "dli.xlsx",
  aza_file = "aza.xlsx",
  immune_file = "immune.xlsx",
  gvhd_file = "gvhd.xlsx",
  ngs_file = "ngs.xlsx",
  immune_filter_file = "immune_suppression_filter.csv",
  output_folder = "~/NMDS14B_p2_output",
  plot_type = "swimmerplot", 
  filters = c(genes = "TP53", outcomes = "Remission")
)
```

## Mandatory filtering of immune suppression data

Only immune suppression treatments relevant to the study are kept using a ";"-separated csv file, provided by the user (see example below). Imunne suppressive treatments that should be excluded are put as exlude = TRUE.

| pattern | standardized_name | exclude |
| ------- | ------- | ------- |
| c.\*osp.\*in |  ciclosporin | FALSE |
| mmf\|my.\*enol | mycophenolic acid | FALSE |
| entocort\|jorv | budesonide | TRUE |

## Optional filtering

Pass either (1) a .json filepath or (2) an R-compatible list to the filters option, containing your filters of interest. Either work, but using an R list is probably the more user-friendly option. 

### (1) .json filtering

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

### (2) R list filtering 

The possible options for ```outcomes ``` are ```"Remission" ```, ```"Relapse" ``` and ```"Nonrelapse mortality" ```.
The possible options for ```treatments ``` are ```"Azacitidine" ``` and ```"Donor lymphocyte infusion" ```.
The possible options for ```mrd_positive ``` and ```immune_suppression ``` are ```true ```, ```false ``` and ```null ```.

```r
filters = c(
  genes = c(
    "TP53", 
    "ASXL1"
    ), 
  outcomes = c(
    "Remission", 
    "Relapse", 
    "Nonrelapse mortality"
    ), 
  treatments = c(
    "Azacitidine", 
    "Donor lymphocyte infusion"
    ), 
  mrd_positive = true, 
  immune_suppresison = true
)
```
