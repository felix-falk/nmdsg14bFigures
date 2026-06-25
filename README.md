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
library(nmdsg14bFigures)
packageVersion("nmdsg14bFigures")
```

# Usage

## Precautions

1. The package cannot read "\\" in file paths, which is the default on Windows. Therefore, replace any "\\" with "/" in your file paths. 

2. If any files used in the package are open in the background, the files cannot be read by the package. Therefore, close any open data files before running the package. 

3. The excel data files require the following columns

| Excel file | Required columns |
| ------- | ------- |
| ```general_info.xlsx``` |  ```patno, transpldt, termindat, eosreason, relapsedat, deathdat``` |
| ```mrd.xlsx``` |  ```patno, MRDdat, mutname, level``` |
| ```dli.xlsx``` | ```patno, dlidat``` |
| ```aza.xlsx``` | ```patno, azacitstdat``` |
| ```immune.xlsx``` | ```patno, drugname, drugdt, drugstopped``` |
| ```gvhd.xlsx``` | ```patno, gvhddate, agvhdstage, cgvhdstage, agvhdmaxstage, agvhdmaxdt, cgvhdmaxstage, cgvhdmaxdt``` |
| ```ngs.xlsx``` | ```Studienummer, Gen, cDNA förändring``` |

## Run the package in R

```r
# Optional: navigate to the files directory
setwd("~/study_files")

# Run nmds_figures_main
nmds_figures_main(
  general_info_file = "general_info.xlsx",
  mrd_file = "mrd.xlsx",
  dli_file = "dli.xlsx",
  aza_file = "aza.xlsx",
  immune_file = "immune.xlsx",
  gvhd_file = "gvhd.xlsx",
  ngs_file = "ngs.xlsx",
  chimerism_file = "chimerism.xlsx",
  immune_filter_file = "immune_suppression_filter.csv",
  output_folder = "~/nmdsg14bFigures_output",
  plot_type = "swimmerplot", 
  output_format = "pdf",
  filters = list(genes = "TP53", outcomes = "Remission")
)
```

## Output format

Set ```output_format``` to ```"pdf"``` (default) or ```"svg"```.

## Plot type

Set ```plot_type``` to ```"swimmerplot"```, ```"clinical_course"```, or ```"clinical_course_chimerism"```.

## Filtering

### Mandatory filtering

#### Immune suppression data

Only immune suppression treatments relevant to the study are kept using a ";"-separated csv file, provided by the user (see example below). Imunne suppressive treatments that should be excluded are put as exlude = TRUE. The immune suppresion treatment names are recognized using the ```pattern``` column, and are changed to a given ```standardized_name```. Treatments that are to be excluded from the analysis are put as ```TRUE``` in the ```exclude``` column.

| pattern | standardized_name | exclude |
| ------- | ------- | ------- |
| c.\*osp.\*in |  ciclosporin | FALSE |
| mmf\|my.\*enol | mycophenolic acid | FALSE |
| entocort\|jorv | budesonide | TRUE |

### Optional filtering

Pass a list to the filters option, containing your filters of interest. Exclude filters from the list if you do not wish to include them as criteria.

```r
# Select patients with TP53 and ASXL1 mutations, who either went to relapse or nonrelapse mortality, and were treated with Azacitidine: 
filters = list(
  genes = c(
    "TP53", 
    "ASXL1"
    ), 
  outcomes = c(
    "Relapse", 
    "Nonrelapse mortality"
    ), 
  treatments = c(
    "Azacitidine"
    )
)

# Select patients treated with DLI and never were MRD positive: 
filters = list(
  treatments = "DLI", 
  mrd_positive = false
)
```

The possible options for ```outcomes``` are ```"Remission"```, ```"Relapse"``` and ```"Nonrelapse mortality"```.
The possible options for ```treatments``` are ```"Azacitidine"``` and ```"Donor lymphocyte infusion"```.
The possible options for ```mrd_positive``` and ```immune_suppression``` are ```true```, ```false``` and ```null```.
