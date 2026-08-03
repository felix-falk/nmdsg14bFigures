# nmdsg14bFigures

**Version: 0.1.0**

**Author: [Felix Falk](https://github.com/felix-falk)**

## About

An R package that draws swimmer plots, clinical course figures, chimerism figures and survival figures based on the data sets collected in the NMDSG14B parts 1 and 2 clinical studies.

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

2. The package cannot read files that are open in the background. 

3. The package cannot read excel files that are password protected. 

3. The excel data files require the following columns:

| Excel file | Required columns |
| ------- | ------- |
| ```general_info.xlsx``` |  ```patno, transpldt, termindat, eosreason, mdsdiagnosis, karyotyp, deathcause``` |
| ```mrd.xlsx``` |  ```patno, MRDdat, mutname, level``` |
| ```dli.xlsx``` | ```patno, dlidat``` |
| ```aza.xlsx``` | ```patno, azacitstart``` |
| ```immune.xlsx``` | ```patno, drugname, drugdt, drugstopped``` |
| ```gvhd.xlsx``` | ```patno, gvhddate, agvhdstage, cgvhdstage, agvhdmaxstage, agvhdmaxdt, cgvhdmaxstage, cgvhdmaxdt``` |
| ```ngs.xlsx``` | ```patno, Gen, cDNA forandring``` |
| ```chimerism.xlsx``` | ```patno, chimbmdt, surface_marker``` |

## Run the package in R

```r
# Optional: navigate to the files directory
setwd("~/study_files")

# Create a swimmerplot
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

# Create a clinical course figure
nmds_figures_main(
  general_info_file = "general_info.xlsx",
  mrd_file = "mrd.xlsx",
  dli_file = "dli.xlsx",
  aza_file = "aza.xlsx",
  immune_file = "immune.xlsx",
  gvhd_file = "gvhd.xlsx",
  ngs_file = "ngs.xlsx",
  immune_filter_file = "immune_suppression_filter.csv",
  output_folder = "~/nmdsg14bFigures_output",
  plot_type = "clinical_course", 
  output_format = "pdf",
  filters = list(genes = "TP53", outcomes = "Remission")
)

# Create a clinical course and chimerism figure
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
  plot_type = "clinical_course_chimerism", 
  output_format = "pdf",
  filters = list(genes = "TP53", outcomes = "Remission")
)

# Create a survival figure
nmds_figures_main(
  general_info_file = "general_info.xlsx",
  mrd_file = "mrd.xlsx",
  ngs_file = "ngs.xlsx",
  immune_file = "immune.xlsx",
  immune_filter_file = "immune_suppression_filter.csv",
  output_folder = "~/nmdsg14bFigures_output",
  plot_type = "survival", 
  output_format = "svg",
  strata_filename = "ngs",
  strata_colname = "Gen",
  strata_itemname = "TP53",
  survival_baseline = "transplant",
  survival_metric = "rfs"
)
```

## Output format

Set ```output_format``` to ```"pdf"``` or ```"svg"```.

## Plot type

Set ```plot_type``` to ```"swimmerplot"```, ```"clinical_course"```, ```"clinical_course_chimerism"``` or ```"survival"```.

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

| Filter name | Possible options |
| ------- | ------- |
| ```outcomes``` |  ```"Remission"```, ```"Relapse"```, ```"Nonrelapse mortality"``` |
| ```treatments``` | ```"Azacitidine"```, ```"Donor lymphocyte infusion"``` |
| ```mrd_positive``` | ```true```, ```false```, ```null``` |
| ```immune_suppression``` | ```true```, ```false```, ```null``` |

| Survival setting name | Possible options | Explanation |
| ------- | ------- | ------- |
| ```strata_filename``` | ```"general_info"```, ```"mrd"```, ```"dli"```, ```"aza"```, ```"immune"```, ```"gvhd"```, ```"ngs"```, ```"chimerism"``` | The file containing the strata you wish to use in the survival analysis. |
| ```survival_baseline``` | ```"transplant"```, ```"rel_pos_mrd_dat_0.1"```, ```"rel_pos_mrd_dat_0.5"```, ```"rel_pos_mrd_dat_1.0"``` | The event at which the survival analysis starts, either transplantation or when MRD first reaches a certain level (0.1, 0.5, 1.0 %) after transplantation. |
| ```survival_metric``` | ```"os"```, ```"rfs"```, ```"efs"``` | Overall survival, relapse-free survival or event-free survival. |

