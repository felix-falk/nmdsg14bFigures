pkgname <- "nmdsg14bFigures"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('nmdsg14bFigures')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apply_filters")
### * apply_filters

flush(stderr()); flush(stdout())

### Name: apply_filters
### Title: Manual data filtering ahead of plotting.
### Aliases: apply_filters

### ** Examples

processed <- list(
  general_info = data.frame(patno = c(1, 2), outcome = c("Relapse", "Remission")),
  treatment = data.frame(patno = 1, treatment = "Azacitidine"),
  mrd = data.frame(patno = 1, level = 0.2),
  immune = data.frame(patno = 1),
  ngs = data.frame(patno = 1, Gen = "TP53")
)
filters <- list(outcomes = "Relapse")
apply_filters(processed, filters)



cleanEx()
nameEx("column_check")
### * column_check

flush(stderr()); flush(stdout())

### Name: column_check
### Title: Called by preprocess_data to check that all required columns are
###   present in the provided file.
### Aliases: column_check

### ** Examples

column_check(dli_raw, c("patno", "dlidat"))



cleanEx()
nameEx("create_treatment_df")
### * create_treatment_df

flush(stderr()); flush(stdout())

### Name: create_treatment_df
### Title: Creates treatment data frame based on dli and aza data frames.
### Aliases: create_treatment_df

### ** Examples

create_treatment_df(aza_raw, dli_raw, end_date_df)



cleanEx()
nameEx("draw_chimerism_plot")
### * draw_chimerism_plot

flush(stderr()); flush(stdout())

### Name: draw_chimerism_plot
### Title: Generate MRD + chimerism plot for a given patient.
### Aliases: draw_chimerism_plot

### ** Examples

draw_chimerism_plot(
  d$mrd,
  d$general_info,
  d$ngs_data,
  d$chimerism_data,
  x_range,
  y_upper,
  pat_id
)



cleanEx()
nameEx("draw_clinical_course")
### * draw_clinical_course

flush(stderr()); flush(stdout())

### Name: draw_clinical_course
### Title: Draw clinical course figures and export to PDF.
### Aliases: draw_clinical_course

### ** Examples

draw_clinical_course(
processed,
patient_subset,
"~/output",
"clinical_course.pdf"
)



cleanEx()
nameEx("draw_clinical_course_chimerism")
### * draw_clinical_course_chimerism

flush(stderr()); flush(stdout())

### Name: draw_clinical_course_chimerism
### Title: Draw clinical course figures and export to PDF.
### Aliases: draw_clinical_course_chimerism

### ** Examples

draw_clinical_course_chimerism(
processed,
patient_subset,
"~/output",
"clinical_course.pdf",
output_format = "pdf"
)



cleanEx()
nameEx("draw_events_plot")
### * draw_events_plot

flush(stderr()); flush(stdout())

### Name: draw_events_plot
### Title: Generate events plot for a given patient
### Aliases: draw_events_plot

### ** Examples

draw_events_plot(d$gvhd, d$immune_intervals, d$treatment, x_range)



cleanEx()
nameEx("draw_mrd_plot")
### * draw_mrd_plot

flush(stderr()); flush(stdout())

### Name: draw_mrd_plot
### Title: Generate MRD plot for a given patient
### Aliases: draw_mrd_plot

### ** Examples

## Not run: 
##D mrd_data <- data.frame(
##D   rel_mrd_dat = c(0, 30),
##D   level_no0s = c(0.2, 0.1),
##D   Mutation = c("TP53", "TP53")
##D )
##D general_info_data <- data.frame(
##D   mdsdiagnosis = "MDS",
##D   ipssm_title = "High",
##D   karyotyp = "del(5q)",
##D   outcome = "Remission",
##D   rel_term_dat = 100,
##D   deathcause = NA_character_
##D )
##D ngs_data <- data.frame(mutlist = "TP53")
##D draw_mrd_plot(mrd_data, general_info_data, ngs_data, c(0, 100), 1, "P1")
## End(Not run)



cleanEx()
nameEx("draw_swimmerplot")
### * draw_swimmerplot

flush(stderr()); flush(stdout())

### Name: draw_swimmerplot
### Title: Draw swimmer plot and export to PNG.
### Aliases: draw_swimmerplot

### ** Examples

## Not run: 
##D processed <- list(
##D   general_info = data.frame(patno = 1, rel_term_dat = 100, outcome = "Remission"),
##D   treatment = data.frame(patno = 1, treatment = "Azacitidine", rel_treatment_dat = 20),
##D   mrd = data.frame(patno = 1, rel_mrd_dat = 0, mrd_category = "Negative (< 0.1)", rel_term_dat = 100),
##D   gvhd = data.frame(patno = 1),
##D   immune_intervals = data.frame(patno = 1),
##D   ngs = data.frame(patno = 1)
##D )
##D draw_swimmerplot(processed, patient_subset = c(1), output_folder = tempdir(), output_filename = "swimmerplot", output_format = "svg")
## End(Not run)



cleanEx()
nameEx("interval_finder")
### * interval_finder

flush(stderr()); flush(stdout())

### Name: interval_finder
### Title: Called by the preprocess_data function to identify immune
###   suppression intervals, based on the immune suppression data frame.
### Aliases: interval_finder

### ** Examples

interval_finder(immune)



cleanEx()
nameEx("make_dummy_legend")
### * make_dummy_legend

flush(stderr()); flush(stdout())

### Name: make_dummy_legend
### Title: Called by the draw_clinical_course function to create GVHD dummy
###   legends.
### Aliases: make_dummy_legend

### ** Examples

make_dummy_legend(c("0", "1", "2"), c("red", "blue", "green"), "GVHD Stage")



cleanEx()
nameEx("nmds_figures_main")
### * nmds_figures_main

flush(stderr()); flush(stdout())

### Name: nmds_figures_main
### Title: Run analysis using the nmds_figures_main function.
### Aliases: nmds_figures_main

### ** Examples

nmds_figures_main(
 general_info_file = "general.xlsx",
 mrd_file = "mrd.xlsx",
 dli_file = "dli.xlsx",
 aza_file = "aza.xlsx",
 immune_file = "immune.xlsx",
 gvhd_file = "gvhd.xlsx",
 ngs_file = "ngs.xlsx",
 chimerism_file = "chimerism.xlsx",
 immune_filter_file = "immune_filter.csv",
 output_folder = "output",
 plot_type = "clinical_course",
 filters = list(
   genes = "TP53",
   outcomes = "Relapse"
 )
)



cleanEx()
nameEx("plot_chimerism_timeline")
### * plot_chimerism_timeline

flush(stderr()); flush(stdout())

### Name: plot_chimerism_timeline
### Title: Generate MRD + Chimerism + GVHD timeline for a given patient
### Aliases: plot_chimerism_timeline

### ** Examples

plot_patient_timeline(processed, pat_id)



cleanEx()
nameEx("plot_patient_timeline")
### * plot_patient_timeline

flush(stderr()); flush(stdout())

### Name: plot_patient_timeline
### Title: Generate MRD + GVHD timeline for a given patient
### Aliases: plot_patient_timeline

### ** Examples

plot_patient_timeline(processed, pat_id)



cleanEx()
nameEx("preprocess_data")
### * preprocess_data

flush(stderr()); flush(stdout())

### Name: preprocess_data
### Title: Preprocess data ahead of manual filtering and plotting and.
### Aliases: preprocess_data

### ** Examples

preprocess_data(
general_info_file = "general_info.xlsx",
mrd_file = "mrd.xlsx",
dli_file = "dli.xlsx",
aza_file = "aza.xlsx",
immune_file = "immune.xlsx",
gvhd_file = "gvhd.xlsx",
ngs_file = "ngs.xlsx",
immune_filter_file = "immune_filter.csv"
)



cleanEx()
nameEx("select_one_patient")
### * select_one_patient

flush(stderr()); flush(stdout())

### Name: select_one_patient
### Title: Called by the draw_clinical_course function to select one
###   patient per graph.
### Aliases: select_one_patient

### ** Examples

select_one_patient(df)



cleanEx()
nameEx("standardize_drug")
### * standardize_drug

flush(stderr()); flush(stdout())

### Name: standardize_drug
### Title: Called by the preprocess_data function to standardize drug names
###   in the immune suppression data frame, based on a provided mapping
###   data frame.
### Aliases: standardize_drug

### ** Examples

standardize_drug("Drug A", immune_suppression_filter)



cleanEx()
nameEx("swimmerplot")
### * swimmerplot

flush(stderr()); flush(stdout())

### Name: swimmerplot
### Title: Called by draw_swimmerplot to draw swimmer plot.
### Aliases: swimmerplot

### ** Examples

swimmerplot(
plot_data,
immune_pts,
outcome_pts,
treatment_pts,
gvhd_pts,
mrd_terminal_pts,
title_string
)



cleanEx()
nameEx("x_range_finder")
### * x_range_finder

flush(stderr()); flush(stdout())

### Name: x_range_finder
### Title: Called by the draw_clinical_course function to find the range of
###   MRD x-axis values.
### Aliases: x_range_finder

### ** Examples

x_range_finder(d$general_info)



cleanEx()
nameEx("y_limit_finder")
### * y_limit_finder

flush(stderr()); flush(stdout())

### Name: y_limit_finder
### Title: Called by the draw_clinical_course function to find the upper
###   limit of the MRD and/or chimerism y-axis.
### Aliases: y_limit_finder

### ** Examples

y_limit_finder(d$mrd)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
