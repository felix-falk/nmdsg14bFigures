#!/usr/bin/env Rscript

source("R/preprocess_data.R")
source("R/apply_filters.R")
source("R/draw_swimmerplot.R")
source("R/draw_clinical_course.R")

# ==========================================================
# COMMAND LINE OPTIONS
# ==========================================================

option_list <- list(
  
  make_option(
    "--general_info_file",
    type = "character",
    help = "Excel file with general patient clinical information"
  ),
  
  make_option(
    "--mrd_file",
    type = "character",
    help = "Excel file with mrd measurements"
  ),
  
  make_option(
    "--dli_file",
    type = "character",
    help = "Excel file with dli treatment information"
  ),
  
  make_option(
    "--aza_file",
    type = "character",
    help = "Excel file with Azacitidine treatment information"
  ),
  
  make_option(
    "--immune_file",
    type = "character",
    help = "Excel file with immune suppression treatment information"
  ),
  
  make_option(
    "--gvhd_file",
    type = "character",
    help = "Excel file with gvhd events information"
  ),
  
  make_option(
    "--ngs_file",
    type = "character",
    help = "Excel file with ngs information"
  ),
  
  make_option(
    "--immune_filter_file",
    type = "character",
    help = "CSV file with immune suppression filter"
  ),
  
  make_option(
    "--processed_folder",
    type = "character",
    help = "Folder for processed CSV outputs"
  ),
  
  make_option(
    "--output_folder",
    type = "character",
    help = "Folder for generated figures"
  ),
  
  make_option(
    "--plot_type",
    type = "character",
    help = "swimmerplot or clinical_course"
  ),
  
  make_option(
    "--filters",
    type = "character",
    default = NULL,
    help = "Optional JSON filter file"
  )
)

args <- parse_args(
  OptionParser(option_list = option_list)
)

# ==========================================================
# VALIDATE INPUTS
# ==========================================================

if (is.null(args$general_info_file)) {
  stop("--general_info_file is required")
}

if (is.null(args$mrd_file)) {
  stop("--mrd_file is required")
}

if (is.null(args$dli_file)) {
  stop("--dli_file is required")
}

if (is.null(args$aza_file)) {
  stop("--aza_file is required")
}

if (is.null(args$immune_file)) {
  stop("--immune_file is required")
}

if (is.null(args$gvhd_file)) {
  stop("--gvhd_file is required")
}

if (is.null(args$ngs_file)) {
  stop("--ngs_file is required")
}

if (is.null(args$immune_filter_file)) {
  stop("--immune_filter_file is required")
}

if (is.null(args$processed_folder)) {
  stop("--processed_folder is required")
}

if (is.null(args$output_folder)) {
  stop("--output_folder is required")
}

if (is.null(args$plot_type)) {
  stop("--plot_type is required")
}

# ==========================================================
# LOAD FILTERS
# ==========================================================

user_filters <- NULL

if (!is.null(args$filters)) {
  
  user_filters <- jsonlite::fromJSON(args$filters)
  
  message(
    sprintf(
      "Loaded filter file: %s",
      args$filters
    )
  )
}

# ==========================================================
# PREPROCESS DATA
# ==========================================================

message("Preprocessing data...")

processed <- preprocess_data(
  general_info_file = args$general_info_file,
  mrd_file = args$mrd_file,
  dli_file = args$dli_file,
  aza_file = args$aza_file,
  immune_file = args$immune_file,
  gvhd_file = args$gvhd_file,
  ngs_file = args$ngs_file,
  immune_filter_file = args$immune_filter_file,
  processed_folder = args$processed_folder
)

# ==========================================================
# OPTIONAL FILTERING
# ==========================================================

selected_patients <- NULL

if (!is.null(user_filters)) {
  
  message("Applying patient filters...")
  
  filter_results <- apply_filters(
    processed = processed,
    filters = user_filters
  )
  
  selected_patients <- filter_results$patient_ids
  
  message(
    sprintf(
      "%d patients matched filter criteria",
      filter_results$n_patients
    )
  )
}

# ==========================================================
# PLOT GENERATION
# ==========================================================

switch(
  
  args$plot_type,
  
  swimmerplot = {
    
    draw_swimmerplot(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = args$output_folder
    )
    
  },
  
  clinical_course = {
    
    draw_clinical_course(
      processed = processed,
      patient_subset = selected_patients,
      output_folder = args$output_folder
    )
    
  },
  
  stop(
    "plot_type must be either 'swimmerplot' or 'clinical_course'"
  )
)

message("Finished.")