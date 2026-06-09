#' Draw clinical course figures and export to PDF.
#' 
#' @param processed A list of data frames containing the processed data for general_info, treatment, mrd, gvhd, immune_intervals, and ngs.
#' @param patient_subset A vector of patient IDs.
#' @param output_folder The folder where the output PDF will be saved.
#' @param output_filename The name of the output PDF file.
#' @returns A numeric vector.
#' @examples
#' draw_clinical_course(processed, patient_subset, "~/output", "clinical_course.pdf")
draw_clinical_course <- function(
    processed,
    patient_subset = NULL,
    output_folder,
    output_filename = "clinical_course.pdf"
) {
  
  if (nrow(processed$general_info) == 0) {
    stop("No patients available after filtering.")
  }
  
  if (!is.null(patient_subset)) {
    
    processed$general_info <-
      processed$general_info %>%
      filter(patno %in% patient_subset)
    
    processed$treatment <-
      processed$treatment %>%
      filter(patno %in% patient_subset)
    
    processed$mrd <-
      processed$mrd %>%
      filter(patno %in% patient_subset)
    
    processed$gvhd <-
      processed$gvhd %>%
      filter(patno %in% patient_subset)
    
    processed$immune_intervals <-
      processed$immune_intervals %>%
      filter(patno %in% patient_subset)
    
    processed$ngs <-
      processed$ngs %>%
      filter(patno %in% patient_subset)
  
  }
  
  # To achieve a log10 y axis scale, convert the 0 values in level to 0.09
  processed$mrd <- processed$mrd %>% mutate(level_no0s = ifelse(level == 0, 0.08, level))
  
  # Create dummy GVHD legends
  make_legend <- function(levels, colours, title) {
    df <- data.frame(stage=factor(levels, levels=levels), x=1, y=1)
    get_legend(
      ggplot(df, aes(x, y, colour=stage)) +
        geom_point(size=3) +
        scale_colour_manual(name=title, values=colours) +
        theme_void() + theme(legend.position="right")
    )
  }
  
  agvhd_colours <- c("0" = "#EBEBEB",
                     "1" = "#EDC0C0",
                     "2" = "#FF7878",
                     "3" = "#D42626",
                     "4" = "#800000")
  cgvhd_colours <- c("None" = "#EBEBEB",
                     "Mild" = "#AA88BB",
                     "Moderate" = "#622BD6",
                     "Severe" = "#290088")
  
  agvhd_legend_grob <- make_legend(names(agvhd_colours), agvhd_colours, "aGVHD Stage")
  cgvhd_legend_grob <- make_legend(names(cgvhd_colours), cgvhd_colours, "cGVHD Stage")
  
  # Define a function to generate MRD + GVHD timeline for a given patient
  
  plot_patient_timeline <- function(processed, pat_id) {
    
    # Select one patient
    d <- lapply(processed, function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if (!is.data.frame(df)) {
        return(df)
      }
      if (!"patno" %in% names(df)) {
        return(df)
      }
      dplyr::filter(df, patno == pat_id)
    })
    
    # Determine the range of the x axis
    x_end <- d$general_info$rel_term_dat[1]
    
    # Fallback if rel_term_dat does not exist
    if (is.na(x_end) || !is.finite(x_end)) {
      x_end <- 365
    }
    
    # Add 10 days to the upper x-axis limit
    x_range <- c(0, x_end)
    
    # Determine MRD y-axis range
    if(nrow(d$mrd) == 0 ||
       all(is.na(d$mrd$level_no0s))) {
      y_upper <- 10
    } else {
      y_upper <- max(
        10,
        ceiling(max(d$mrd$level_no0s, na.rm = TRUE))
      )
    }
    
    # ----------------------------
    # MRD plot (top)
    # ----------------------------
    mrd_plot <- ggplot() +
      
      annotate("rect",
               xmin = -Inf,
               xmax = Inf,
               ymin = 0.08,
               ymax = 0.1,
               fill = "lightgrey",
               alpha = 0.4) +
      
      geom_line(data = d$mrd %>%
                  filter(!is.na(Mutation)) %>%
                  group_by(Mutation) %>%
                  filter(n() > 1) %>%
                  ungroup(),
                aes(x = rel_mrd_dat,
                    y = level_no0s,
                    colour = Mutation)) +
      
      geom_point(data = d$mrd,
                 aes(x = rel_mrd_dat,
                     y = level_no0s,
                     colour = Mutation)) +
      
      theme_minimal() +
      
      xlab(NULL) +
      ylab(NULL) +
      
      scale_colour_brewer(palette="Set2", na.translate = FALSE) +
      
      scale_x_continuous(limits = x_range) +
      
      scale_y_log10(
        limits = c(0.08, y_upper),
        labels = label_number()) +
      
      # Add clinical information title
      labs(
        title = paste0(
          "Patient: ",
          pat_id
        ),
        subtitle = paste0(
          "Diagnosis: ",
          d$general_info$mdsdiagnosis,
          "\nIPSS-M: ",
          d$general_info$ipssm_title,
          "\nKaryotype: ",
          d$general_info$karyotyp,
          "\nNGS: ",
          d$ngs$mutlist
        )) +
      
      # Add relapse line
      geom_textvline(
        data = d$general_info %>% filter(outcome == "Relapse"),
        aes(xintercept = rel_term_dat,
            label = "Relapse")) +
      
      # Add death line
      geom_textvline(
        data = d$general_info %>% filter(outcome == "Nonrelapse mortality"),
        aes(xintercept = rel_term_dat,
            label = paste0("Death: ", deathcause))) +
      
      # Draw MRD threshold line
      geom_texthline(
        yintercept = 0.1,
        label = "MRD Threshold",
        linetype = "dashed",
        color = "darkgrey",
        size = 3,
        vjust = -0.2,
        hjust = 1) +
      
      theme(legend.position = "right",
            plot.title = element_text(size = 12),
            plot.subtitle = element_text(size = 9))
    
    # Extract mrd legend
    mrd_legend <- get_legend(mrd_plot)
    
    # Remove mrd legend from mrd_plot
    mrd_plot_clean <- mrd_plot + theme(legend.position="none")
    
    # ----------------------------
    # GVHD / IS events plot (bottom)
    # ----------------------------
    
    events_plot <- ggplot() +
      
      # aGVHD
      geom_point(
        data = d$gvhd %>% filter(gvhd == "Acute GVHD" & !is.na(agvhdstage)),
        aes(x = rel_gvhd_dat,
            y = 1,
            colour = agvhdstage),
        size = 3
      ) +
      
      scale_colour_manual(
        values = c("0" = "#EBEBEB",
                   "1" = "#EDC0C0",
                   "2" = "#FF7878",
                   "3" = "#D42626",
                   "4" = "#800000"),
        guide = "none"
      ) +
      
      ggnewscale::new_scale_colour() +
      
      # cGVHD
      geom_point(
        data = d$gvhd %>% filter(gvhd == "Chronic GVHD" & !is.na(cgvhdstage)),
        aes(x = rel_gvhd_dat,
            y = 2,
            colour = cgvhdstage),
        size = 3
      ) +
      
      scale_colour_manual(
        values = c("None" = "#EBEBEB",
                   "Mild" = "#AA88BB",
                   "Moderate" = "#622BD6",
                   "Severe" = "#290088"),
        guide = "none"
      ) +
      
      # Immune suppression duration
      geom_segment(
        data = d$immune_intervals,
        aes(
          x = interval_start,
          xend = interval_end,
          y = 3,
          yend = 3
        ),
        linewidth = 2,
        colour = "black"
      ) +
      
      # Azacitidine events
      geom_point(
        data = d$treatment %>% filter(treatment == "Azacitidine"),
        aes(x = rel_treatment_dat,
            y = 4),
        colour = "black",
        size = 3
      ) +
      
      # DLI events
      geom_point(
        data = d$treatment %>% filter(treatment == "Donor Lymphocyte Infusion"),
        aes(x = rel_treatment_dat,
            y = 5),
        colour = "black",
        size = 3
      ) +
      
      labs(x = "Days after transplantation", y = NULL) +
      
      theme_minimal() +
      
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10)
      ) +
      
      scale_x_continuous(limits = x_range) +
      
      scale_y_continuous(
        breaks = c(1, 2, 3, 4, 5),
        labels = c("aGVHD",
                   "cGVHD",
                   "Immune suppression",
                   "Azacitidine",
                   "DLI"),
        limits = c(0.5, 5.5))
    
    # Combine MRD + events vertically
    combined_plots <- plot_grid(
      mrd_plot_clean,
      events_plot,
      ncol=1,
      rel_heights=c(2,1),
      align="v",
      axis="tblr"
    )
    
    # Combine all legends vertically
    combined_legends <- plot_grid(mrd_legend, agvhd_legend_grob, cgvhd_legend_grob, ncol=1, align="v")
    
    # Final combined plot
    final_plot <- plot_grid(combined_plots, combined_legends, ncol=2, rel_widths=c(4,1), align="v")
    return(final_plot)
  }
  
  cat("\nProcessed objects:\n")
  
  print(
    sapply(
      processed,
      function(x) {
        if (is.null(x)) {
          "NULL"
        } else {
          paste(class(x), collapse = ", ")
        }
      }
    )
  )
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Export the figures to a pdf
  pdf(file.path(output_folder, output_filename), width=10, height=6)
  walk(unique(processed$general_info$patno),
       \(p) print(plot_patient_timeline(processed, p)))
  dev.off()
  
}
