#' Draw swimmer plot and export to PNG.
#' 
#' @param processed A list of data frames containing the processed data for general_info, treatment, mrd, gvhd, immune_intervals, and ngs.
#' @param patient_subset A vector of patient IDs.
#' @param output_folder The folder where the output PNG will be saved.
#' @param output_filename The name of the output PNG file.
#' @returns A numeric vector.
#' @examples
#' draw_swimmerplot(processed, patient_subset, "~/output", "swimmerplot.png")
draw_swimmerplot <- function(
    processed,
    patient_subset = NULL,
    output_folder,
    output_filename = "swimmerplot.png"
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
  
  title_string <- "All patients"
  
  # --- MRD RECTANGLES ---
  
  mrd_base <- processed$mrd %>%
    select(patno, rel_mrd_dat, mrd_category, rel_term_dat) %>%
    distinct() %>%
    arrange(patno, rel_mrd_dat)
  
  mrd_rectangles <- mrd_base %>%
    group_by(patno) %>% # Perform following calculations on a per-patient basis
    mutate(
      xmin = rel_mrd_dat,
      xmax = coalesce(lead(rel_mrd_dat), first(rel_term_dat) + 5)
    ) %>%
    ungroup() %>%
    select(patno, xmin, xmax, mrd_category, rel_term_dat) %>%
    bind_rows(
      mrd_base %>%
        group_by(patno) %>%
        slice(1) %>%
        transmute(
          patno,
          xmin         = 0,
          xmax         = rel_mrd_dat,
          mrd_category = if_else(rel_mrd_dat == 0, mrd_category, NA),
          rel_term_dat
        ) %>%
        ungroup()
    ) %>%
    filter(xmin != xmax) %>%
    arrange(patno, xmin) %>%
    group_by(patno) %>%
    mutate(rect_index = row_number()) %>%
    ungroup()
  
  # Calculate mrd_terminal
  mrd_terminal <- mrd_base %>% filter(rel_mrd_dat == rel_term_dat)
  
  # --- PLOT DATA & LOOKUP ---
  
  plot_data <- mrd_rectangles %>%
    group_by(patno) %>%
    mutate(max_end_event = first(rel_term_dat)) %>%
    ungroup() %>%
    arrange(max_end_event, patno, xmin) %>%
    mutate(
      patno_factor = factor(patno, levels = unique(patno)),
      y    = as.numeric(patno_factor),
      ymin = y,
      ymax = y
    )
  
  patient_y <- distinct(select(plot_data, patno, y))
  
  # Pre-build all annotation datasets once, outside ggplot()
  mrd_terminal_pts <- mrd_terminal %>%
    left_join(patient_y, by = "patno")
  outcome_pts <- processed$general_info %>% 
    select(patno, rel_term_dat, outcome) %>%
    distinct() %>% 
    left_join(patient_y, by = "patno")
  gvhd_pts <- processed$gvhd %>%
    distinct() %>%
    left_join(patient_y, by = "patno")
  treatment_pts <- processed$treatment %>%
    distinct() %>%
    left_join(patient_y, by = "patno")
  immune_pts <- processed$immune_intervals %>%
    left_join(patient_y, by = "patno")
  
  # --- SWIMMER PLOT FUNCTION ---
  
  print(processed$gvhd)
  
  swimmerplot <- function(plot_data,
                          immune_pts,
                          outcome_pts,
                          treatment_pts,
                          gvhd_pts,
                          title_string){
    swimmer_plot <- ggplot(plot_data) +
      
      geom_rect(aes(xmin = xmin, xmax = xmax,
                    ymin = ymin - 0.2, ymax = ymax + 0.2,
                    fill = mrd_category),
                color = "black") +
      
      scale_fill_manual(
        name = "MRD category",
        values = c(
          "Negative (< 0.1)"       = "#FFFFFF",
          "Low (0.1 - 0.5)"        = "#FFD65C",
          "Intermediate (0.5 - 1)" = "#FF9800",
          "High (> 1)"             = "#F21C0D"
        ),
        na.value = "lightgrey",
        guide = guide_legend(order = 1)) +
      
      # Add immune suppression line
      geom_segment(
        data = immune_pts,
        aes(
          x = interval_start,
          xend = interval_end,
          y = y + 0.3,
          yend = y + 0.3,
          linetype = "Immune suppression"
        ),
        linewidth = 1.5,
        color = "brown"
      ) +
      
      scale_linetype_manual(
        name = NULL,
        values = c("Immune suppression" = "solid"),
        guide = guide_legend(
          order = 5,
          override.aes = list(
            linewidth = 2.5,
            color = "brown"
          )
        )
      ) +
      
      geom_text(data = filter(outcome_pts, outcome == "Relapse"),
                aes(x = rel_term_dat + 5, y = y, label = "R"), hjust = -0.2) +
      
      geom_text(data = filter(outcome_pts, outcome == "Nonrelapse mortality"),
                aes(x = rel_term_dat + 5, y = y, label = "\u2020"), hjust = -0.2) +
      
      # Add MRD annotations at the final recorded date
      geom_point(
        data = mrd_terminal_pts,
        aes(
          x = rel_term_dat + 5,
          y = y,
          fill = mrd_category
        ),
        shape = 22,
        size = 1,
        color = "black"
      ) +
      
      new_scale_fill() +
      
      geom_point(data = treatment_pts %>% filter(!is.na(treatment)),
                 aes(x = rel_treatment_dat, y = y - 0.3, fill = treatment), color = "black", shape = 24) +
      
      scale_fill_manual(
        name = "Treatment",
        values = c(
          "Donor Lymphocyte Infusion" = "darkgrey",
          "Azacitidine" = "white"),
        guide = guide_legend(order = 2)) +
      
      new_scale_fill() +
      
      # Add acute GVHD points
      geom_point(data = gvhd_pts %>% 
                   filter(gvhd == "Acute GVHD", agvhdstage %in% c(3, 4)),
                 aes(x = rel_gvhd_dat, y = y - 0.3, fill = agvhdstage), color = "black", shape = 23) +
      
      scale_fill_manual(
        name = "Acute GVHD",
        values = c(
          "3" = "#FF8A8A",
          "4" = "#D10000"),
        guide = guide_legend(order = 3)) +
      
      new_scale_fill() +
      
      # Add chronic GVHD points
      geom_point(data = gvhd_pts %>% filter(gvhd == "Chronic GVHD", cgvhdstage %in% c("Moderate", "Severe")),
                 aes(x = rel_gvhd_dat, y = y - 0.3, fill = cgvhdstage), color = "black", shape = 23) +
      
      scale_fill_manual(
        name = "Chronic GVHD",
        values = c(
          "Moderate" = "#27D6F5",
          "Severe"   = "#5B27F5"),
        guide = guide_legend(order = 4)) +
      
      labs(x = "Days from transplantation", y = "Patient",
           title = "NMDS14B Part 2", subtitle = title_string) +
      
      # Start the x-axis at 0, equivalent to the date of transplantation
      xlim(0, NA) +
      
      scale_y_continuous(
        breaks = unique(plot_data$y),
        labels = unique(plot_data$patno),
        expand = expansion(add = c(1, 1))
      ) +
      
      theme_classic()
    
    return(swimmer_plot)
  }
  
  # Run plotting function
  swimmer_plot <- swimmerplot(plot_data,
                              immune_pts,
                              outcome_pts,
                              treatment_pts,
                              gvhd_pts,
                              title_string)
  
  # --- EXPORT ---
  ggsave(
    filename = file.path(output_folder, output_filename),
    plot = swimmer_plot,
    width = 6,
    height = max(5, length(unique(plot_data$patno)) * 0.2),
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
}

