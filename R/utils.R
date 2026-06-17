#' Called by the preprocess_data function to identify
#' immune suppression intervals, based on the immune suppression data frame.
#'
#' @param df Immune suppression data frame.
#' @returns A data frame with identified immune suppression intervals.
#' @examples
#' interval_finder(immune)
interval_finder <- function(df) {

  df |>
    dplyr::arrange(patno, drugname_standardized, rel_immune_dat) |>
    dplyr::group_by(patno, drugname_standardized) |>
    dplyr::group_modify(~ {

      stop_idx <- which(.x$drugstopped == "Yes")

      if (length(stop_idx) == 0) {
        return(tibble::tibble(
          interval_no = 1,
          interval_start = min(.x$rel_immune_dat, na.rm = TRUE),
          interval_end = max(.x$rel_term_dat, na.rm = TRUE)
        ))
      }

      starts <- c(1, stop_idx[-length(stop_idx)] + 1)
      ends <- stop_idx

      tibble::tibble(
        interval_no = seq_along(starts),
        interval_start = .x$rel_immune_dat[starts],
        interval_end = .x$rel_immune_dat[ends]
      )
    }) |>
    dplyr::ungroup()
}

#' Called by the preprocess_data function to standardize
#' drug names in the immune suppression data frame,
#' based on a provided mapping data frame.
#'
#' @param drug Drug name to standardize.
#' @param mapping_df Mapping data frame with patterns and standardized names.
#' @returns The standardized drug name or NA if no match is found.
#' @examples
#' standardize_drug("Drug A", immune_suppression_filter)
standardize_drug <- function(drug, mapping_df) {
  match_idx <- which(
    purrr::map_lgl(mapping_df$pattern,
        ~ stringr::str_detect(drug, stringr::regex(.x, ignore_case = TRUE)))
  )
  if (length(match_idx) > 0) {
    return(mapping_df$standardized_name[match_idx[1]])
  }
  return(NA_character_)
}

#' Called by the draw_clinical_course function to create GVHD dummy legends.
#'
#' @param levels The levels of the legend.
#' @param colours The colours for the legend.
#' @param title The title of the legend.
#' @returns A legend grob.
#' @examples
#' make_dummy_legend(c("0", "1", "2"), c("red", "blue", "green"), "GVHD Stage")
make_dummy_legend <- function(levels, colours, title) {
  df <- data.frame(stage = factor(levels, levels = levels), x = 1, y = 1)
  cowplot::get_legend(
    ggplot2::ggplot(df, ggplot2::aes(x, y, colour = stage)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_colour_manual(name = title, values = colours) +
      ggplot2::theme_void() + ggplot2::theme(legend.position = "right")
  )
}

#' Called by the draw_clinical_course function to select one patient per graph.
#'
#' @param df Data frame containing patient information with a "patno" column.
#' @returns Filtered data frame for a specific patient,
#' or the original data frame if it does not meet the criteria.
#' @examples
#' select_one_patient(df)
select_one_patient <- function(df, pat_id = NULL) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!is.data.frame(df)) {
    return(df)
  }
  if (!"patno" %in% names(df)) {
    return(df)
  }
  if (is.null(pat_id)) {
    return(df)
  }
  dplyr::filter(df, patno == pat_id)
}

#' Called by the draw_clinical_course function to
#' list the class of each element in the processed data list.
#'
#' @param x An object whose class needs to be identified.
#' @returns A string indicating the class of the object.
#' @examples
#' class_finder(processed)
class_finder <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else {
    paste(class(x), collapse = ", ")
  }
}

#' Called by the draw_clinical_course function to
#' find the upper limit of the MRD y-axis.
#'
#' @param mrd_data A data frame containing MRD data.
#' @returns A numeric value indicating the upper y-axis limit.
#' @examples
#' y_limit_finder(d$mrd)
y_limit_finder <- function(mrd_data) {

  # Determine the upper y-axis limit for the MRD figure
  # If there's no data or the column doesn't exist, fall back to 10
  if (is.null(mrd_data) || nrow(mrd_data) == 0 ||
      !"level_no0s" %in% names(mrd_data) ||
      all(is.na(mrd_data$level_no0s))) {
    y_upper <- 10
  } else {
    y_upper <- max(
      10,
      ceiling(max(mrd_data$level_no0s, na.rm = TRUE))
    )
  }

  return(y_upper)

}

#' Called by the draw_clinical_course function to
#' find the range of MRD x-axis values.
#'
#' @param general_info_data A data frame containing the
#' termination date of the patient, relative to the transplantation date.
#' @returns A numeric vector indicating the range of the x-axis.
#' @examples
#' x_range_finder(d$general_info)
x_range_finder <- function(general_info_data){

  # Determine the range of the x axis
  x_end <- general_info_data$rel_term_dat[1]

  # Fallback if rel_term_dat does not exist
  if (is.na(x_end) || !is.finite(x_end)) {
    x_end <- 365
  }

  # Add 10 days to the upper x-axis limit
  x_range <- c(0, x_end)

  return(x_range)

}
