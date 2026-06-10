#' Called by the preprocess_data function to identify immune suppression intervals based on the immune suppression data frame.
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
            interval_start = min(.x$rel_immune_dat),
            interval_end = max(.x$rel_term_dat)
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

#' Called by the preprocess_data function to standardize drug names in the immune suppression data frame based on a provided mapping data frame.
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
        if(length(match_idx) > 0) {
            return(mapping_df$standardized_name[match_idx[1]])
            }
            return(NA_character_)
            }


#' Called by the draw_clinical_course function to create dummy legends for GVHD stages.
#' 
#' @param levels The levels of the legend.
#' @param colours The colours for the legend.
#' @param title The title of the legend.
#' @returns A legend grob.
#' @examples
#' make_dummy_legend(c("0", "1", "2"), c("red", "blue", "green"), "GVHD Stage")
make_dummy_legend <- function(levels, colours, title) {
    df <- data.frame(stage=factor(levels, levels=levels), x=1, y=1)
    cowplot::get_legend(
        ggplot2::ggplot(df, ggplot2::aes(x, y, colour=stage)) +
        ggplot2::geom_point(size=3) +
        ggplot2::scale_colour_manual(name=title, values=colours) +
        ggplot2::theme_void() + ggplot2::theme(legend.position="right")
        )
        }

#' Called by the draw_clinical_course function to select one patient per graph.
#' 
#' @param df Data frame containing patient information with a "patno" column.
#' @returns Returns a filtered data frame for a specific patient or the original data frame if it does not meet the criteria.
#' @examples
#' select_one_patient(df)
select_one_patient <- function(df) {
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
                }

#' Called by the draw_clinical_course function to list the class of each element in the processed data list.
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
