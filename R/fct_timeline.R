#' Geology timeline eons and eras
#' @noRd
geotime <- tibble::tribble(
  ~type,  ~eon_era_name,              ~early_age,~late_age,
  "eon",  "Phanerozoic",      541,0,
  "eon",  "Proterozoic",      2500,541,
  "eon",  "Archean",          4000,2500,
  "eon",  "Hadean",           4600,4000,
  "era",  "Ceno",         66,0,
  "era",  "Meso",         252,66,
  "era",  "Paleo",        541,252,
  "era",  "Neoproterozoic",   1000,541,
  "era",  "Mesoproterozoic",  1600,1000,
  "era",  "Paleoproterozoic", 2500,1600,
  "era",  "Neoarchaen",   2800,2500,
  "era",  "Mesoarchaen",      3200,2800,
  "era",  "Paleoarchaen",     3600,3200,
  "era",  "Eoarchaen",        4000,3600)


#' Upper y-axis limit for timeline plot
#' @noRd
timeline_upper <- 12

#' Lower y-axis limit for timeline plot
#' @noRd
timeline_lower <- -2.25

#' Y-axis fudge for timeline plot
#' @noRd
y_fudge <- 0.15

#' Timeplot plot color for geochemical evidence of microbial life
#' @noRd
geochemical_evidence_color <- "forestgreen"

#' Timeplot plot color for great oxidation events
#' @noRd
goe_color <- "dodgerblue"

#' Timeplot plot labels for user colors
#' @noR
selected_time_frame_levels <- c("Selected time range", "Other time range")


#' Timeplot bands alpha
#' @noRd
band_alpha <- 0.02


#' Function to build the baseline timeline plot, for any future minerals
#' @return Named list: `plot` is the baseline plot, and `legend` is the band legend
#' @noRd
baseline_timeline <- function()
{
  geotime %>%
    dplyr::mutate(ymin = dplyr::case_when(type == "eon" ~ timeline_lower,
                                          type == "era" ~ timeline_lower + 1), ## Top is at zero now
           ymax = ymin + 1, 
           label_x = (late_age+early_age)/2,
           label_y = (ymin+ymax)/2) %>%
    ggplot2::ggplot() + 
    ggplot2::geom_rect( ggplot2::aes(xmin = late_age, 
                                     xmax = early_age, 
                                     ymin = ymin, 
                                     ymax = ymax), fill = "burlywood3", color = "grey30") +
    ggplot2::geom_text( ggplot2::aes(label = eon_era_name, 
                                     x = label_x, 
                                     y = label_y),
              angle = c(rep(0, 4), rep(90, 3), rep(0, 3), rep(0, 4)),
              size  = c(rep(4, 4), rep(3, 10))) +
    ggplot2::scale_x_reverse(limits = c(4700,0), 
                             position = "top", 
                             breaks = c(seq(0,4500,500), 4700),
                             expand=c(0,100))+ 
    ggplot2::labs(x = "Millions of years ago",
                  y = "Number of minerals discovered") +
    ggplot2::theme(legend.position = "none") -> timeline_base_plot
  
  tibble::tibble(x = c(rep("a", 5), rep("b",5))) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = x, fill = x) + 
    ggplot2::geom_bar(alpha = band_alpha*10, color = "black") +  ## legend alpha seems weaker?!
    ggplot2::scale_fill_manual(values = c(geochemical_evidence_color, goe_color),
                               labels = c("Earliest geochemical evidence of microbial metabolism",
                                          "Great Oxidation Events"),
                               name   = "") -> timeline_base_legend_raw
    #ggplot2::guides(fill = ggplot2::guide_legend(nrow=2))-> timeline_base_legend_raw
  
  timeline_base_legend <- cowplot::get_legend(timeline_base_legend_raw)
  
  return(list("plot"   = timeline_base_plot,
              "legend" = timeline_base_legend))
    
}


prepare_timeline_data <- function(elements_only, age_range, max_age_type)
{
  age_lb <- age_range[1]
  age_ub <- age_range[2]
  if (max_age_type == "Minimum") elements_only %<>% dplyr::mutate(age_check = min_age) 
  if (max_age_type == "Maximum") elements_only %<>% dplyr::mutate(age_check = max_age) 
  
  elements_only %>%
    dplyr::mutate(selected_time_frame = ifelse(age_check >= age_lb & age_check <= age_ub, 
                                               selected_time_frame_levels[[1]], 
                                               selected_time_frame_levels[[2]])) %>%
    dplyr::select(mineral_name, age_check, selected_time_frame) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(age_check)) %>%
    dplyr::rename(x = age_check) %>%
    dplyr::mutate(x = x * 1000, ## age to GA
                  yend = y_fudge + seq(0, 1, 1/(dplyr::n()))[1:dplyr::n()] * timeline_upper, ##  THIS?
                  selected_time_frame = factor(selected_time_frame, levels = selected_time_frame_levels)) 
}

build_current_timeline <- function(timeline_minerals, within_range_color, outside_range_color)
{
  ## Set up breaks -----------------------------------------------------
  total <- nrow(timeline_minerals)
  if (total <= 100){
    break_chunk <- 10
  } else{
    break_chunk <- round((total/9)/50,0) * 50 ## we want 10 breaks (hence 9!) along the y axis, where the labels are at a multiple of 50
  }
  break_labels <- seq(0, total, break_chunk)
  if (max(break_labels) < total) break_labels <- c(break_labels, max(break_labels) + break_chunk)
  break_points <- break_labels * timeline_upper/total

  ## Plot -------------------------------------------------------------
  baseline_timeline()$plot +
    ggplot2::geom_point(data = timeline_minerals, 
                        ggplot2::aes(x = x,
                                     y = yend,
                                     color = selected_time_frame)) +
    ggplot2::geom_segment(data = timeline_minerals, 
                          ggplot2::aes(x = x, xend = x,
                                       y = 0, yend = yend, 
                                       color = selected_time_frame)) +
    
    ggplot2::scale_y_continuous(expand=c(0, y_fudge), 
                                limits=c(timeline_lower, y_fudge + timeline_upper + 0.2),
                                breaks = break_points,
                                labels = break_labels, 
                                position = "right") +
    #ggplot2::theme(axis.line.y = element_blank()) + 
    #ggplot2::geom_segment(x = -4827, xend = -4827, y = 0, yend = Inf) + ## yend will automatically end at limit
    ggplot2::scale_color_manual(values=c(within_range_color, outside_range_color), name = "") +
    ggplot2::geom_hline(yintercept=0) +
    ## GOE 1
    ggplot2::geom_rect(ggplot2::aes(xmin = 2400,  # x axis reverse!
                                    xmax = 2300,
                                    ymin = 0,
                                    ymax = Inf),
                       fill = goe_color, 
                       alpha = band_alpha) + 
    ## GOE 2
    ggplot2::geom_rect(ggplot2::aes(xmin = 630, # x axis reverse!
                                    xmax = 540,
                                    ymin = 0, 
                                    ymax = Inf),
                       fill = goe_color, 
                       alpha = band_alpha) + 
    ## Geochemical evidence of microbial metabolism
    ggplot2::geom_rect(ggplot2::aes(xmin = 3800, # x axis reverse!
                                    xmax = 3400,
                                    ymin = 0,
                                    ymax = Inf),
                       fill = geochemical_evidence_color, 
                       alpha = band_alpha) -> final_nolegend

  cowplot::plot_grid(final_nolegend, baseline_timeline()$legend, nrow = 2, rel_heights=c(1, 0.1))
  
}
