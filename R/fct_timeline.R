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
#' @noRd
selected_age_range_levels <- c("Selected time range", "Other time range")

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
                                     ymax = ymax,
                                     fill = early_age), color = "black") +
    ggplot2::geom_text( ggplot2::aes(label = eon_era_name, 
                                     x = label_x, 
                                     y = label_y),
              angle = c(rep(0, 4), rep(90, 3), rep(0, 3), rep(0, 4)),
              fontface = c(rep("bold", 4), rep("plain", 10)),
              size  = c(rep(6, 4), rep(3, 3), rep(3.5, 7))) +
    colorspace::scale_fill_continuous_sequential(h1 = 57, c1 = 97, c2 = NA, cmax = NA, l1 = 35, l2 = 98, p1=1.1) +
    ggplot2::scale_x_reverse(limits = c(4700,0), 
                             position = "top", 
                             breaks = c(seq(0,4500,500), 4700),
                             expand=c(0,100))+ 
    ggplot2::labs(x = "Millions of years ago",
                  y = "Number of minerals discovered") +
    ggplot2::guides(fill = FALSE)-> timeline_base_plot

  tibble::tibble(x = c(rep("a", 5), rep("b",5))) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = x, fill = x) + 
    ggplot2::geom_bar(alpha = band_alpha*10, color = "black") +  ## legend alpha seems weaker so *10.
    ggplot2::scale_fill_manual(values = c(geochemical_evidence_color, goe_color),
                               labels = c("Geochemical evidence of early microbial metabolism",
                                          "Oxygenation Events"),
                               name   = "") + 
    ggplot2::guides(fill = ggplot2::guide_legend(nrow=2)) + 
    ggplot2::theme(legend.text = ggplot2::element_text(size=12),
                   legend.key.size = ggplot2::unit(0.85, "cm"))-> timeline_base_legend_raw
  
  timeline_base_legend <- cowplot::get_legend(timeline_base_legend_raw)
  
  return(list("plot"   = timeline_base_plot,
              "legend" = timeline_base_legend))
    
}

#' Function to prepare data for use in timeline plot
#' 
#' @param df Mineral data to plot
#' @param age_range Array of ages (low, high) of minerals in the selected age range
#' @param max_age_type "Maximum" or "Minimum" indicating how to select minerals in age range
#' 
#' @return Named list of two tibbles: `all` is minerals to plot from all localities plot, and `maxage` is the minerals to plot ONLY at their max age
#' @noRd
prepare_timeline_data <- function(df, age_range, max_age_type)
{

  age_lb <- min(age_range)
  age_ub <- max(age_range)
  if (max_age_type == "Minimum") df %<>% dplyr::mutate(age_check = min_age) 
  if (max_age_type == "Maximum") df %<>% dplyr::mutate(age_check = max_age) 
  
  df %>%
    dplyr::mutate(selected_age_range = ifelse(age_check >= age_lb & age_check <= age_ub, 
                                              selected_age_range_levels[[1]], 
                                              selected_age_range_levels[[2]])) %>%
    dplyr::select(mineral_name, age_check, selected_age_range) %>%
    dplyr::ungroup() %>%
    dplyr::rename(x = age_check) %>%
    dplyr::mutate(x = x * 1000, ## age to GA
                  selected_age_range = factor(selected_age_range, levels = selected_age_range_levels)) %>%
    dplyr::distinct() -> all_minerals
  
  all_minerals %>%
    dplyr::group_by(mineral_name) %>%
    dplyr::filter(x == max(x)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() -> oldest_minerals
  
  return(list("all" = all_minerals, 
              "maxage" = oldest_minerals))
}



#' Add "GOE1", "GOE2", and microbial metabolism bands to timeline plot
#' 
#' @param p ggplot that these bands should be added to
#' @return ggplot containing bands
#' @noRd
add_timeline_events <- function(p)
{
  p + 
    # GOE 1: Warke et al 2020 says 2.501 - 2.3 ga
    ggplot2::geom_rect(ggplot2::aes(xmin = 2501,  # x axis reverse!
                                    xmax = 2300,
                                    ymin = 0,
                                    ymax = Inf),
                       fill = goe_color, 
                       alpha = band_alpha) + 
      ## GOE 2: from scott at al 2008 0.66-0.55 ga
      ggplot2::geom_rect(ggplot2::aes(xmin = 660, # x axis reverse!
                                      xmax = 550,
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
                         alpha = band_alpha)
}

#' Build final timeline plot for display
#' 
#' @param timeline_minerals Tibble of minerals to plot created by `prepare_timeline_data()`
#' @param nodes Tibble of network nodes
#' @param mineral_color_by Which variable should we colors the nodes in the selected age range, or "singlecolor"
#' @param mineral_color_palette Either a hex color or RColorBrewer palette string to use to color the nodes in the selected age range
#' @param outside_range_color Color to use for minerals outside age range
#' @return timeline plot with associated legend(s)
#' @noRd
build_current_timeline <- function(timeline_minerals, nodes, mineral_color_by, mineral_color_palette, outside_range_color)
{
  color_by <- as.symbol(mineral_color_by)

  ## Merge timeline_minerals with node information, and add yend --------------
  total <- nrow(timeline_minerals)
  nodes %>%
    dplyr::filter(group == "mineral") %>%
    dplyr::select(id, mean_pauling, w_mean_pauling, cov_pauling, w_cov_pauling, num_localities, max_age) %>%
    dplyr::rename(mineral_name = id) %>%
    dplyr::right_join(timeline_minerals) %>% 
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(x)) %>%
    dplyr::mutate(yend = y_fudge + (seq(0, 1, (1/total))[1:total])  * timeline_upper) -> timeline_minerals_ready
  stopifnot(nrow(timeline_minerals_ready) == total)

  ## Set up breaks -----------------------------------------------------------
  if (total <= 100){
    break_chunk <- 10
  } else if (total > 100 & total < 250) {
    break_chunk <- 25
  } else {
    # must be >= 250
    break_chunk <- round((total/9)/50,0) * 50 ## we want 10 breaks (hence 9!) along the y axis, where the labels are at a multiple of 50
  }
  break_labels <- seq(0, total, break_chunk)
  if (max(break_labels) < total) break_labels <- c(break_labels, max(break_labels) + break_chunk)
  break_points <- break_labels * timeline_upper/total

  ## Plot ------------------------------------------------------------------
  timeline_minerals_ready %>% dplyr::filter(selected_age_range == selected_age_range_levels[1]) -> timeline_minerals_inside
  timeline_minerals_ready %>% dplyr::filter(selected_age_range == selected_age_range_levels[2]) -> timeline_minerals_outside

  ######### Set the baseline ######  
  baseline_timeline()$plot +
    ggplot2::scale_y_continuous(expand=c(0, y_fudge), 
                                limits=c(timeline_lower, y_fudge + max(break_points) + 0.2),
                                breaks = break_points,
                                labels = break_labels, 
                                position = "right")  -> raw_plot
  
  ##### Add in outside minerals, if any ######
  if (nrow(timeline_minerals_outside) > 0)
  {
    raw_plot + 
      ggplot2::geom_point(data = timeline_minerals_outside, 
                          color = outside_range_color, 
                          ggplot2::aes(x = x,
                                       y = yend)) +
      ggplot2::geom_segment(data = timeline_minerals_outside, 
                            color = outside_range_color, 
                            ggplot2::aes(x = x, xend = x,
                                         y = 0, yend = yend)) -> raw_plot
  }
  
  
  ### To avoid very irritating warnings, we need to entirely set inside range colors within ifs:
  if (color_by == "singlecolor") {
    raw_plot_colored <- raw_plot + 
                          ggplot2::geom_point(data  = timeline_minerals_inside, 
                                              color = mineral_color_palette,
                                              ggplot2::aes(x = x, y = yend)) +
                          ggplot2::geom_segment(data  = timeline_minerals_inside, 
                                                color = mineral_color_palette,
                                                ggplot2::aes(x = x, xend = x, y = 0, yend = yend))
    legend_grid <- cowplot::plot_grid(baseline_timeline()$legend)
  } else {
    
    rev_guide <- FALSE
    direction <- -1
    if (color_by == "max_age"){
      direction <- 1
      rev_guide <- TRUE
    }
    
    raw_plot_colored <- raw_plot + 
      ggplot2::geom_point(data  = timeline_minerals_inside, 
                          ggplot2::aes(x = x,
                                       y = yend,
                                       color = {{color_by}})) +
      ggplot2::geom_segment(data  = timeline_minerals_inside, 
                            ggplot2::aes(x = x, xend = x, y = 0, yend = yend,
                                         color = {{color_by}})) +
      ggplot2::scale_color_distiller(palette   = mineral_color_palette, 
                                     name      = variable_to_title[[color_by]],
                                     direction = direction) + ## TODO: do we need an na_value?
      ggplot2::theme(legend.position = "bottom") + 
      ggplot2::guides(colour = ggplot2::guide_colourbar(reverse = rev_guide,
                                                        barheight = ggplot2::unit(1, "cm"),
                                                        title.position="top", 
                                                        frame.colour = "black", 
                                                        ticks.colour = "black"))
    
      
      legend_grid <- cowplot::plot_grid(baseline_timeline()$legend, 
                                        cowplot::get_legend(raw_plot_colored), 
                                        nrow = 1, scale = 0.8)
    
    
  }
    
  ## Add GOEs and metabolism bars as well as hline
  final_plot <- add_timeline_events(raw_plot_colored) + ggplot2::geom_hline(yintercept=0) + ggplot2::theme(legend.position = "none")

  cowplot::plot_grid(final_plot, legend_grid, nrow = 2, rel_heights=c(1, 0.1), scale=c(1, 0.9))
}
