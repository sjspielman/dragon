
prepare_timeline_plot <- function()
{
  
  ## Prepare timeline data -----------------------------------------
  geo_timeline %>% 
    dplyr::select(level, interval_name, late_age, early_age) %>%
    dplyr::filter(level <= 2) %>%
    dplyr::mutate(level = level + 0.5) %>%
    dplyr::rename(ymin = level) %>%
    dplyr::mutate(ymin = ymin + 1,
                  ymax = ymin + 1,
                  label_x = (late_age+early_age)/2, 
                  label_y = (ymin+ymax)/2)  -> geo_data
  
  geo_data %>% 
    dplyr::group_by(ymin) %>% 
    dplyr::tally() %>% 
    dplyr::pull(n) -> level_ncat
  
  geo_data %>% 
    dplyr::arrange(ymin, early_age) %>% 
    dplyr::mutate(interval_name = factor(interval_name)) -> interval_name_levels
  
  
  ## Plot timeline data for *baseline* -----------------------------------------------
  all_geo_colors <- c( colorRampPalette(RColorBrewer::brewer.pal(9,"BuPu"))(level_ncat[1]),
                       colorRampPalette(RColorBrewer::brewer.pal(9,"BuPu"))(level_ncat[2])
  ) 
  timeline_upper <- 12
  timeline_space <- 6
  
  geo_data %>%
    ggplot2::ggplot() +
    ggplot2::xlab("Millions of years ago") +
    ggplot2::ylab("") + 
    ggplot2::geom_rect(ggplot2::aes(fill = interval_name, 
                                    xmin = late_age, 
                                    xmax = early_age, 
                                    ymin = ymin, 
                                    ymax = ymax), 
                       color = "black") +
    ggplot2::scale_fill_manual(values = all_geo_colors) + 
    ggplot2::geom_text(ggplot2::aes(x = label_x, y = label_y, label = interval_name), 
                       angle = c( 0, 90, 60, 35, rep(0, 5), rep(30, 3), rep(0, 2)),
                       color = c( rep("black", 12), "grey80", "grey80"), 
                       size  = c( 5.5, #phanerozoic
                                  3, # cenozoic
                                  3.5,  #mesozoic
                                  4,  #paleozoic
                                  7,   #proterozoic
                                  3.5,  #Neoproterozoic
                                  4,  #Mesoproterozoic
                                  6,  # Paleoproterozoic
                                  7,  #Archean
                                  3, #Neoarchean
                                  3.5, #Mesoarchean
                                  3.5, #Paleoarchean
                                  4.5, # Eoarchean
                                  7)  ) + #hadean 
    ggplot2::annotate("text", 
                      label = stringr::str_wrap("Geochemical evidence of microbial metabolism", width=30),
                      color = "dodgerblue4", 
                      size = 5, 
                      x = 3600, y = 1.5) +
    ggplot2::geom_point(x = -3800, y = 2, size = 3, color = "dodgerblue4") + 
    ggplot2::geom_point(x = -3400, y = 2, size = 3, color = "dodgerblue4") +                
    ggplot2::geom_segment(color = "dodgerblue4", x = -3800, xend = -3400, y=2, yend=2, size=1.5) + 
    ggplot2::annotate("text", 
                      label = stringr::str_wrap("First Great Oxidation Event", width=20),
                      color = "dodgerblue4", 
                      size = 5, 
                      x = 2350, y = 1.5) +
    ggplot2::geom_point(x = -2400, y = 2, size = 3, color = "dodgerblue4") + 
    ggplot2::geom_point(x = -2300, y = 2, size = 3, color = "dodgerblue4") +                
    ggplot2::geom_segment(color = "dodgerblue4", x = -2400, xend = -2300, y=2, yend=2, size=1.5) + 
    ggplot2::annotate("text", 
                      label = stringr::str_wrap("Second Great Oxidation Event", width=20),
                      color = "dodgerblue4", 
                      size = 5, 
                      x = 800, y = 1.5) +
    ggplot2::geom_point(x = -630, y = 2, size = 3, color = "dodgerblue4") + 
    ggplot2::geom_point(x = -540, y = 2, size = 3, color = "dodgerblue4") +                
    ggplot2::geom_segment(color = "dodgerblue4", x = -630, xend = -540, y=2, yend=2, size=1.5) +         
    ggplot2::geom_point(data = extinctions, ggplot2::aes(x = x, y = y), color = "firebrick", size=2)+ 
    ggplot2::geom_text(data = extinctions, ggplot2::aes(x = x-30, y = y, label = name), color = "firebrick", size = 4, hjust=0)+
    ggplot2::scale_y_continuous(limits=c(0, timeline_upper), expand=c(0,0)) +
    ggplot2::scale_x_reverse(breaks=c(seq(0, 4500,500)), 
                             limits=c(4750, -200), 
                             sec.axis = ggplot2::dup_axis()) +
    ggplot2::theme_classic() + 
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 13),
                   axis.title.x = ggplot2::element_text(size=15),
                   legend.position = "none") 
}

