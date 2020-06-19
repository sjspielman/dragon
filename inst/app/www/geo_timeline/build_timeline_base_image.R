library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(RColorBrewer)


geotime <- tibble::tribble(
~type,  ~name,              ~early_age,~late_age,
"eon",  "Phanerozoic",      541,0,
"eon",  "Proterozoic",      2500,541,
"eon",  "Archean",          4000,2500,
"eon",  "Hadean",           4600,4000,
"era",  "Cenozoic",         66,0,
"era",  "Mesozoic",         252,66,
"era",  "Paleozoic",        541,252,
"era",  "Neoproterozoic",   1000,541,
"era",  "Mesoproterozoic",  1600,1000,
"era",  "Paleoproterozoic", 2500,1600,
"era",  "Neoproterozoic",       2800,2500,
"era",  "Mesoarchaen",      3200,2800,
"era",  "Paleoarchaen",     3600,3200,
"era",  "Eoarchaen",        4000,3600)



#Eoarchean = Eo; Paleoarchean = Paleo; Mesoarchean = Meso; Neoarchean = Neo
#Paleoproterozoic = Paleo; Mesoproterozoic = Meso; Neoproterozoic = Neo
#Paleozoic = Paleo; Mesozoic = Meso; Cenozoic = Ceno

timeline_upper <- 100 # total number of minerals 
geotime %>%
  mutate(ymin = case_when(type == "eon" ~ -2,
                          type == "era" ~ -1), ## Top is at zero now
         ymax = ymin + 1, 
         label_x = (late_age+early_age)/2,
         label_y = (ymin+ymax)/2) %>%
  ggplot() + 
    geom_rect(aes(fill = type,
                  xmin = late_age, 
                  xmax = early_age, 
                  ymin = ymin, 
                  ymax = ymax), 
                  color = "black") +
    geom_text(aes(label = name, x = label_x, y = label_y),
              angle = c(rep(0, 4), rep(90, 3), rep(90, 3), rep(90, 4)),
              size  = c(rep(4, 4), rep(2, 10))) +
    scale_x_reverse(sec.axis = dup_axis()) #+
    
    
#    geotime$name
# [1] "Phanerozoic" "Proterozoic" "Archean"     "Hadean"      "Cz"         
# [6] "Mz"          "Pz"          "Neo"         "Meso"        "Paleo"      
#[11] "Neo"         "Meso"        "Paleo"       "Eo"         

  #  scale_y_continuous(limits=c(-2, timeline_upper))# +
   # scale_x_reverse(breaks=c(seq(0, 4500,500)), limits=c(4750, -200), sec.axis = dup_axis())
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

# level_ncat is 2 

geo_data %>% 
    arrange(ymin, early_age) %>% 
    pull(interval_name) -> interval_name_levels
geo_data$interval_name <- factor(geo_data$interval_name, interval_name_levels)
all_geo_colors <- c( colorRampPalette(brewer.pal(9,"BuPu"))(level_ncat[1]),
                 colorRampPalette(brewer.pal(9,"BuPu"))(level_ncat[2])) #,
                # colorRampPalette(brewer.pal(9,"Greens"))(level_ncat[3]))

timeline_upper <- 12
timeline_space <- 6
    
geo_data %>%
    ggplot() +
    xlab("Millions of years ago") +
    ylab("") + 
    geom_rect(aes(fill = interval_name, 
                  xmin = late_age, 
                  xmax = early_age, 
                  ymin = ymin, 
                  ymax = ymax), 
              color = "black") +
    scale_fill_manual(values = all_geo_colors) + 
    geom_text(aes(x = label_x, y = label_y, label = interval_name), 
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
    annotate("text", 
                label = str_wrap("Geochemical evidence of microbial metabolism", width=30),
                color = "dodgerblue4", 
                size = 5, 
                x = 3600, y = 1.5) +
    geom_point(x = -3800, y = 2, size = 3, color = "dodgerblue4") + 
    geom_point(x = -3400, y = 2, size = 3, color = "dodgerblue4") +                
    geom_segment(color = "dodgerblue4", x = -3800, xend = -3400, y=2, yend=2, size=1.5) + 
    annotate("text", 
                label = str_wrap("First Great Oxidation Event", width=20),
                color = "dodgerblue4", 
                size = 5, 
                x = 2350, y = 1.5) +
    geom_point(x = -2400, y = 2, size = 3, color = "dodgerblue4") + 
    geom_point(x = -2300, y = 2, size = 3, color = "dodgerblue4") +                
    geom_segment(color = "dodgerblue4", x = -2400, xend = -2300, y=2, yend=2, size=1.5) + 
    annotate("text", 
                label = str_wrap("Second Great Oxidation Event", width=20),
                color = "dodgerblue4", 
                size = 5, 
                x = 800, y = 1.5) +
    geom_point(x = -630, y = 2, size = 3, color = "dodgerblue4") + 
    geom_point(x = -540, y = 2, size = 3, color = "dodgerblue4") +                
    geom_segment(color = "dodgerblue4", x = -630, xend = -540, y=2, yend=2, size=1.5) +         
    geom_point(data = extinctions, aes(x = x, y = y), color = "firebrick", size=2)+ 
    geom_text(data = extinctions, aes(x = x-30, y = y, label = name), color = "firebrick", size = 4, hjust=0)+
    theme_classic() + 
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.title.x = element_text(size=15),
          legend.position = "none") + 
          #legend.position = "top", 
          #legend.text = element_text(size =14)) +
    #guides(fill=FALSE, 
    #       color = guide_legend(override.aes = list(size=4))) + 
    scale_y_continuous(limits=c(0, timeline_upper), expand=c(0,0)) +
    scale_x_reverse(breaks=c(seq(0, 4500,500)), limits=c(4750, -200), sec.axis = dup_axis()) -> timeline_plot_base



build_timeline_plot <- function(elements_only_minerals, age_lb, age_ub, max_age_type, selected_color, notselected_color)
{
    
   if (max_age_type == "Minimum")
    {
        elements_only_minerals %<>% dplyr::mutate(age_check = min_age) 
    } else {
        elements_only_minerals %<>% dplyr::mutate(age_check = max_age) 
    }
    elements_only_minerals %>%
        mutate( selected_time_frame = ifelse(age_check >= age_lb & age_check <= age_ub, "Selected time range", "Other time range")) %>%
        dplyr::select(mineral_name, age_check, selected_time_frame) %>%
        ungroup() %>%
        arrange(desc(age_check)) %>%
        rename(x = age_check) %>%
        mutate(x = x * 1000,
               y = (timeline_upper-0.25) - (seq(0, 1, 1/(n()))[1:n()] * timeline_space)) -> timeline_minerals
    timeline_minerals$selected_time_frame <- factor(timeline_minerals$selected_time_frame, levels = c("Selected time range", "Other time range"))


    timeline_plot <- timeline_plot_base +
                        geom_point(data = timeline_minerals, aes(x = x, y = y, color = selected_time_frame)) +
                        geom_segment(data = timeline_minerals, aes(x = x, xend = x, y = y, yend = timeline_upper, color = selected_time_frame)) +
                        scale_color_manual(values=c(selected_color, notselected_color), name = "")
    
    timeline_plot
}
