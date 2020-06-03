#####################################################################
#### Creates all the <palette>.png files for use in dragon menus ####
#####################################################################
library(RColorBrewer)
library(ggplot2)
library(cowplot)

RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) -> palettes_of_interest

for (this_pal in palettes_of_interest$name){
  tibble::tibble(x = 1:7, y = rep(1,7)) %>% 
    ggplot2::ggplot(aes(x,y, fill = factor(x))) + 
    ggplot2::geom_point(size=12, pch=21, color="black") + 
    ggplot2::scale_fill_brewer(palette=this_pal, name = "")+
    ggplot2::guides(fill = ggplot2::guide_legend(nrow=1)) + 
    ggplot2::theme(legend.text = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.1, "cm"),
                   legend.spacing.x = ggplot2::unit(0.1, 'cm')) -> p
  cowplot::get_legend(p) -> l
  cowplot::save_plot(paste0(this_pal, ".png"), l, base_height = 0.4, base_width = 3.64)
}