#####################################################################
#### Creates all the <palette>.png files for use in dragon menus ####
#####################################################################
library(RColorBrewer)
library(ggplot2)
library(dplyr)

RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) -> palettes_of_interest

for (this_pal in palettes_of_interest$name){
  if (palettes_of_interest$category[palettes_of_interest$name == this_pal] == "qual")
  { direction = 1
  } else {
    direction = -1
  }

  tibble::tibble(x = 1:6, y = rep(1,6)/10) %>% 
    ggplot2::ggplot(aes(x,y, fill = factor(x))) + 
    ggplot2::geom_point(size=3.5, pch=21, color="black") + 
    ggplot2::scale_fill_brewer(palette=this_pal, name = "", direction = direction)+
    ggplot2::guides(fill = ggplot2::guide_legend(nrow=1)) + 
    ggplot2::ylim(c(0.05, 0.15)) + 
    ggplot2::xlim(c(0.75,6.25)) + 
    ggplot2::theme_void() + 
    ggplot2::theme(legend.position = "none") -> p
  ggplot2::ggsave(paste0(this_pal, ".png"), p, width=0.8, height=0.15)

}

