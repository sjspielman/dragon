

#' Point size used for legend keys
geom.point.size <- 8

#' Theme for legend key items built on top of cowplot
theme_dragon <- ggplot2::theme_set(cowplot::theme_cowplot() + 
                     ggplot2::theme(legend.position       = "bottom",
                                    legend.text           = ggplot2::element_text(size=11),
                                    legend.key.size       = ggplot2::unit(1, "cm"),
                                    legend.title          = ggplot2::element_text(size=13),
                                    legend.box.background = ggplot2::element_rect(color = "white")))                                  

  
set_cluster_colors <- function(cluster_palette, n_clusters)
{
  
  full_palette <- RColorBrewer::brewer.pal(8, cluster_palette)
  if(n_clusters <= 8){    
    cluster_colors <- full_palette[1:n_clusters]
  } else {
    cluster_colors <- grDevices::colorRampPalette(full_palette)(n_clusters)
  }
  
  return(cluster_colors)
  
}


obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, na_color, discrete_colors = NA)
{
  ## variable type:
  ## "d" = discrete, ordinal. THERE ARE NO NOMINAL EXCEPT FOR CLUSTER, WHICH IS HANDLED DIFFERENTLY.
  ## "c" = continuous
  
  cvar <- as.symbol(color_variable)
  dat %>% dplyr::mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.
  legendtitle <- variable_to_title[[color_variable]]
  
  
  if (variable_type == "d")
  {
    p <- ggplot2::ggplot(dat2) + 
      ggplot2::aes(x = x, y = factor(!!cvar), color = factor(!!cvar)) + 
      ggplot2::geom_point(size = geom.point.size) + 
      ggplot2::guides(colour = ggplot2::guide_legend(title.position="top",  title.hjust = 0.5, byrow=TRUE, nrow=2)  ) +
      ggplot2::theme(legend.key.size = ggplot2::unit(0.05, 'lines'), legend.title = ggplot2::element_text(size = ggplot2::rel(0.8)))
    if (color_variable == "cluster_ID") 
    {   ## cluster, colors already given
      p <- p + ggplot2::scale_color_manual(name = legendtitle, na.value = na_color, values = discrete_colors)
    } else {
      p <- p + ggplot2::scale_color_brewer(palette = palettename, name = legendtitle, na.value = na_color)
    } 
  }
  
  if (variable_type == "c")
  {
    p <- ggplot2::ggplot(dat2) + 
      ggplot2::aes(x = x, y = !!cvar, color = !!cvar) + 
      ggplot2::geom_point(size = geom.point.size) + 
      ## TODO: KEEP OR KILL DIRECTION=-1?
      ggplot2::scale_color_distiller(name = legendtitle, palette = palettename, direction = -1, na.value = na_color) + 
      ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5, frame.colour = "black", ticks.colour = "black"), 
             size = ggplot2::guide_legend(title.position="top", title.hjust = 0.5)) +
      ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)))
  }

  ggplot2::ggplot_build(p)$data[[1]] %>% 
    tibble::as_tibble() %>% 
    dplyr::bind_cols(dat2) %>%
    ## god help me, hadley.
    dplyr::rename(color = colour) -> data_colors  
  data_legend <- cowplot::get_legend(p)
  return (list("cols" = data_colors, "leg" = data_legend))
}





obtain_colors_legend_single <- function(group, singleshape, singlecolor, na_color)
{
  p <- tibble::tibble(x = 1, y = 1, type = group) %>% 
    ggplot2::ggplot() + 
      ggplot2::aes(x=x,y=y,color=type) + 
      ggplot2::geom_point(size = geom.point.size, shape = singleshape) + 
      ggplot2::scale_color_manual(name = "", values=c(singlecolor), na.value=na_color) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size=16))
  data_colors <- ggplot2::ggplot_build(p)$data[[1]]$colour
  data_legend <- cowplot::get_legend(p)
  return (list("cols" = data_colors, "leg" = data_legend))
}






obtain_node_sizes <- function(dat, size_variable, lowsize, highsize, size_scale = 1)
{
  
  svar <- as.symbol(size_variable)
  
  dat %>% dplyr::mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.
  p <- ggplot2::ggplot(dat2) + 
        ggplot2::aes(x = label, y = !!svar, size = !!svar) +
        ggplot2::geom_point() + 
        ggplot2::scale_size(range = c(lowsize,highsize))
  
  # only returning this, no legend since size. 
  ## TODO: Need to have a size legend for the PDF output. that's going to be gnarly as all hell
  ggplot2::ggplot_build(p)$data[[1]] %>% 
    tibble::as_tibble() %>% 
    dplyr::bind_cols(dat2) %>%
    dplyr::mutate(size = size * size_scale)
}














