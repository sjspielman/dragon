#' Add visual styles to edges for display in the Shiny App.
#' 
#' @param edges A tibble containing network edges
#' @param edge_options   A named list of all network styles to be applied
#' 
#' @returns Named list containing a legend to display for edges ('edge_legend') if needed, and an updated edge tibble with colors incorporated ('styled_edges')
#' @noRd
style_edges <- function(edges, edge_options){
  
  colorlegend_edge <- FALSE
  if (edge_options$edge_color_by == "singlecolor") 
  {
    edge_colors <- edges %>% 
      dplyr::mutate(color = edge_options$edge_color)             
  } else {
    out <- obtain_dynamic_colors_legend(edges, 
                                        edge_options$edge_color_by, 
                                        "c", 
                                        edge_options$edge_palette,
                                        edge_options$na_color)
    
    edge_colors <-  dplyr::mutate(edges, color = out$colors)
    colorlegend_edge <- out$leg
  }

  return(
    list("edge_legend" = colorlegend_edge, "styled_edges" = edge_colors)
  )
}


#' Add visual styles to nodes for display in the Shiny App.
#' 
#' @param nodes A tibble containing network nodes
#' @param style_options   A named list of all network styles to be applied
#' 
#' @returns Named list containing legend(s) to display for nodes ('both_legend' used for cluster coloring, 'element_legend' used for element nodes alone, and 'mineral_legend' used for mineral nodes alone), and an updated node tibble with styles incorporated ('styled_nodes')
#' @noRd
style_nodes <- function(nodes, style_options)
{
  
  node_attr <- list()
  
  ## Assigns node colors, shapes, and exports legend --------------------------------------
  colors_legend <- style_nodes_colors_legend(nodes, style_options)
  
  # Assign legend(s) to node_attr list
  node_attr[["both_legend"]]    <- colors_legend$both_legend
  node_attr[["element_legend"]] <- colors_legend$element_legend   
  node_attr[["mineral_legend"]] <- colors_legend$mineral_legend

  
  ## Assigns node sizes -------------------------------------------------------------------
  sizes <- style_nodes_sizes(nodes, style_options) ## Returns a tibble

  ## Merge size and color specifications ---------------------------------------------
  nodes %>% 
    dplyr::left_join( colors_legend$colors ) %>%
    dplyr::left_join(sizes) -> node_attr[["styled_nodes"]]
  
  ## Shape, highlight, and label styles ------------------------------------------------
  node_attr[["styled_nodes"]] <- style_nodes_shape_highlight_label(node_attr[["styled_nodes"]], style_options)

  ## Lighten and darken colors appropriately
  node_attr[["styled_nodes"]] %<>% 
    dplyr::mutate(color.border           = colorspace::darken(color.background,  lighten_darken_factor),
                  color.highlight        = colorspace::lighten(color.background, lighten_darken_factor),
                  color.hover.border     = colorspace::darken(color.background,  lighten_darken_factor),
                  color.hover.background = colorspace::lighten(color.background, lighten_darken_factor)) %>%
    dplyr::arrange(dplyr::desc(group)) ## arranging minerals first is necessary so that element nodes are always on top and not obscured by giant minerally networks; https://github.com/spielmanlab/dragon/issues/5
  
  
  return(node_attr)
}





#' Determine and apply node colors for display in the Shiny App.
#' 
#' @param full_nodes A tibble containing network nodes
#' @param style_options   A named list of all network styles to be applied
#' 
#' @returns Named list containing legend(s) to display for nodes ('both_legend' used for cluster coloring, 'element_legend' used for element nodes alone, and 'mineral_legend' used for mineral nodes alone), and a tibble of colors to apply to nodes ('colors')
#' @noRd
style_nodes_colors_legend <- function(full_nodes, style_options)
{

  legend_list <- list()
  # these are not always made and must be FALSE if not made
  legend_list$both_legend    <- FALSE
  legend_list$element_legend <- FALSE
  legend_list$mineral_legend <- FALSE
  
  ## Color *all* nodes by cluster ----------------------------------------------------------------
  if (style_options$color_by_cluster) 
  {        
    out_cluster <- obtain_dynamic_colors_legend(full_nodes, 
                                               "cluster_ID", 
                                               "d", 
                                               style_options$cluster_colors, 
                                               style_options$na_color)
    legend_list[["both_legend"]] <- out_cluster$legend
    full_nodes %>%
      dplyr::select(id, group) %>%
      dplyr::mutate(color.background = out_cluster$colors) -> final_colors

  } else 
  { 
    element_nodes <- full_nodes %>% dplyr::filter(group == "element")
    mineral_nodes <- full_nodes %>% dplyr::filter(group == "mineral")
    raw_element_colors <- rep(style_options$element_color, nrow(element_nodes))
    raw_mineral_colors <- style_options$mineral_color  #rep(style_options$mineral_color, nrow(mineral_nodes))
                                                            
    ## Color minerals  -----------------------------------------------------------
    if (style_options$mineral_color_by == "singlecolor")
    {
      mineral_out  <- obtain_legend_singlecolor(style_options$mineral_color, style_options$mineral_shape, "Mineral")
    } else
    {  
      mineral_out <- obtain_dynamic_colors_legend(mineral_nodes, 
                                                  style_options$mineral_color_by, 
                                                  "c", 
                                                  style_options$mineral_palette,
                                                  style_options$na_color)
      raw_mineral_colors <- mineral_out$colors 
    } 
    
    ## Color elements  ------------------------------------------------------------
    if (style_options$element_color_by == "singlecolor")
    {
      element_color <- style_options$element_color
     # if (style_options$element_shape == "text") element_color <- style_options$element_label_color
      
      element_out <- obtain_legend_singlecolor(element_color, style_options$element_shape, "Element")
    }  else
    {
      element_out  <- obtain_dynamic_colors_legend(element_nodes,
                                                   style_options$element_color_by, 
                                                   ifelse(style_options$element_color_by %in% ordinal_color_variables, "d", "c"),
                                                   style_options$element_palette,
                                                   style_options$na_color) 
      raw_element_colors <- element_out$colors
    }   

    ## Add legends to legend_list ---------------------------------
    legend_list[["mineral_legend"]] <- mineral_out$legend
    legend_list[["element_legend"]] <- element_out$legend
    
    
    ## Add node colors to tibbles ---------------------------------
    mineral_nodes %>% 
      dplyr::select(id, group) %>%
      dplyr::mutate(color.background = raw_mineral_colors) -> mineral_node_colors
    element_nodes %>% 
      dplyr::select(id, group) %>%
      dplyr::mutate(color.background = raw_element_colors) %>%
      dplyr::bind_rows(mineral_node_colors) -> final_colors  ## names: id, group, color.background

  }   
  # Some sanity on colors 
  stopifnot(nrow(final_colors) == nrow(full_nodes))
  
  return( list("colors" = final_colors, ## id, group, color.background
               "both_legend" = legend_list$both_legend,
               "element_legend" = legend_list$element_legend,
               "mineral_legend" = legend_list$mineral_legend
               )
        )  
}




#' Determine node sizes for display in the Shiny App.
#' 
#' @param full_nodes    A tibble containing network nodes
#' @param style_options A named list of all network styles to be applied
#' 
#' @returns Tibble with names: id, size, group, font.size
#' @noRd
style_nodes_sizes <- function(full_nodes, style_options)
{
  element_nodes <- full_nodes %>% dplyr::filter(group == "element")
  mineral_nodes <- full_nodes %>% dplyr::filter(group == "mineral")
  ## ELEMENT SIZES -----------------------------------------------------------
  if (style_options$element_size_by != "singlesize") 
  {
    obtain_node_sizes(element_nodes, 
                      style_options$element_size_by, 
                      element_size_min, 
                      element_size_max, 
                      size_scale = (style_options$element_size_scale / element_size_scale_divisor)) -> element_sizes
    
  } else
  {
    element_sizes <- style_options$element_label_size ## LABEL SIZE
  }                     
  
  ## MINERAL SIZES -----------------------------------------------------------
  if (style_options$mineral_size_by != "singlesize") {
    obtain_node_sizes(mineral_nodes, 
                      style_options$mineral_size_by, 
                      mineral_size_min, 
                      mineral_size_max, 
                      size_scale = (style_options$mineral_size_scale / mineral_size_scale_divisor)) -> mineral_sizes
    
  } else 
  {
    mineral_sizes <- style_options$mineral_size ## REGULAR SIZE
  }        

  ## Add node sizes to tibbles ---------------------------------
  mineral_nodes %>% 
    dplyr::mutate(size      = mineral_sizes,
                  font.size = style_options$mineral_label_size) -> mineral_nodes_size
  
  element_nodes %>% 
    dplyr::mutate(size      = element_sizes, # Unused but hey why not
                  font.size = size) %>%
    dplyr::bind_rows(mineral_nodes_size) %>%
    dplyr::select(id, group, size, font.size)
}






#' Assign node shapes and colors associated with highlighted and/selected elements for display in the Shiny App.
#' 
#' @param node_attr_styled_nodes A tibble containing network nodes
#' @param style_options   A named list of all network styles to be applied
#' 
#' @returns Updated tibble of nodes containing new styling
#' @noRd
style_nodes_shape_highlight_label <- function(node_attr_styled_nodes, style_options)
{
  focal_element_names       <- element_info$element_name[element_info$element %in% style_options$elements_of_interest]

  #print(names(names(style_options$custom_element_colors)))
  #print(names(style_options$custom_element_colors))

  node_attr_styled_nodes %>%
    dplyr::mutate(shape = ifelse(group == "element", style_options$element_shape, style_options$mineral_shape),
                  ## Node color for focal elements if highlight is T
                  color.background = ifelse(element_name %in% focal_element_names & style_options$highlight_element,
                                            style_options$highlight_color, 
                                            color.background),
                  ## Node color for custom selections if specified  
                  color.background = ifelse(id %in% names(style_options$custom_element_colors), 
                                            style_options$custom_element_colors[id], 
                                            color.background), 
                  ## Element font color                                                                                       
                  font.color = ifelse(group == "element", style_options$element_label_color, style_options$mineral_label_color),
                  ## Update font color to equal background color if text shape
                  font.color = ifelse(group == "element" & style_options$element_shape == "text",
                                      color.background, 
                                      font.color))                        
}



## Functions to obtain the colors/sizes/shapes with appropriate legend ------------------------------------------------


#' Point size used for legend keys
#' @noRd
geom.point.size <- 10

#' Theme for legend key items built on top of cowplot
#' @noRd
theme_dragon <- ggplot2::theme_set(cowplot::theme_cowplot() + 
                                     ggplot2::theme(legend.position       = "bottom",
                                                    legend.text           = ggplot2::element_text(size=11),
                                                    legend.key.size       = ggplot2::unit(1, "cm"),
                                                    legend.title          = ggplot2::element_text(size=13),
                                                    legend.box.background = ggplot2::element_rect(color = "white")))                                  




#' Assign specific color palette to use for community cluster styling throughout Shiny App
#' 
#' @param cluster_palette A string of a palette from RColorBrewer
#' @param n_clusters      A numeric equaling the number of clusters in the network
#' 
#' @returns array of hexadecimal colors to style clusters with
#' @noRd
set_cluster_colors <- function(cluster_palette, n_clusters)
{
  
  full_palette <- RColorBrewer::brewer.pal(8, cluster_palette)
  if(n_clusters <= 8){    
    cluster_colors <- full_palette[1:n_clusters]
  } else {
    cluster_colors <- grDevices::colorRampPalette(full_palette)(n_clusters)
  }
  
  ## array of colors
  return(cluster_colors)
  
}


#' Obtain colors based on user-provided attribute and build associated legend
#' 
#' @param dat A tibble of either nodes or edges to be styled
#' @param color_variable A string indicating which variable is being used to determine color
#' @param variable_type A string, either "c" or "d", indicating if color_variable is continuous ("c") or discrete ("d")
#' @param colors_or_palette A string representing either a palette name from RColorBrewer to be used as continuous node colors, or an array of colors to be applied to community clusters
#' @param na_color A string indicating the color to use for NA conditions
#' 
#' @returns A single-length list with item 'legend' containing the legend grob itself
#' @noRd
obtain_dynamic_colors_legend <- function(dat, color_variable, variable_type, colors_or_palette, na_color)
{
  ## variable type:
  ## "d" = discrete, ordinal. THERE ARE NO NOMINAL EXCEPT FOR CLUSTER, WHICH IS HANDLED DIFFERENTLY.
  ## "c" = continuous
  
  ## NOTE: KEEP direction=-1 in scale_color_brewer() and scale_color_distiller()

  cvar <- as.symbol(color_variable)
  dat %>% dplyr::mutate(x = 1:dplyr::n()) -> dat2  ## quick hack works with both edges, nodes.
  legendtitle <- variable_to_title[[color_variable]]
  
  
  if (variable_type == "d")
  {
    ggplot2::ggplot(dat2) + 
      ggplot2::aes(x = x, y = factor(!!cvar), color = factor(!!cvar)) + 
      ggplot2::geom_point(size = geom.point.size) + 
      ggplot2::guides(colour = ggplot2::guide_legend(title.position="left",  
                                                     #title.hjust = 0.5,
                                                     byrow=TRUE, 
                                                     nrow = 2)
                      )  -> p
    #, legend.title = ggplot2::element_text(size = ggplot2::rel(0.8)))
    if (color_variable == "cluster_ID") 
    {   ## cluster, colors already given
      p <- p + ggplot2::scale_color_manual(name = legendtitle, na.value = na_color, values = colors_or_palette) ## NO DIRECTION HERE
    } else {
      p <- p + ggplot2::scale_color_brewer(palette = colors_or_palette, name = legendtitle, na.value = na_color) #, direction = -1)
    } 
  }
  
  if (variable_type == "c")
  {
    ## we want max_age legend to go high->low, but colors should in same direction as the rest.
    rev_guide <- FALSE
    direction <- -1
    if (color_variable == "max_age"){
      direction <- 1
      rev_guide <- TRUE
    }
    
    ggplot2::ggplot(dat2) + 
      ggplot2::aes(x = x, y = !!cvar, color = !!cvar) + 
      ggplot2::geom_point() + 
      ggplot2::scale_color_distiller(name = legendtitle, palette = colors_or_palette, na.value = na_color, direction = direction) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(reverse = rev_guide,
                                                        barheight = ggplot2::unit(1, "cm"),
                                                        title.position="top", 
                                                        frame.colour = "black", 
                                                        ticks.colour = "black")) -> p
  }
  
  
  
  ggplot2::ggplot_build(p)$data[[1]] %>% 
    tibble::as_tibble() %>% 
    dplyr::pull(colour) -> data_colors  ## ARRAY OF COLORS
  return (list("colors" = data_colors, "legend" = cowplot::get_legend(p)))
}





#' Build a legend associated with single-colored nodes 
#' 
#' @param singlecolor A string of the color to be used
#' @param singlesize A string of the associated node shape
#' @param legendtitle A string to use as the outputted legend's title
#' 
#' @returns A single-length list with item 'legend' containing the legend grob itself
#' @noRd
obtain_legend_singlecolor <- function(singlecolor, singleshape, legendtitle)
{

  use_shape <- dplyr::case_when(singleshape %in% c("text", "circle", "dot") ~ "circle",
                                singleshape %in% c("square", "box")         ~ "square")
  
  tibble::tibble(x = 1, y = 1, type = legendtitle) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=x,y=y,color=type) +
    ggplot2::geom_point(size = geom.point.size, shape = use_shape) +
    ggplot2::scale_color_manual(name = "", values=c(singlecolor)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size=16)) -> p
 return (list("legend" = cowplot::get_legend(p) ))
  
}





#' Determine node sizes based on user-provided attribute
#' 
#' @param filtered_nodes A tibble containing nodes filtered for the group, either element or mineral
#' @param size_variable A string indicating which variable is being used to determine node sizing
#' @param lowsize A numeric for the lower bound of sizing
#' @param highsize A numeric for the upper bound of sizing
#' @param size_scale A numeric to scale ggplot sizes for improved display in visnetwork
#' 
#' @returns Numeric array of sizes to be applied to nodes in filtered group 
#' @noRd
obtain_node_sizes <- function(filtered_nodes, size_variable, lowsize, highsize, size_scale = 1)
{
  
  svar <- as.symbol(size_variable)
  ggplot2::ggplot(filtered_nodes) + 
    ggplot2::aes(x = id, y = !!svar, size = !!svar) +
    ggplot2::geom_point() + 
    ggplot2::scale_size(range = c(lowsize,highsize)) -> p
  
  ggplot2::ggplot_build(p)$data[[1]] %>% 
    tibble::as_tibble() %>%
    dplyr::pull(size) -> sizes
  sizes <- sizes * size_scale
  return(sizes) ## ARRAY

}



