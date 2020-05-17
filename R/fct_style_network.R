style_edges <- function(edges, edge_options){
  
  if (edge_options$color_edge_by == "singlecolor") 
  {
    edge_colors <- edges %>% 
      mutate(color = edge_options$edge_color)             
    colorlegend_edge <- NA
  } else 
  {
    out <- obtain_colors_legend(session, 
                                edges, 
                                edge_options$color_edge_by, 
                                "c", 
                                edge_options$edge_palette, 
                                variable_to_title[[edge_options$color_edge_by]])
    
    edge_colors <-  left_join(edges, out$cols)
    colorlegend_edge <- out$leg
  }
  
  list("edge_legend" = colorlegend_edge, "styled_edges" = edge_colors)
}






style_nodes_colors_legend <- function(session, full_nodes, node_attr, style_options)
{

  ## Color *all* nodes by cluster ----------------------------------------------------------------
  if (style_options$color_by_cluster) 
  {        
    out <- obtain_colors_legend(session, 
                                full_nodes, 
                                "cluster_ID", 
                                "d", 
                                "NA", 
                                variable_to_title[["cluster_ID"]], 
                                style_options$cluster_colors)
    out$leg -> node_attr[["both_legend"]]
    out$cols %>% 
      dplyr::select(label, id, color) %>% 
      dplyr::rename(color.background = color) -> node_attr[["colors"]]
  } else 
  { 
    ## Color minerals  -----------------------------------------------------------
    if (style_options$color_mineral_by == "singlecolor")
    {
      out <- obtain_colors_legend_single("Mineral", 
                                         vis_to_gg_shape[style_options$mineral_shape], 
                                         style_options$mineral_color)
      out$leg -> node_attr[["mineral_legend"]] 
      full_nodes %>% 
        filter(group == "mineral") %>% 
        select(label, id) %>%
        mutate(color.background = style_options$mineral_color) -> node_attr[["mineral_colors"]]
      
    } else
    {  
      out <- obtain_colors_legend(session, full_nodes %>% filter(group == "mineral"), style_options$color_mineral_by, "c", style_options$mineral_palette, variable_to_title[[style_options$color_mineral_by]])
      out$leg -> node_attr[["mineral_legend"]]
      out$cols %>% 
        select(label, id, color) %>% 
        rename(color.background = color) -> node_attr[["mineral_colors"]]
    } 
    
    ## Color elements  ------------------------------------------------------------
    if (style_options$color_element_by == "singlecolor")
    {
      if (style_options$element_shape == "text") { 
        this_color <- style_options$element_label_color
      } else { 
        this_color <- style_options$element_color
      }
      out <- obtain_colors_legend_single("Element", 
                                         vis_to_gg_shape[style_options$element_shape], 
                                         this_color)
      out$leg -> node_attr[["element_legend"]]
      full_nodes %>% 
        filter(group == "element") %>% 
        select(label, id) %>%
        mutate(color.background = style_options$element_color) -> node_attr[["element_colors"]]
    } else if (style_options$color_element_by == "element_redox_network" & style_options$elements_by_redox == TRUE)
    {  
      obtain_colors_legend(session, 
                           full_nodes %>% dplyr::select(element, element_redox_network), 
                           style_options$color_element_by, 
                           "c", 
                           style_options$element_palette, 
                           variable_to_title[[style_options$color_element_by]]) -> out_legend
      out_legend$leg -> node_attr[["element_legend"]]
      out_legend$cols %>% 
        dplyr::select(element, color) %>% 
        dplyr::rename(id               = element, 
                      color.background = color) %>% 
        dplyr::left_join(full_nodes) %>% 
        dplyr::filter(group == "element") %>%
        dplyr::select(label, id, color.background) %>%
        dplyr::distinct() -> node_attr[["element_colors"]]
    } else
    {
      full_nodes %>% dplyr::filter(group == "element") -> legend_data
      data_type <- "c"
      if (style_options$color_element_by %in% ordinal_color_variables)  data_type <- "d"
      out_legend <- obtain_colors_legend(session, 
                                         legend_data, 
                                         style_options$color_element_by, 
                                         data_type,
                                         style_options$element_palette, 
                                         variable_to_title[[style_options$color_element_by]])
      out_legend$leg -> node_attr[["element_legend"]]
      out_legend$cols %>% 
        dplyr::select(label, id, color) %>% 
        dplyr::rename(color.background = color) -> node_attr[["element_colors"]]
    }   
    node_attr[["colors"]] <- dplyr::bind_rows(node_attr[["element_colors"]], node_attr[["mineral_colors"]]) 
  }   
  
  ## Return updated node_attr list 
  node_attr
}




style_nodes_sizes <- function(full_nodes, node_attr, style_options)
{
  
  ## ELEMENT SIZES -----------------------------------------------------------
  if (style_options$element_size_type != "singlesize") 
  {
    node_attr[["sizes"]] <- obtain_node_sizes(full_nodes %>% dplyr::filter(group == "element"), 
                                              style_options$element_size_type, 1, 4, size_scale = style_options$element_size_scale) %>%
      dplyr::select(label, id, size) %>%
      dplyr::mutate(group = "element") 
    
  } else
  {
    node_attr[["sizes"]] <- full_nodes %>% 
      dplyr::filter(group == "element") %>% 
      dplyr::select(label, id, group) %>%
      dplyr::mutate(size = style_options$element_label_size)
    
  }                     
  
  ## MINERAL SIZES -----------------------------------------------------------
  if (style_options$mineral_size_type != "singlesize") {
    obtain_node_sizes(full_nodes %>% dplyr::filter(group == "mineral"), 
                      style_options$mineral_size_type, 5, 30, size_scale = style_options$mineral_size_scale / 10) %>%
      dplyr::select(label, id, size) %>%
      dplyr::mutate(group = "mineral") -> minsizes
    
  } else 
  {
    minsizes <- full_nodes %>% 
      dplyr::filter(group == "mineral") %>% 
      dplyr::mutate(size = style_options$mineral_size) %>%
      dplyr::select(label, id, size, group)
  }         
  node_attr[["sizes"]] %<>% 
    dplyr::bind_rows(minsizes) %>% 
    dplyr::mutate(font.size = ifelse(group == "element", size, style_options$mineral_label_size))
  

  node_attr
}

style_nodes_shape_highlight_label <- function(node_attr_styled_nodes, style_options)
{
  node_attr_styled_nodes %>%
    dplyr::mutate(color.background = ifelse((id %in% style_options$elements_of_interest & style_options$highlight_element), style_options$highlight_color, color.background), 
                  color.background = ifelse(id %in% style_options$custom_selection_element, style_options$custom_selection_color, color.background), 
                  font.color = ifelse(group == "element", style_options$element_label_color, style_options$mineral_label_color),
                  font.color = ifelse((id %in% style_options$elements_of_interest & style_options$highlight_element & style_options$element_shape == "text"), style_options$highlight_color, font.color),
                  font.color = ifelse((id %in% style_options$custom_selection_element & style_options$element_shape == "text"), style_options$custom_selection_color, font.color),
                  shape = ifelse(group == "element", style_options$element_shape, style_options$mineral_shape))
}

style_nodes_elements_by_redox <- function(node_attr_styled_nodes, style_options)
{

  node_attr_styled_nodes %>%
    dplyr::filter(group == "element") %>% 
    dplyr::mutate(id2 = id) %>%
    tidyr::separate(id2, into=c("base_element", "blah")) %>%
    dplyr::mutate(color.background = ifelse(base_element %in% style_options$elements_of_interest & style_options$highlight_element, style_options$highlight_color, color.background),
                  font.color       = ifelse(base_element %in% style_options$elements_of_interest & style_options$element_shape == "text" & style_options$highlight_element, style_options$highlight_color, font.color),
                  color.background = ifelse(base_element %in% style_options$custom_selection_element, style_options$custom_selection_color, color.background),
                  font.color       = ifelse(base_element %in% style_options$custom_selection_element & style_options$element_shape == "text", style_options$custom_selection_element, font.color)) %>%
    dplyr::select(-base_element, -blah) %>%
    dplyr::bind_rows( node_attr_styled_nodes %>% dplyr::filter(group == "mineral") )
}