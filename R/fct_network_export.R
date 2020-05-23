##########################################################################################################################
######################## Convert visNetwork to suitable igraph version for exporting the image ###########################
visnetwork_to_igraph <- function(nodes, edges, show_node_frame)
{
  
  ####################### BASELINES ###########################
  element_cex_baseline_textonly <- 1.4
  element_cex_baseline <- 1 
  mineral_cex_baseline <- 0.4
  max_cex_limit         <- 2.5
  max_size_limit        <- 20
  element_size_baseline <- 8
  mineral_size_baseline <- 1.5 
  norm_element <- 1
  norm_mineral <- 1 
  baseline_num_elements <- 50 
  baseline_num_minerals <- 1000
  #############################################################
  
  
  ### Modify baselines depending on number of each node types.
  number_element_nodes <- nodes %>% 
                            dplyr::filter(group == "element") %>%
                            dplyr::tally() %>% 
                            dplyr::pull(n) 
  number_mineral_nodes <- nodes %>% 
                            dplyr::filter(group == "mineral") %>% 
                            dplyr::tally() %>% 
                            dplyr::pull(n) 
  
  if (number_element_nodes > baseline_num_elements) norm_element <- (number_element_nodes / baseline_num_elements) 
  if (number_mineral_nodes > baseline_num_minerals) norm_mineral <- (number_mineral_nodes / baseline_num_minerals) 
  
  
  element_size_baseline <- element_size_baseline / norm_element
  element_cex_baseline_textonly <- element_cex_baseline_textonly / norm_element
  mineral_size_baseline <- mineral_size_baseline / norm_mineral
  element_cex_baseline  <- element_cex_baseline / norm_element
  mineral_cex_baseline  <- mineral_cex_baseline / norm_mineral
  max_cex_limit         <- max_cex_limit / norm_element
  max_size_limit        <- max_size_limit / norm_element
  
  vis_shapes_only <- c("diamond", "triangle", "star", "ellipse") # these shapes are not available in igraph, so we may remove them from dragon as well?
  ## igraph shapes: “circle”, “square”, “csquare”, “rectangle”, “crectangle”, “vrectangle”, “pie” (see vertex.shape.pie), ‘sphere’, and “none” are supported,
  
  ### Keep only the columns we need from edges and nodes, AND rename columns to their igraph names rather than visNetwork names
  #### remember: for elements, font.size IS size due to how visNetwork handles labeling
  edges %>% 
    dplyr::select(from, to, color) -> edges_igraph
  
  nodes %>%
    dplyr::select(id, group, shape, size, color.background, color.border, font.color, font.size, font.face, x, y) %>%
    dplyr::rename(color = color.background) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(shape = dplyr::case_when(
                                    shape %in% c("dot", "circle") ~ "circle",
                                    shape %in% c("square", "box") ~ "square", 
                                    shape %in% vis_shapes_only    ~ "circle",
                                    shape == "text"               ~ "none"
                          ),
      label        = ifelse(font.size == 0, NA, id),
      label.color  = font.color,
      label.font   = 1, 
      label.family = "mono",## courier in visNetwork, this is equivalent
      frame.color  = ifelse(show_node_frame == TRUE, color.border, NA) ) %>%  ## borders in igraph are overly thick, user can choose
      # UNGROUP UNGROUP UNGROUP
      dplyr::ungroup() -> nodes_igraph   
  
  ### Hack for getting edges to text-only nodes to look reasonable. Rather than removing shapes, we will make very very tiny white borderless circles.
  nodes_igraph %<>%
    dplyr::mutate(frame.color = ifelse(shape == "none", NA, frame.color),
                  color       = ifelse(shape == "none", "#FFFFFF", color),
                  size        = ifelse(shape == "none", 0.001, size),
                  noshape     = shape == "none", 
                  shape       = ifelse(shape == "none", "circle", shape))
  
  
  ###### And now for the great sizing hellscape
  nodes_igraph %>% 
    dplyr::filter(group == "element") %>%
    dplyr::mutate(mean_font_size = mean(font.size),
           mean_size = mean_font_size,   ## line is NOT a bug - size = font.size for elements in vis 
           size      = size/mean_size * element_size_baseline, 
           label.cex = dplyr::case_when(noshape == TRUE & mean_font_size != 0 ~ font.size/mean_font_size * element_cex_baseline_textonly, 
                                        noshape == FALSE & mean_font_size != 0 ~ font.size/mean_font_size * element_cex_baseline,
                                        mean_font_size == 0                   ~ 0)) -> elements
  nodes_igraph %>% 
    dplyr::filter(group == "mineral") %>%
    dplyr::mutate(mean_size = mean(size),
                  mean_font_size = mean(font.size),
                  size = size/mean_size * mineral_size_baseline,
                  label.cex = ifelse(mean_font_size == 0, 0, font.size/mean_font_size * mineral_cex_baseline)) -> minerals
  
  dplyr::bind_rows(minerals, elements) %>% ## elements on top so bottom of df
    dplyr::group_by(group) %>%
    dplyr::mutate(rescale_cex = max(label.cex) > max_cex_limit,
           rescale_size = max(size) > max_size_limit) %>%               
    ## Cap max sizes
    dplyr::mutate(label.cex = ifelse(rescale_cex,
                              (label.cex / max(label.cex)) * max_cex_limit,
                              label.cex),
           size      = ifelse(rescale_size,
                              (size / max(size)) * max_size_limit,
                              size)) %>%
    ### ungroup ungroup ungroup!!!!! for coords
    dplyr::ungroup() -> nodes_igraph
  
  
  nodes_igraph %>%
    dplyr::select(x, y) %>% ## Select order can flip 180, FYI
    ## igraph plots upside down from visNetwork, because sure why not, so flip the sign.
    dplyr::mutate(y = -1 * y) -> coords
  
  inet <- igraph::graph_from_data_frame(edges_igraph, directed=FALSE, vertices = nodes_igraph)
  
  y_size <- abs( max(coords$y) - min(coords$y) ) 
  x_size <- abs( max(coords$x) - min(coords$x) ) 
  vis_aspect_ratio <- y_size / x_size  ## >1 is wide. x goes on denom.
  
  return (list("igraph_network" = inet, "coords" = as.matrix(coords), "vis_aspect_ratio" = vis_aspect_ratio))
}





calculate_output_node_positions <- function(nodes, positions, inet, output_layout, seed)
{
  if (is.null(positions)){
    ## DEFAULT LAYOUT. Obtain the original coordinates, *unless physics* 
    set.seed(seed)
    
    coord_string <- paste0("igraph::", output_layout, "(inet)")
    as.data.frame( eval(parse(text = coord_string)) ) %>%
      dplyr::rename(x = V1, y = V2) %>%
      dplyr::mutate(id = igraph::vertex_attr(inet, "name")) -> coords
  } else {
    ## CUSTOM LAYOUT by dragging network around
    coords <- do.call("rbind", lapply(positions, function(p){ data.frame(x = p$x, y = p$y)}))
    coords$id <- names(positions)
  }
  nodes %>% dplyr::left_join(coords, by = "id")     
}
