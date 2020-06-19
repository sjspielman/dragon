#' Build table to display for all element node information
#' @param nodes Tibble of nodes and associated metadata
#' @return Element node information table for display, rounded to 3 digits
#' @noRd
build_element_exploration_table <- function(nodes)
{
  nodes %>%
    dplyr::filter(group == "element") %>%
    dplyr::rename(element = id) %>%
    ## specifies output order as well
    dplyr::select(element, 
                  element_name, 
                  cluster_ID, 
                  closeness,
                  network_degree_norm, 
                  pauling, 
                  element_redox_network,
                  num_localities,
                  element_hsab, 
                  element_metal_type,
                  atomic_mass, 
                  number_of_protons, 
                  element_table_period,
                  element_table_group, 
                  atomic_radius, 
                  element_specific_heat, 
                  element_density) %>%
    dplyr::distinct() %>%
    dplyr::arrange(element) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 3)) %>%
    rename_for_ui()
  
}



#' Build table to display for all mineral node information
#' @param nodes Tibble of nodes and associated metadata
#' @param locality_info Tibble of mineral locality information
#' @return Mineral node information table for display, rounded to 3 digits
#' @noRd
build_mineral_exploration_table <- function(nodes, locality_info)
{
  nodes %>%
    dplyr::filter(group == "mineral") %>%
    dplyr::rename(mineral_name = id) %>%
    dplyr::select(mineral_name, 
                  cluster_ID, 
                  closeness,
                  network_degree_norm, 
                  max_age,
                  mean_pauling, 
                  cov_pauling,
                  rruff_chemistry,
                  ima_chemistry,
                  num_localities) %>%
    dplyr::left_join(locality_info) %>%
    dplyr::distinct() %>%
    dplyr::arrange(mineral_name) %>%
    dplyr::select(mineral_name, mineral_id, ima_chemistry, rruff_chemistry, max_age, mean_pauling, cov_pauling, cluster_ID, closeness, network_degree_norm, dplyr::everything()) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 3)) %>%
    rename_for_ui() 
    
}




#' Prepare baseline table to display for "Selected Node Table" that includes all edges in the network
#' @param edges Tibble of network edges
#' @param nodes Tibble of network nodes
#' @return Baseline tibble
#' @noRd
prepare_raw_node_table <- function(edges, nodes)
{
  edges %>% 
    dplyr::select(to,  ## element
                  mineral_name,  ## mineral
                  element_redox_mineral) %>% 
    dplyr::rename(element = to) -> sel_edges

  # Columns that apply to both minerals and elements, will need to be "duplicated"
  nodes %>%
    dplyr::select(group, id, cluster_ID, closeness, network_degree_norm, num_localities) -> sel_both
  # Duplicate for minerals
  sel_both %>% 
    dplyr::filter(group == "mineral") %>%
    dplyr::rename(mineral_cluster_ID = cluster_ID,
                  mineral_closeness = closeness,
                  mineral_network_degree_norm = network_degree_norm,
                  num_localities_mineral = num_localities) -> sel_both_mineral
  # Duplicate for elements
  sel_both %>% 
    dplyr::filter(group == "element") %>%
    dplyr::rename(element_cluster_ID = cluster_ID,
                  element_closeness = closeness,
                  element_network_degree_norm = network_degree_norm,
                  num_localities_element = num_localities) -> sel_both_element
  
  # Element-only columns
  nodes %>%
    dplyr::filter(group == "element") %>%
    dplyr::select(id, pauling, element_hsab, element_metal_type, element_redox_network) %>%
    dplyr::right_join(sel_both_element) %>%
    dplyr::rename(element = id) %>%
    dplyr::select(-group) -> sel_element
  
  
  # Mineral-only columns
  nodes %>%
    dplyr::filter(group == "mineral") %>%
    dplyr::select(id, mineral_id, max_age, mean_pauling, cov_pauling, ima_chemistry, rruff_chemistry) %>%
    dplyr::right_join(sel_both_mineral)  %>%
    dplyr::rename(mineral_name = id) %>%
    dplyr::select(-group) -> sel_mineral
  
  # Join everything together, rename, and return
  sel_edges %>% 
    dplyr::left_join(sel_mineral, by = "mineral_name") %>%
    dplyr::left_join(sel_element, by = "element") %>%
    dplyr::select(element, mineral_name, dplyr::everything()) %>% 
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 3)) %>% ## ROUND NUMERIC TO 3
    rename_for_ui() 
}

build_final_node_table <- function(raw_node_table, selected_nodes, columns_to_display)
{

  ## Subset raw_node_table to only the elements of interest --------------------------------
  
  ## Add the specified cluster elements to `selected_nodes` ----------------------------
  selected_clusters <- stringr::str_match(selected_nodes, "All cluster ([0-9]+) elements")[,2]
  selected_clusters <- selected_clusters[!is.na(selected_clusters)] ## "5", "6"
  cluster_col <- as.symbol(eval(parse(text = "element_cluster_ID_str")))
  raw_node_table %>% 
    dplyr::filter(!!cluster_col %in% selected_clusters) %>%
    dplyr::pull(Element) -> selected_cluster_elements
  selected_nodes <- unique( c(selected_nodes, selected_cluster_elements) )
  selected_nodes <- selected_nodes[!(stringr::str_detect(selected_nodes, "All"))]
  
  ## Subset the specified elements
  raw_node_table %>%
    dplyr::filter(Element %in% selected_nodes) %>%
    dplyr::distinct() -> subsetted_node_table

  # Network metrics need to have SEPARATE mineral and element columns
  for (both_choice in selected_node_table_column_choices_network)
  {
    if (both_choice %in% columns_to_display)
    { 
      raw_choice <- title_to_variable[both_choice][[1]]
      columns_to_display <- columns_to_display[columns_to_display != both_choice]
      element_choice <- eval(parse(text=paste0("element_", raw_choice, "_str")))
      mineral_choice <- eval(parse(text=paste0("mineral_", raw_choice, "_str")))
      columns_to_display <- c(columns_to_display, element_choice, mineral_choice)
    }
  }  

  ## Select the columns of interest
  subsetted_node_table %>%
    dplyr::arrange(Element) %>%
    dplyr::select(columns_to_display) %>%  ## this works!
    dplyr::distinct() %>% 
    dplyr::select(Element, Mineral, dplyr::everything()) ## just to order columns

}
