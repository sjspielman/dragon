prepare_selected_node_table <- function(nodes, edges, locality_info)
{
  edges %>% 
    dplyr::select(from, to, element_redox_mineral, max_age, num_localities_mineral, mean_pauling, cov_pauling) %>% 
    dplyr::rename(id = from) -> edges_mineral_id
  edges %>% 
    dplyr::select(from, to, element_redox_mineral, element_redox_network) %>%
    dplyr::rename(id = to) -> edges_element_id

  nodes %>%
    dplyr::select(-label, -network_degree, -font.face, -title, -type) -> nodes2

  nodes2 %>% 
    dplyr::filter(group == "mineral") %>%
    dplyr::select(id, cluster_ID, closeness, network_degree_norm, mineral_id, max_age, ima_chemistry, rruff_chemistry, mean_pauling, cov_pauling, num_localities) %>%
    dplyr::rename(num_localities_mineral = num_localities) -> mineral_nodes

  nodes2 %>% 
    dplyr::filter(group == "element") %>%
    dplyr::select(id, cluster_ID, closeness, network_degree_norm, element_name, element_hsab, TablePeriod, TableGroup, AtomicMass, AtomicRadius, pauling, MetalType, Density, SpecificHeat, element_redox_network, num_localities) %>%
    dplyr::rename(num_localities_element = num_localities)-> element_nodes  
    

  edges_mineral_id %>% 
    dplyr::left_join(mineral_nodes) %>% 
    dplyr::rename(from = id,
                  mineral_cluster_ID = cluster_ID,
                  mineral_closeness = closeness,
                  mineral_network_degree_norm = network_degree_norm) -> mineral_all

  edges_element_id %>% 
    dplyr::left_join(element_nodes) %>% 
    dplyr::select(-from, -id, -element_redox_mineral)%>%
    dplyr::rename(element_cluster_ID = cluster_ID,
                  element_closeness = closeness,
                  element_network_degree_norm = network_degree_norm) -> element_all

  ## CHECK HERE
  if (nrow(element_all) != nrow(mineral_all)){
    final_table <- FALSE
  } else {
    dplyr::bind_cols(mineral_all, element_all) %>%
      dplyr::left_join(locality_info, by = "mineral_id") %>%
      dplyr::select(-mineral_name) %>%
      # THE GREAT RENAMING IS UPON US. TODO this should probably be a separate function
      dplyr::rename(!! variable_to_title[["mineral_name"]] := from,          ## Mineral
                    !! variable_to_title[["element"]] := to,                 ## Element
                    ## Mineral information
                    !! variable_to_title[["mineral_id"]] := mineral_id,
                    !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry,
                    !! variable_to_title[["ima_chemistry"]] := ima_chemistry,
                    !! variable_to_title[["max_age"]] := max_age,
                    !! variable_to_title[["num_localities_mineral"]] := num_localities_mineral,   
                    !! variable_to_title[["mean_pauling"]] := mean_pauling,
                    !! variable_to_title[["cov_pauling"]] := cov_pauling,
                    !! variable_to_title[["mineral_cluster_ID"]] := mineral_cluster_ID,
                    !! variable_to_title[["mineral_closeness"]] := mineral_closeness,
                    !! variable_to_title[["mineral_network_degree_norm"]] := mineral_network_degree_norm,
                    ## Mineral locality information
                    !! variable_to_title[["mindat_id"]] := mindat_id,
                    !! variable_to_title[["locality_longname"]] := locality_longname,
                    !! variable_to_title[["age_type"]] := age_type,
                    !! variable_to_title[["max_age_locality"]] := max_age_locality,
                    !! variable_to_title[["min_age_locality"]] := min_age_locality,
                    ## Element information
                    !! variable_to_title[["element_name"]] := element_name,  ## Full element name
                    !! variable_to_title[["element_redox_network"]] := element_redox_network,            
                    !! variable_to_title[["pauling"]] := pauling,
                    !! variable_to_title[["element_hsab"]] := element_hsab,
                    !! variable_to_title[["TableGroup"]] := TableGroup,
                    !! variable_to_title[["TablePeriod"]] := TablePeriod,
                    !! variable_to_title[["MetalType"]] := MetalType,
                    !! variable_to_title[["AtomicMass"]] := AtomicMass,
                    !! variable_to_title[["AtomicRadius"]] := AtomicRadius,
                    !! variable_to_title[["SpecificHeat"]] := SpecificHeat,
                    !! variable_to_title[["Density"]] := Density ,
                    !! variable_to_title[["num_localities_element"]] := num_localities_element,  
                    !! variable_to_title[["element_cluster_ID"]] := element_cluster_ID,
                    !! variable_to_title[["element_closeness"]] := element_closeness,
                    !! variable_to_title[["element_network_degree_norm"]] := element_network_degree_norm,
                    ## Joint mineral/element information
                    !! variable_to_title[["element_redox_mineral"]] := element_redox_mineral) -> final_table
  }
  return(final_table)
}



#build_node_table <- function(raw_node_table, selected_nodes, selected_node_group, columns_to_display)
#{
#  ## If no selected columns, display either "Element" or "Mineral"
#  if(is.null(columns_to_display)){
#    raw_node_table
#   
# }

