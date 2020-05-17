rruff_separated <-  element_redox_states %>% dplyr::select(-element_redox_mineral)
total_max_age   <- round( max(rruff$max_age) + 0.1, 1)
community_detect_network <- function(network, cluster_algorithm)
{
  if (cluster_algorithm == "Louvain")               return (cluster_louvain(network))
  if (cluster_algorithm == "Leading eigenvector")   return (cluster_leading_eigen(network))    
}


calc_total_max_age <- function()
{
  round( max(rruff$max_age) + 0.1, 1)
}

initialize_data <- function(elements_of_interest, force_all_elements)
{ 
  
  #rruff_separated <-  element_redox_states %>% dplyr::select(-element_redox_mineral)

  ## Must have all elements
  if (force_all_elements)
  {
    n_elements <- length(elements_of_interest)
    rruff_separated %>%
      dplyr::group_by(mineral_name) %>%
      dplyr::mutate(has_element = ifelse( sum(element %in% elements_of_interest) == n_elements, TRUE, FALSE)) %>% 
      dplyr::filter(has_element == TRUE) -> elements_only_raw
  } else 
  { ## Has at least one element
    rruff_separated %>%
      dplyr::group_by(mineral_name) %>%
      dplyr::filter(element %in% elements_of_interest) -> elements_only_raw
  }
  elements_only_raw %>%
    dplyr::select(mineral_name) %>%
    dplyr::inner_join(rruff) 
}

initialize_data_age <- function(elements_only, age_limit, max_age_type)
{
  lb <- age_limit[1]
  ub <- age_limit[2]
  
  if (max_age_type == "Minimum") elements_only %<>% dplyr::mutate(age_check = min_age) 
  if (max_age_type == "Maximum") elements_only %<>% dplyr::mutate(age_check = max_age) 
  
  elements_only %<>% 
    dplyr::filter(age_check >= lb, age_check <= ub) %>% 
    dplyr::group_by(mineral_name) %>%
    dplyr::mutate(num_localities_mineral = n()) %>%
    dplyr::ungroup() 
  
  elements_only %>% 
    dplyr::select(mineral_name, mineral_id, max_age, min_age, mindat_id, locality_longname, age_type) %>%
    dplyr::rename(max_age_locality = max_age,
                  min_age_locality = min_age) %>%
    dplyr::ungroup() -> locality_info
  
  elements_only %>%
    dplyr::group_by(mineral_name) %>%
    dplyr::summarize(overall_max = max(max_age)) %>% 
    dplyr::rename(max_age = overall_max) %>%
    dplyr::inner_join(elements_only %>% dplyr::select(-min_age, -max_age)) %>%
    ## below line removes columns:mindat_id, locality_longname, age_type, age_check, element
    dplyr::select(mineral_name, mineral_id, max_age, num_localities_mineral, ima_chemistry, rruff_chemistry, chemistry_elements) %>%
    #dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, chemistry_elements) %>%
    dplyr::ungroup() %>% 
    dplyr::distinct() -> elements_only_age
  # > names(elements_only_age)
  # [1] "mineral_name"           "mineral_id"             "max_age"               
  # [4] "num_localities_mineral" "ima_chemistry"          "rruff_chemistry"       
  # [7] "chemistry_elements" 
  
  list("elements_only_age" = elements_only_age, "locality_info" = locality_info)
  
}

obtain_network_information <- function(elements_only_age, elements_by_redox)
{      
 
  elements_only_age %>%    
    tidyr::separate_rows(chemistry_elements,sep=" ") %>%
    dplyr::rename(element = chemistry_elements) %>%
    dplyr::left_join(element_redox_states, by = c("element", "mineral_name"))  -> network_information   
    
  if (elements_by_redox)
  {
    network_information %<>% 
      dplyr::mutate(base_element = element, 
                    element_redox_mineral_sign = dplyr::case_when(element_redox_mineral == abs(element_redox_mineral) ~ "+", ## works for 0s (shouldn't be any though?)
                                                                  element_redox_mineral != abs(element_redox_mineral) ~ "-"
                                                                 ), ## END case_when  
                    element = dplyr::if_else(is.na(element_redox_mineral), 
                                             paste0(element, "  "), 
                                             paste0(element, element_redox_mineral_sign, abs(element_redox_mineral))
                                            ) ## END if_else
                    ) %>%
      dplyr::select(-element_redox_mineral_sign) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }
  
  
  network_information %>%
    dplyr::group_by(element) %>%
    dplyr::mutate(element_redox_network  = mean(element_redox_mineral, na.rm=TRUE),
                  num_localities_element = sum(num_localities_mineral)) %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(element, mineral_name) %>%
    dplyr::mutate(element_redox_mineral = mean(element_redox_mineral, na.rm=TRUE),
                  element_redox_mineral = ifelse(is.nan(element_redox_mineral), NA, element_redox_mineral),
                  element_redox_network = ifelse(is.nan(element_redox_network), NA, element_redox_network)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(element_info, by = "element") %>%
    dplyr::group_by(mineral_name) %>%
    dplyr::mutate(mean_pauling = mean(pauling),
                  cov_pauling  = stats::sd(pauling) / mean_pauling ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    ## Order columns for downstream use ease
    dplyr::select(mineral_name, 
                  mineral_id,
                  max_age,
                  mean_pauling,
                  cov_pauling,
                  ima_chemistry,
                  rruff_chemistry,
                  num_localities_mineral,
                  element,
                  element_name,
                  pauling,
                  element_redox_mineral,
                  element_redox_network,
                  everything())

}


construct_network   <- function(network_information, elements_by_redox)
{
  
  ## Build igraph network
  network_information %>% 
    dplyr::select(mineral_name, element) %>%
    dplyr::distinct() -> network_to_from
      
  element_network                 <- igraph::graph.data.frame(network_to_from, directed=FALSE)
  igraph::V(element_network)$type <- igraph::bipartite_mapping(element_network)$type  ## mineral is FALSE and element is TRUE
  
  
  ### VERTEX_INFORMATION WAS BUILT HERE. IT;S ON THE DESKTOP RIGHT NOW
  ### TODO: CREATE A SEPARATE FUNCTION FOR CONVERSION TO VISNETWORK, TO ALLOW FOR SOME CMD LINE USE FREE OF SHINY
  ## vertexinformation is one row per node in the network
  ## networkinformation is one row per separated out element. eg if a mineral has 5 elements it has 5 rows
  
  ## Convery to visNetwork object for use in shiny
  net <- visNetwork::toVisNetworkData(element_network)
  
  #   tibble::as_tibble(net$edges) # this is columns from and to
    
  ## Obtain data associated with nodes -------------------------------------------
  deg       <- 
  closeness <- igraph::closeness(element_network)

  
  ## Mineral node data ------------------------------------------------
  network_information %>% 
    dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, mean_pauling, cov_pauling, rruff_chemistry, ima_chemistry) %>% #sd_pauling,
    dplyr::rename(id = mineral_name) %>%
    dplyr::distinct() -> mineral_information
    ## why again? dplyr::rename(item = mineral_name) %>%
    
  ## Element node data ------------------------------------------------
  network_information %>% 
    dplyr::select(-mineral_name, -mineral_id, -num_localities_mineral, -max_age, -mean_pauling, -cov_pauling, -rruff_chemistry, -ima_chemistry, -element_redox_mineral) %>% 
    dplyr::rename(id = element) %>%
    dplyr::distinct() %>%
    dplyr::mutate(element_hsab = factor(element_hsab, levels = element_hsab_levels),
                  TablePeriod  = factor(TablePeriod),
                  TableGroup   = factor(TableGroup)) -> element_information
  
    
  ## Merge nodes with associated data ---------------------------------------------
  tibble::as_tibble(net$nodes) %>%  # names:  id type label 
    dplyr::mutate(type           = dplyr::if_else(type == FALSE, "mineral", "element"),
                  network_degree = as.numeric( igraph::degree(element_network) ), 
                  closeness      = as.numeric( igraph::closeness(element_network) )   
                 ) %>%  
    dplyr::group_by(type) %>%
    dplyr::mutate(network_degree_norm = network_degree / max(network_degree)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(group = type) %>%       
    dplyr::left_join(mineral_information) %>%  
    dplyr::left_join(element_information) %>%
    dplyr::mutate(num_localities = ifelse(is.na(num_localities_mineral), 
                                                num_localities_element, 
                                                num_localities_mineral)) %>%
    dplyr::select(-num_localities_mineral, -num_localities_element) -> nodes
    

  ## Obtain data associated with edges -------------------------------------------
  network_information %>% 
    dplyr::select(mineral_name, max_age, num_localities_mineral) %>%
    distinct() -> edge_only_information

  ## Merge edges with associated data ---------------------------------------------
  tibble::as_tibble(net$edges) %>%
    rename(mineral_name = from,
           element = to) %>%
    inner_join(edge_only_information) -> edges
  
  return (list("nodes" = nodes, "edges" = edges, "graph" = element_network))
}



add_shiny_node_titles <- function(nodes, element_by_redox)
{
  charadd <- 0
  if (elements_by_redox){
    charadd <- 2
  }
  
  nodes %>%
    dplyr::mutate(font.face = "courier",
                  label = dplyr::case_when(group == "mineral"                     ~ label,
                                          group == "element" & nchar(label) == 1+charadd  ~ paste0(" ", label, " "),
                                          group == "element" & nchar(label) == 2+charadd  ~ paste0(" ", label),
                                          group == "element" & nchar(label) == 3+charadd  ~ label), ## END label case_when  
                  title = dplyr::case_when(group == "mineral"  ~ paste0("<p>", 
                                                                        id, "<br>", 
                                                                        ima_chemistry, "<br>", 
                                                                        paste0("Maximum known age: ", max_age, " Ga"), "</p>"),
                                          group == "element" & elements_by_redox == TRUE ~ paste0("<p>", 
                                                                                                   element_name, "<br>",
                                                                                                   ifelse(is.na(element_redox_network), 
                                                                                                          "", 
                                                                                                          paste0("Redox state: ", element_redox_network)
                                                                                                    ),"<br>",    
                                                                                                   ifelse(is.na(AtomicMass), 
                                                                                                          "", 
                                                                                                          paste0("Atomic mass: ", AtomicMass)), "<br>",  
                                                                                                   ifelse(is.na(pauling), 
                                                                                                          "", 
                                                                                                          paste0("Electronegativity: ", pauling)), "</p>"),                                                        
                                            group == "element" & elements_by_redox == FALSE ~ paste0("<p>", 
                                                                                                     element_name, "<br>", 
                                                                                                     ifelse(is.na(AtomicMass), 
                                                                                                            "", 
                                                                                                            paste0("Atomic mass: ", AtomicMass)), "<br>",  
                                                                                                     ifelse(is.na(pauling), 
                                                                                                            "", 
                                                                                                            paste0("Electronegativity: ", pauling)), "</p>")
                                          ), ## END title case_when                     
                 ) %>% ## END mutate
    dplyr::distinct() 
  
}





















