load("R/sysdata.rda") ## TODO devtools:check() fails without explicit load. how fix?


total_max_age   <- round( max(rruff$max_age) + 0.1, 1)
rruff_separated <-  element_redox_states %>% dplyr::select(-element_redox_mineral)

initialize_data <- function(elements_of_interest, force_all_elements)
{ 

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
    dplyr::ungroup() %>%
    dplyr::select(mineral_name) %>%
    dplyr::inner_join(rruff) -> elements_only
  elements_only
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
    dplyr::mutate(num_localities_mineral = dplyr::n()) %>%
    dplyr::ungroup() 
  
  elements_only %>% 
    dplyr::select(mineral_name, mineral_id, max_age, min_age, mindat_id, locality_longname, age_type) %>%
    dplyr::rename(max_age_locality = max_age,
                  min_age_locality = min_age) %>%
    dplyr::ungroup() -> locality_info
  
  elements_only %>%
    dplyr::group_by(mineral_name) %>%
    dplyr::summarize(overall_max = max(age_check)) %>% 
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


construct_network   <- function(elements_only_age, elements_by_redox)
{
  
  ## Merge data with redox states, element_info, and some associated processing --------------------------------
  elements_only_age %>%    
    tidyr::separate_rows(chemistry_elements,sep=" ") %>%
    dplyr::rename(element = chemistry_elements) %>%
    dplyr::left_join(element_redox_states, by = c("element", "mineral_name")) %>%
    ## Add two columns: element_redox_mineral_sign is +/- and element_as_redox is the pasted expression eg Ca2+
    dplyr::mutate(element_redox_mineral_sign = dplyr::case_when(element_redox_mineral == abs(element_redox_mineral) ~ "+",
                                                                element_redox_mineral != abs(element_redox_mineral) ~ "-"
                                                               ), ## END case_when  
                  element_as_redox           = dplyr::if_else(is.na(element_redox_mineral), 
                                                              paste0(element, "  "), 
                                                              paste0(element, element_redox_mineral_sign, abs(element_redox_mineral))
    ) ## END if_else
    ) %>%
    dplyr::select(-element_redox_mineral_sign) %>%
    dplyr::left_join(element_info, by = "element") %>%
    ## Factor some of the joined in colunms from element_info
    dplyr::mutate(element_hsab = factor(element_hsab, levels = element_hsab_levels),
                  TablePeriod  = factor(TablePeriod),
                  TableGroup   = factor(TableGroup)) %>%
    ## Calculate electronegativity quantities - so we need to merge in element_info at this time
    dplyr::group_by(mineral_name) %>%
    dplyr::mutate(mean_pauling = mean(pauling),
                  cov_pauling  = stats::sd(pauling) / mean_pauling ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() -> network_information
  
  
  
  ## Build igraph network
  if (elements_by_redox){
    network_information %<>% dplyr::mutate(to = element_as_redox)
  } else {
    network_information %<>% dplyr::mutate(to = element)
  }
  network_information %>% 
    dplyr::rename(from = mineral_name) %>%
    dplyr::select(from, to) %>%
    dplyr::distinct() -> network_to_from
  element_network                 <- igraph::graph.data.frame(network_to_from, directed = FALSE)
  igraph::V(element_network)$type <- igraph::bipartite_mapping(element_network)$type  ## mineral is FALSE and element is TRUE
  
  ## Edge metadata ------------------------------------------------
  ## Columns that require an explicit link between minerals and elements, OR are used in edge styling
  network_information %>%
    dplyr::select(mineral_name, element, element_as_redox, element_redox_mineral, max_age, num_localities_mineral) %>%
    dplyr::distinct() -> edge_metadata
  
  
  ## Node metadata ------------------------------------------------
  network_information %>% 
    dplyr::select(mineral_name, mineral_id, max_age, num_localities_mineral, ima_chemistry, rruff_chemistry,  mean_pauling, cov_pauling) %>% 
    dplyr::rename(id = mineral_name) %>%
    dplyr::distinct() -> mineral_metadata
  
  ## Calculate average element redox state in the whole network to be added into element_metadata
  edge_metadata %>% 
    dplyr::group_by(element) %>%
    dplyr::summarize(element_redox_network = mean(element_redox_mineral, na.rm=T)) %>% 
    dplyr::mutate(element_redox_network = ifelse(is.nan(element_redox_network), 
                                                 NA, element_redox_network)) -> element_redox_network_df
  network_information %>% 
    dplyr::select(-mineral_name, -mineral_id, -max_age, -ima_chemistry, -rruff_chemistry, -mean_pauling, -cov_pauling, -element_redox_mineral) %>%
    dplyr::group_by(element_as_redox) %>%
    dplyr::mutate(num_localities_element = sum(num_localities_mineral)) %>% 
    dplyr::select(-num_localities_mineral) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(element_redox_network_df) %>%
    dplyr::rename(id = to) %>% ## Will automatically be either element or element_as_redox due to first if/else in this function
    dplyr::distinct() -> element_metadata
  
  ## Merge nodes with associated data ---------------------------------------------
  tibble::tibble(id    = unique(c(network_to_from$from, network_to_from$to)), 
                 label = id, 
                 group = dplyr::if_else(id %in% network_to_from$from, "mineral", "element")) %>%
    dplyr::mutate(network_degree = as.numeric( igraph::degree(element_network) ), 
                  closeness      = as.numeric( igraph::closeness(element_network) )   
                 ) %>%  
    dplyr::group_by(group) %>%
    dplyr::mutate(network_degree_norm = network_degree / max(network_degree)) %>%
    dplyr::left_join(mineral_metadata) %>%
    dplyr::left_join(element_metadata) %>% 
    dplyr::mutate(num_localities = dplyr::if_else(group == "mineral",  
                                                  num_localities_mineral,
                                                  num_localities_element)) %>%
    dplyr::select(-num_localities_mineral, -num_localities_element) %>%
    dplyr::distinct() -> nodes
  

  ## Merge edges with associated data ---------------------------------------------
  network_to_from %>% 
    dplyr::mutate(mineral_name = from) %>%
    dplyr::inner_join(edge_metadata) -> edges
  
  return (list("nodes" = nodes, "edges" = edges, "graph" = element_network))
}



community_detect_network <- function(network, cluster_algorithm)
{
  if (cluster_algorithm == "Louvain")             return (igraph::cluster_louvain(network))
  if (cluster_algorithm == "Leading eigenvector") return (igraph::cluster_leading_eigen(network))    
}



add_shiny_node_titles <- function(nodes, elements_by_redox)
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





















