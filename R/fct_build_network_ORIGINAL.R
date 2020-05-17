# rruff_separated <-  element_redox_states %>% dplyr::select(-element_redox_mineral)
# total_max_age   <- round( max(rruff$max_age) + 0.1, 1)
# community_detect_network <- function(network, cluster_algorithm)
# {
#   if (cluster_algorithm == "Louvain")               return (cluster_louvain(network))
#   if (cluster_algorithm == "Leading eigenvector")   return (cluster_leading_eigen(network))    
# }
# 
# 
# calc_total_max_age <- function()
# {
#   round( max(rruff$max_age) + 0.1, 1)
# }
# 
# initialize_data <- function(elements_of_interest, force_all_elements)
# { 
#   
#   #rruff_separated <-  element_redox_states %>% dplyr::select(-element_redox_mineral)
# 
#   ## Must have all elements
#   if (force_all_elements)
#   {
#     n_elements <- length(elements_of_interest)
#     rruff_separated %>%
#       dplyr::group_by(mineral_name) %>%
#       dplyr::mutate(has_element = ifelse( sum(chemistry_elements %in% elements_of_interest) == n_elements, TRUE, FALSE)) %>% 
#       dplyr::filter(has_element == TRUE) %>%
#       dplyr::select(mineral_name) %>%
#       dplyr::inner_join(rruff) -> elements_only
#   } else 
#   { ## Has at least one element
#     rruff_separated %>%
#       dplyr::group_by(mineral_name) %>%
#       dplyr::mutate(has_element = ifelse(chemistry_elements %in% elements_of_interest, TRUE, FALSE)) %>% 
#       dplyr::filter(has_element == TRUE) %>%
#       dplyr::select(mineral_name) %>%
#       dplyr::inner_join(rruff) %>%
#       dplyr::select(-chemistry_elements) -> elements_only
#     
#   }  
#   elements_only
# }
# 
# initialize_data_age <- function(elements_only, age_limit, max_age_type)
# {
#   lb <- age_limit[1]
#   ub <- age_limit[2]
#   
#   if (max_age_type == "Minimum")
#   {
#     elements_only %<>% dplyr::mutate(age_check = min_age) 
#   } else {
#     elements_only %<>% dplyr::mutate(age_check = max_age) 
#   }
#   
#   elements_only %<>% 
#     dplyr::filter(age_check >= lb, age_check <= ub) %>% 
#     dplyr::group_by(mineral_name) %>%
#     dplyr::mutate(num_localities_mineral = n()) %>%
#     dplyr::ungroup() 
#   
#   elements_only %>% 
#     dplyr::select(mineral_name, mineral_id, max_age, min_age, mindat_id, locality_longname, age_type) %>%
#     dplyr::rename(max_age_locality = max_age,
#                   min_age_locality = min_age) %>%
#     dplyr::ungroup() -> locality_info
#   
#   elements_only %>%
#     dplyr::group_by(mineral_name) %>%
#     dplyr::summarize(overall_max = max(max_age)) %>% 
#     dplyr::rename(max_age = overall_max) %>%
#     dplyr::left_join(elements_only %>% dplyr::select(-min_age, -max_age)) %>%
#     dplyr::left_join(rruff_separated) %>%
#     dplyr::select(mineral_name, mineral_id, ima_chemistry, rruff_chemistry, chemistry_elements, num_localities_mineral, max_age) %>%
#     dplyr::ungroup() %>% 
#     dplyr::distinct() -> elements_only_age
#   #     > names(elements_only_age)
#   #     [1] "mineral_name"           "mineral_id"             "ima_chemistry"         
#   #     [4] "rruff_chemistry"        "chemistry_elements"     "num_localities_mineral"
#   #     [7] "max_age"  
#   
#   readr::write_csv(elements_only_age, "elements_only_age.csv")
#   readr::write_csv(locality_info, "locality_info.csv")
#   list("elements_only_age" = elements_only_age, "locality_info" = locality_info)
#   
# }
# 
# obtain_network_information <- function(elements_only_age, elements_by_redox)
# {      
#   
#   chem <- rruff %>% 
#     dplyr::select(mineral_name, ima_chemistry, rruff_chemistry) %>%
#     dplyr::distinct()
#   # names():  mineral_name ima_chemistry  rruff_chemistry 
#   network_information <- elements_only_age %>%    
#     dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, chemistry_elements) %>%
#     dplyr::distinct() %>%
#     tidyr::separate_rows(chemistry_elements,sep=" ") %>%
#     dplyr::rename(element = chemistry_elements) %>%
#     dplyr::left_join(element_info, by = "element") %>%
#     dplyr::left_join(element_redox_states, by = c("element", "mineral_name")) %>%
#     dplyr::left_join(chem, by = "mineral_name") %>%
#     dplyr::group_by(mineral_name) %>%
#     dplyr::mutate(mean_pauling = mean(pauling),
#            #sd_pauling   = sd(pauling),
#            cov_pauling = stats::sd(pauling) / mean_pauling ) %>%
#     dplyr::ungroup()
#   
#   if (elements_by_redox)
#   {
#     network_information %<>%
#       dplyr::mutate(base_element = element, 
#              element_redox_mineral_sign = dplyr::case_when(element_redox_mineral == 0 ~ "+",
#                                                     element_redox_mineral == abs(element_redox_mineral) ~ "+",
#                                                     element_redox_mineral != abs(element_redox_mineral) ~ "-"),
#              element = dplyr::if_else(is.na(element_redox_mineral), paste0(element, "  "), 
#                               paste0(element, element_redox_mineral_sign, abs(element_redox_mineral)))) %>%
#       dplyr::select(-element_redox_mineral_sign) %>%
#       dplyr::distinct() %>%
#       dplyr::ungroup()    
#   }
# 
#   network_information %<>%
#     dplyr::group_by(element) %>%
#     dplyr::mutate(element_redox_network = mean(element_redox_mineral, na.rm=TRUE),#### TODO: KEEP OR CHUCK NA.RM???
#            num_localities_element = sum(num_localities_mineral)) %>% 
#     dplyr::ungroup() %>%
#     dplyr::group_by(element, mineral_name) %>%
#     dplyr::mutate(element_redox_mineral = mean(element_redox_mineral, na.rm=TRUE), #### TODO: KEEP OR CHUCK NA.RM???
#            element_redox_mineral = ifelse(is.nan(element_redox_mineral), NA, element_redox_mineral),
#            element_redox_network = ifelse(is.nan(element_redox_network), NA, element_redox_network)) %>%
#     dplyr::ungroup() %>%
#     dplyr::distinct()
#   #     > names(network_information)
#   #      [1] "mineral_name"           "mineral_id"             "num_localities_mineral"
#   #      [4] "max_age"                "element"                "element_hsab"          
#   #      [7] "AtomicMass"             "NumberofProtons"        "TablePeriod"           
#   #     [10] "TableGroup"             "AtomicRadius"           "pauling"               
#   #     [13] "MetalType"              "Density"                "SpecificHeat"          
#   #     [16] "element_name"           "element_redox_mineral"  "ima_chemistry"         
#   #     [19] "mean_pauling"           "cov_pauling"            "element_redox_network" 
#   #     [22] "num_localities_element"
#   
#   network_information    
# }
# 
# 
# construct_network   <- function(network_information, elements_by_redox)
# {
#   
#   network_data <- network_information %>% 
#     dplyr::select(mineral_name, element) %>%
#     dplyr::distinct()
#   
#   element_network <- igraph::graph.data.frame(network_data, directed=FALSE)
#   igraph::V(element_network)$type <- igraph::bipartite_mapping(element_network)$type 
#   #clustered_net <- community_detect_network(element_network, cluster_algorithm)
#   deg <- igraph::degree(element_network)
#   closeness <- igraph::closeness(element_network)
# 
#   
#   ### 1 row per VERTEX, to be joined with nodes
#   mineral_information <- network_information %>% 
#     dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, mean_pauling, cov_pauling, rruff_chemistry, ima_chemistry) %>% #sd_pauling,
#     dplyr::rename(item = mineral_name) %>%
#     dplyr::distinct() 
# 
#   
#   ## TWO COLUMNS, ID IS NOW UNIQUE NODE
#   element_information <- network_information %>% 
#     dplyr::select(-mineral_name, -mineral_id, -num_localities_mineral, -max_age, -mean_pauling, -cov_pauling, -rruff_chemistry, -ima_chemistry, -element_redox_mineral) %>% 
#     dplyr::rename(id = element) %>%
#     dplyr::distinct() %>%
#     dplyr::mutate(element_hsab = factor(element_hsab, levels = element_hsab_levels),
#                   TablePeriod  = factor(TablePeriod),
#                   TableGroup   = factor(TableGroup))
#   
#   
#   
#   #vertex_information <- left_join( tibble("item" = clustered_net$names, "cluster_ID"= as.numeric(clustered_net$membership)),
#   #                                 tibble("item" = names(deg), "network_degree"= as.numeric(deg), "closeness" = as.numeric(closeness)) ) %>%
#   
#   vertex_information <- tibble::tibble("item" = names(deg), "network_degree"= as.numeric(deg), "closeness" = as.numeric(closeness)) %>%
#     dplyr::mutate(type = ifelse(item %in% network_information$mineral_name, "mineral", "element")) %>%
#     dplyr::group_by(type) %>%
#     dplyr::mutate(network_degree_norm = network_degree / max(network_degree)) %>%
#     dplyr::ungroup() %>%        
#     dplyr::left_join(mineral_information) %>%  
#     dplyr::rename(id = item) %>% ##???
#     dplyr::left_join(element_information) %>%
#     dplyr::mutate(num_localities = ifelse(is.na(num_localities_mineral), num_localities_element, num_localities_mineral)) %>%
#     dplyr::select(-num_localities_mineral, -num_localities_element)
# 
#   net <- visNetwork::toVisNetworkData(element_network)
# 
#   edges <- tibble::as_tibble(net$edges) %>%
#     dplyr::bind_cols(network_information) %>% 
#     dplyr::left_join(mineral_information) %>% 
#     dplyr::select(-item) %>%
#     dplyr::distinct()
# 
#   charadd <- 0
#   if (elements_by_redox){
#     charadd <- 2
#   }
#   
#   nodes <- tibble::as_tibble(net$nodes) %>%
#     dplyr::mutate(type = ifelse(type == FALSE, "mineral", "element")) %>% 
#     dplyr::left_join(vertex_information) %>%
#     dplyr::ungroup() %>%
#     dplyr::rename(group = type) %>% 
#     dplyr::mutate(label = dplyr::case_when(group == "mineral"                     ~ label,
#                                            group == "element" & nchar(label) == 1+charadd ~ paste0(" ", label, " "),
#                                            group == "element" & nchar(label) == 2+charadd ~ paste0(" ", label),
#                                            group == "element" & nchar(label) == 3+charadd ~ label),  
#            title = dplyr::case_when(group == "mineral" ~ paste0("<p>", 
#                                                          id, "<br>", 
#                                                          ima_chemistry, "<br>", 
#                                                          paste0("Maximum known age: ", max_age, " Ga"), "</p>"), #!!!!!!!!! HELP
#                              group == "element" & elements_by_redox == TRUE ~ "",  
#                              group == "element" & elements_by_redox == FALSE ~ paste0("<p>", 
#                                                                                       element_name, "<br>", 
#                                                                                       ifelse(is.na(AtomicMass), "", paste0("Atomic mass: ", AtomicMass)), "<br>",  
#                                                                                       ifelse(is.na(pauling), "", paste0("Electronegativity: ", pauling)), "</p>")),                   
#            font.face = "courier") %>%
#     dplyr::distinct() 
#   
#   if (elements_by_redox)
#   {
#     nodes %<>% 
#       dplyr::mutate(title = ifelse(group == "mineral", title, paste0("<p>", 
#                                                               element_name, "<br>",
#                                                               ifelse(is.na(element_redox_network), "", paste0("Redox state: ", element_redox_network)), "<br>",    
#                                                               ifelse(is.na(AtomicMass), "", paste0("Atomic mass: ", AtomicMass)), "<br>",  
#                                                               ifelse(is.na(pauling), "", paste0("Electronegativity: ", pauling)), "</p>")))
#   }
#   
#   return (list("nodes" = nodes, "edges" = edges, "graph" = element_network))
# }
