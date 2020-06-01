load("R/sysdata.rda") ## TODO devtools:check() fails without explicit load. how fix? can i do library(dragon)? probably not?
# from stackoverflow?: If your package name is somepackage and the object saved was nhanes_files with devtools::use_data(nhanes_files, internal = TRUE) then you can access this in your functions by calling somepackage:::nhanes_files.

#' Maximum age possible for mineral selection
#'
#' @export
total_max_age   <- round( max(rruff$max_age) + 0.1, 1)


#' Tibble of MED minerals and associated elements, in tidy format
#'
#' @export
rruff_separated <- element_redox_states %>% dplyr::select(-element_redox_mineral)


#' Initialize a mineral-chemistry network as stand-alone network rather than for embedding into the Shiny App.
#' 
#' @param elements_of_interest A array of specified elements whose minerals should be included in the network. For all elements, specify "all".
#' @param force_all_elements   A logical. If FALSE (default), minerals containing any of `elements_of_interest` will be included in network. If TRUE, only minerals with full intersection of all specified elements will be included in network.
#' @param elements_by_redox    A logical. If FALSE (default), element nodes will be constructed regardless of redox state. If TRUE, creates separate node for each element's redox state, e.g. Fe2+ and Fe3+ would be separate nodes.
#' @param age_range            A array of two numbers giving inclusive range of mineral ages in Ga to include in network. 
#' @param max_age_type         A string indicating how mineral ages should be assessed. If "Maximum" (default), filters minerals using maximum known ages at localities. If "Minimum", filters minerals using minimum known ages at localities. 
#' @param cluster_algorithm    A string giving community clustering algorithm, one of "Louvain" (default) or "Leading eigenvector". 
#' 
#' @returns Named list containing an igraph-formatted network ('network'), an igraph-formatted list of cluster memberships ('clustering'), a tibble of nodes and associated metadata ('nodes'), and a tibble of edges and associated metadata  ('edges')
#' 
#' @examples
#' # Include all Iron minerals whose maximum known age is between 1-2 Gya, and apply Louvain clustering
#' initialize_network("Fe", age_range = c(1,2))
#'
#' # Include all minerals containing \emph{either} Iron and Oxygen whose maximum known age is between 1-2 Gya, and apply Louvain clustering
#' initialize_network(c("Fe", "O"), age_range = c(1,2))
#'
#' # Include all minerals containing \emph{both} Iron and Oxygen whose maximum known age is between 1-2 Gya, and apply Louvain clustering
#' initialize_network(c("Fe", "O"), force_all_elements = TRUE, age_range = c(1,2))
#'
#' # Build the full mineral network
#' initialize_network("all")
initialize_network <- function(elements_of_interest, 
                               force_all_elements = FALSE, 
                               elements_by_redox = FALSE, 
                               age_range         = c(0, total_max_age),
                               max_age_type      = "Maximum",
                               cluster_algorithm = "Louvain")
{
  if(stringr::str_to_lower(elements_of_interest) == "all"){
    elements_of_interest <- all_elements
  }
  age_range <- sort(age_range)
  
  subset_rruff <- initialize_data(elements_of_interest, force_all_elements)
  if (nrow(subset_rruff) == 0) stop("Network cannot be constructed with provided elements.")
  
  age_data    <- initialize_data_age(subset_rruff, age_range, max_age_type)
  if (nrow(age_data$elements_only_age) == 0) stop("Network cannot be constructed at specified age range.")

  network_raw <- construct_network(age_data$elements_only_age, elements_by_redox)
  if (nrow(network_raw$nodes) == 0) stop("Network could not be constructed. Please adjust input settings.")
  if (nrow(network_raw$edges) == 0) stop("Network could not be constructed. Please adjust input settings.")

  clustered   <- specify_community_detect_network(network_raw$graph, network_raw$nodes, "Louvain")
  return(list("network" = network_raw$graph,
               "nodes"  =  clustered$nodes,
               "edges"  =  network_raw$edges,
               "clustering" = clustered$clustered
              )
        )
}
    


#' Subset MED data to contain only elements of interest for network construction
#' 
#' @param elements_of_interest A array of specified elements whose minerals should be included in the network. For all elements, specify "all".
#' @param force_all_elements   A logical. If FALSE (default), minerals containing any of `elements_of_interest` will be included in network. If TRUE, only minerals with full intersection of all specified elements will be included in network.
#' @param max_age_type         A string indicating how mineral ages should be assessed. If "Maximum" (default), filters minerals using maximum known ages at localities. If "Minimum", filters minerals using minimum known ages at localities. 
initialize_data <- function(elements_of_interest, force_all_elements = FALSE)
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

#' Subset element-filtered data to contain only minerals in specified age range
#' 
#' @param elements_only A tibble containing all minerals and associated information which contain specified elements
#' @param age_range            A array of two numbers giving inclusive range of mineral ages in Ga to include in network. 
initialize_data_age <- function(elements_only, age_range, max_age_type)
{
  lb <- age_range[1]
  ub <- age_range[2]
  
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



#' Construct network and apply metadata to mineral, element nodes
#' 
#' @param elements_only_age A tibble containing all minerals and associated information which contain specified elements at the specified age range
#' @param elements_by_redox A logical. If FALSE, element nodes will be constructed regardless of redox state. If TRUE, creates separate node for each element's redox state, e.g. Fe2+ and Fe3+ would be separate nodes.
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
                                                              element, 
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
    dplyr::ungroup() %>%
    dplyr::rename(from = mineral_name) -> network_information
  
  
  
  ## Build igraph network
  if (elements_by_redox){
    network_information %<>% dplyr::mutate(to = element_as_redox)
  } else {
    network_information %<>% dplyr::mutate(to = element)
  }
  network_information %>% 
    dplyr::select(from, to) %>%
    dplyr::distinct() -> network_to_from
  element_network                 <- igraph::graph.data.frame(network_to_from, directed = FALSE)
  igraph::V(element_network)$type <- igraph::bipartite_mapping(element_network)$type  ## mineral is FALSE and element is TRUE
  
  ## Edge metadata ------------------------------------------------
  ## Columns that require an explicit link between minerals and elements, OR are used in edge styling
  network_information %>%
    dplyr::select(from, to, element_redox_mineral, max_age, num_localities_mineral, mean_pauling, cov_pauling) %>%
    dplyr::distinct() -> edge_metadata
                     
                     
  ## Node metadata ------------------------------------------------
  network_information %>% 
    dplyr::select(from, mineral_id, max_age, num_localities_mineral, ima_chemistry, rruff_chemistry,  mean_pauling, cov_pauling) %>% 
    dplyr::rename(id = from) %>%
    dplyr::distinct() -> mineral_metadata
  
  ## Calculate average element redox state in the whole network to be added into element_metadata
  edge_metadata %>% 
    dplyr::group_by(to) %>%
    dplyr::summarize(element_redox_network = mean(element_redox_mineral, na.rm=T)) %>% 
    dplyr::mutate(element_redox_network = ifelse(is.nan(element_redox_network), 
                                                 NA, element_redox_network)) -> element_redox_network_df
  ## Average element redox state also needs to be an edge attribute
  edge_metadata %<>%
    dplyr::left_join(element_redox_network_df)
  
  ## names to, element_redox_network
  network_information %>% 
    dplyr::select(to, element_name, num_localities_mineral, element_hsab, AtomicMass, NumberofProtons, TablePeriod, TableGroup, AtomicRadius, pauling, MetalType, Density, SpecificHeat) %>%
    dplyr::distinct() %>%
    dplyr::group_by(to) %>%
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
    dplyr::ungroup() %>%
    dplyr::left_join(mineral_metadata) %>%
    dplyr::left_join(element_metadata) %>% 
    dplyr::mutate(num_localities = dplyr::if_else(group == "mineral",  
                                                  num_localities_mineral,
                                                  num_localities_element)) %>%
    dplyr::select(-num_localities_mineral, -num_localities_element) %>%
    dplyr::distinct() -> nodes
  

  ## Merge edges with associated data ---------------------------------------------
  network_to_from %>% 
    dplyr::inner_join(edge_metadata) %>%
    dplyr::mutate(mineral_name = from) -> edges
  
  return (list("nodes" = nodes, "edges" = edges, "graph" = element_network))
}



#' Perform community clustering according to specified algorithm
#' 
#' @param network A network in igraph format
#' @param nodes   A tibble containing all node information and metadata
#' @param cluster_algorithm    A string giving community clustering algorithm, one of "Louvain" (default) or "Leading eigenvector". 
specify_community_detect_network <- function(network, nodes, cluster_algorithm)
{
  if (!(cluster_algorithm %in% allowed_cluster_algorithms)) stop("Cluster algorithm must be one of either 'Louvain' or 'Leading eigenvector'.")
  
  
  if (cluster_algorithm == cluster_alg_louvain_str) clustered_net <- igraph::cluster_louvain(network)
  if (cluster_algorithm == cluster_alg_eig_str)     clustered_net <- igraph::cluster_leading_eigen(network)

  ## Update nodes ----------------------------
  tibble::tibble( "id"                = clustered_net$names, 
                  "cluster_ID"        = as.factor(clustered_net$membership), 
                  "cluster_algorithm" = cluster_algorithm) %>%
    right_join(nodes, by = "id") -> nodes 

  return( 
    list("nodes"          = nodes, 
         "clustered_net"  = clustered_net 
         ) 
    )
  
}

#' Format labels and titles to nodes for display in Shiny App
#' 
#' @param nodes   A tibble containing all node information and metadata
#' @param elements_by_redox  A logical. If FALSE (default), element nodes have been constructed regardless of redox state. If TRUE, separate nodes have been created for each element's redox state, e.g. Fe2+ and Fe3+ would be separate nodes.
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
    dplyr::distinct()  %>% ## TODO: I THINK BELOW CAN BE RM'D??
    # Add type column for grouped node selection dropdown menu
    dplyr::mutate(type = dplyr::if_else(group == "element", 
                                        "All elements", 
                                        "All minerals")
    ) 
  
}





















