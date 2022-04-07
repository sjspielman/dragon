#' Obtain an array of focal elements from provided minerals
#' @param minerals_for_focal An array of minerals whose elements should be used as focal
#' @param med_data The MED data
#' @noRd
#' @returns Array of elements
get_focal_from_minerals <- function(minerals_for_focal, med_data = med_data_cache)
{
  med_data %>% 
    dplyr::filter(mineral_name %in% minerals_for_focal) %>% 
    tidyr::separate_rows(chemistry_elements, sep = " ") %>% 
    dplyr::select(chemistry_elements) %>% 
    dplyr::distinct() %>%
    dplyr::pull(chemistry_elements)
}


#' Initialize a mineral-chemistry network as stand-alone network rather than for embedding 
#' into the Shiny App.
#' 
#' @export
#' @param elements_of_interest An array of specified elements whose minerals should be included in
#' the network. For all elements, specify "all".
#' @param force_all_elements   A logical. If FALSE (default), minerals containing any of 
#' `elements_of_interest` will be included in network. If TRUE, only minerals with full 
#' intersection of all specified elements will be included in network.
#' @param elements_by_redox    A logical. If FALSE (default), element nodes will be constructed
#' regardless of redox state. If TRUE, creates separate node for each element's redox state, 
#' e.g. Fe2+ and Fe3+ would be separate nodes.
#' @param restrict_to_elements    A logical. If FALSE (default), constructed network will _only_ 
#' contain the specified focal element(s) 
#' @param ignore_na_redox A logical. If TRUE and elements_by_redox is TRUE, element nodes 
#' without redox states will be removed from the network.
#' @param age_range            A array of two numbers giving inclusive range of mineral ages in Ga
#'  to include in network. 
#' @param max_age_type         A string indicating how mineral ages should be assessed. 
#' If "Maximum" (default), filters minerals using maximum known ages at localities. 
#' If "Minimum", filters minerals using minimum known ages at localities. 
#' @param cluster_algorithm    A string giving community clustering algorithm, one of 
#' "Louvain" (default) or "Leading eigenvector". 
#' @param cluster_seed   An integer giving a random seed for reproducible clustering. Default is NULL.
#' "Louvain" (default) or "Leading eigenvector". 
#' @param use_data_cache    A logical. If TRUE (default) cached Mineral Evolution Database
#'  will be used to build the network. If FALSE, data will be fetched from MED here. CAUTION: Requires
#'  internet connection and will take several minutes to update.
#' 
#' @returns Named list containing an igraph-formatted network (`network'), an 
#' igraph::communities object giving node cluster memberships (`clustering'), a tibble of nodes
#' associated metadata (`nodes'), and a tibble of edges and associated metadata (`edges'), and a tibble of mineral locality information (`locality_info`)
#' 
#' @examples
#' \dontrun{
#' # Include all Iron minerals whose maximum known age is between 1-2 Gya, and apply 
#' #   Louvain community clustering
#' initialize_network("Fe", age_range = c(1,2))
#'
#' # Include all minerals containing either Iron and Oxygen whose maximum known age 
#' #   is between 1-2 Gya
#' initialize_network(c("Fe", "O"), age_range = c(1,2))
#'
#' # Include all minerals containing both Iron and Oxygen whose maximum known age is 
#' #   between 1-2 Gya
#' initialize_network(c("Fe", "O"), force_all_elements = TRUE, age_range = c(1,2))
#'
#' # Build the full mineral network
#' initialize_network("all")
#'}
initialize_network <- function(elements_of_interest, 
                               force_all_elements = FALSE, 
                               elements_by_redox = FALSE, 
                               restrict_to_elements = FALSE,
                               ignore_na_redox = FALSE,
                               age_range         = c(0, 5),
                               max_age_type      = "Maximum",
                               cluster_algorithm = "Louvain",
                               cluster_seed      = NULL,
                               use_data_cache    = TRUE)
{
  # TODO: Weighted values and exclusions when updating from MED? Should it be incorporated? For now, we just tell them to be aware.
  if (use_data_cache)
  {
    med_data <- med_data_cache
    element_redox_states <- element_redox_states_cache
  } else {
    print("ALERT:  Downloading data from Mineral Evolution Database. Please be patient! This may/will take several minutes, or longer depending on your internet connection.")
    med_data <- fetch_med_data()
    if(med_data == FALSE){
      print("WARNING: Could not connect to the Mineral Evolution Database. Using the cached data.")
      element_redox_states <- element_redox_states_cache
    } else {
      element_redox_states <- calculate_element_redox_states(med_data)
      print("....Done downloading! Building network now. Please be aware that this data has not been fully 'vetted' by dragon maintainers.")
    }
  }
  
  if(stringr::str_to_lower(elements_of_interest) == "all"){
    elements_of_interest <- element_info$element
  }
  age_range <- sort(age_range)
  
  subset_med <- initialize_data(med_data, element_redox_states, elements_of_interest, force_all_elements, restrict_to_elements)
  if (nrow(subset_med) == 0) stop("Network cannot be constructed with provided elements.")

  age_data    <- initialize_data_age(subset_med, age_range, max_age_type)
  if (nrow(age_data$elements_only_age) == 0) stop("Network cannot be constructed at specified age range.")
  
  network_raw <- construct_network(age_data$elements_only_age, elements_by_redox, ignore_na_redox, element_redox_states)
  if (nrow(network_raw$nodes) == 0) stop("Network could not be constructed. Please adjust input settings.")
  if (nrow(network_raw$edges) == 0) stop("Network could not be constructed. Please adjust input settings.")
  
  clustered   <- specify_community_detect_network(network_raw$network, 
                                                  network_raw$nodes, 
                                                  "Louvain", 
                                                  cluster_seed)
  return(list("network" = network_raw$network,
              "nodes"  =  clustered$nodes,
              "edges"  =  network_raw$edges,
              "locality_info" = age_data$locality_info,
              "clustering" = clustered$clustered
  )
  )
}



#' Subset MED data to contain only elements of interest for network construction
#'
#' @param med_data Input Mineral Evolution Database data
#' @param element_redox_states A tibble of elements and their associated redox states per mineral
#' @param elements_of_interest A array of specified elements whose minerals should be included in the network. 
#' For all elements, specify "all".
#' @param force_all_elements   A logical. If FALSE (default), minerals containing any of `elements_of_interest` 
#' will be included in network. If TRUE, only minerals with full intersection of all specified elements will be included in network.
#' @param restrict_to_elements A logical. If FALSE (default), constructed network will _only_ 
#' contain the specified focal element(s) 
#' @param max_age_type   A string indicating how mineral ages should be assessed. If "Maximum" (default), filters minerals using maximum known ages at localities. If "Minimum", filters minerals using minimum known ages at localities. 
#'
#' @returns Tibble subsetted from MED containing only those minerals with specified element settings
#' @noRd
initialize_data <- function(med_data, element_redox_states, elements_of_interest, force_all_elements = FALSE, restrict_to_elements = FALSE)
{ 
  
  ## Must have all elements
  if (force_all_elements)
  {
    n_elements <- length(elements_of_interest)
    element_redox_states %>% 
      dplyr::select(-element_redox_mineral) %>%
      dplyr::group_by(mineral_name) %>%
      dplyr::distinct() %>% ##!!!!!!!!
      dplyr::mutate(has_element = ifelse( sum(element %in% elements_of_interest) == n_elements, TRUE, FALSE)) %>% 
      dplyr::filter(has_element == TRUE) %>%
      dplyr::select(-has_element) -> elements_only_raw
    
    if (restrict_to_elements){
      
      # number of elements per mineral
      elements_only_raw %>%
        dplyr::count(mineral_name) -> elements_per_mineral
      
      # number of elements per mineral when only the elements_of_interest are considered.
      # only keep intersection
      elements_only_raw %>% 
        dplyr::filter(element %in% elements_of_interest) %>% 
        dplyr::count(mineral_name) %>%
        dplyr::intersect(elements_per_mineral) %>%
        dplyr::pull(mineral_name) -> minerals_to_keep
      
      elements_only_raw %>%
        dplyr::filter(mineral_name %in% minerals_to_keep) -> elements_only_raw
    }
  } else 
  { ## Has at least one element
    element_redox_states %>% 
      dplyr::select(-element_redox_mineral) %>%
      dplyr::group_by(mineral_name) %>%
      dplyr::filter(element %in% elements_of_interest) -> elements_only_raw
    
    if (restrict_to_elements) {
      element_redox_states %>% 
        dplyr::select(-element_redox_mineral) %>%
        dplyr::group_by(mineral_name) %>%
        dplyr::filter(!(element %in% elements_of_interest)) %>%
        dplyr::select(mineral_name) %>%
        dplyr::distinct() %>%
        dplyr::pull(mineral_name) -> exclude_these_minerals
      
      elements_only_raw %>%
        dplyr::filter(!(mineral_name %in% exclude_these_minerals)) -> elements_only_raw
    }
  }
  
  elements_only_raw %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral_name) %>%
    dplyr::inner_join(med_data) 
}

#' Subset element-filtered data to contain only minerals in specified age range
#' 
#' @param elements_only A tibble containing all minerals and associated information which contain specified elements
#' @param age_range            A array of two numbers giving inclusive range of mineral ages in Ga to include in network. 
#' @returns Named list of two tibbles: 'elements_only_age' is subsetted MED data to specified age range, and 'locality_info' contains all locality information for minerals in 'elements_age_only'
#' @noRd
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
#' @param ignore_na_redox A logical. If TRUE and elements_by_redox is TRUE, element nodes without redox states will be removed from the network.
#' @param element_redox_states A tibble of elements and their associated redox states per mineral
#'
#' @returns Named list with completed network: 'nodes' is a tibble of all node info, 'edges' is a tibble of all edge info, and 'network' is the igraph::graph object
#' @noRd
construct_network   <- function(elements_only_age, elements_by_redox, ignore_na_redox, element_redox_states)
{
  
  ## Merge data with redox states, element_info, crust abundances, and some associated processing --------------------------------
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
    # element_info and crust (one tibble now)
    dplyr::left_join(element_info, by = "element") %>%
    ## Factor some of the joined in columns from element_info
    dplyr::mutate(element_hsab = factor(element_hsab, levels = c("Hard acid", "Int. acid", "Soft acid", "Soft base", "Int. base", "Hard base")),
                  element_table_period  = factor(element_table_period),
                  element_table_group   = factor(element_table_group)) %>%
    ## Calculate electronegativity quantities - so we need to merge in element_info at this time
    dplyr::group_by(mineral_name) %>%
    dplyr::mutate(mean_pauling = mean(pauling),
                  cov_pauling  = stats::sd(pauling) / mean_pauling ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::left_join(final_weighted_pauling) %>%
    dplyr::rename(from = mineral_name) -> network_information
  
  
  ## Remove element nodes without redox, IF elements_by_redox==TRUE and ignore_na_redox==TRUE
  if(elements_by_redox == TRUE & ignore_na_redox == TRUE)
  {
    network_information %<>% 
      tidyr::drop_na(element_redox_mineral)
    
    # each element node should have a +/- in it now
    stopifnot(sum( stringr::str_detect(network_information$element_as_redox, "[\\+-]")) == nrow(network_information))
  }
  
  
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
    dplyr::select(from, to, element_redox_mineral, max_age, num_localities_mineral, mean_pauling, w_mean_pauling, cov_pauling, w_cov_pauling) %>%
    dplyr::distinct() -> edge_metadata
                     
                     
  ## Node metadata ------------------------------------------------
  network_information %>% 
    dplyr::select(from, mineral_id, max_age, num_localities_mineral, ima_chemistry, rruff_chemistry, mean_pauling, w_mean_pauling, cov_pauling, w_cov_pauling) %>% 
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
    dplyr::select(to, element_name, from, num_localities_mineral, element_hsab, atomic_mass, number_of_protons, element_table_period, element_table_group, atomic_radius, pauling, element_metal_type, element_density, element_specific_heat, element_crust_percent_weight) %>%
    dplyr::distinct() %>% ## this line was causing bugs in element localities. FIXED BY SELECTING FROM ABOVE!
    dplyr::group_by(to) %>%
    dplyr::mutate(num_localities_element = sum(num_localities_mineral)) %>% 
    dplyr::select(-num_localities_mineral, -from) %>% ## remove from as well!!
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
    dplyr::distinct() %>%
    add_shiny_node_titles(elements_by_redox) -> nodes
  

  ## Merge edges with associated data ---------------------------------------------
  network_to_from %>% 
    dplyr::inner_join(edge_metadata) %>%
    dplyr::mutate(mineral_name = from) -> edges
  
  return (list("nodes" = nodes, "edges" = edges, "network" = element_network))
}



#' Perform community clustering according to specified algorithm
#'
#' @param network A network in igraph format
#' @param nodes   A tibble containing all node information and metadata
#' @param cluster_algorithm    A string giving community clustering algorithm, one of "Louvain" (default) or "Leading eigenvector". 
#' @param cluster_seed   An integer giving a random seed for reproducible clustering. Default is NULL.
#' @returns A named list of 'nodes' tibble updated to containing 'cluster_ID' and 'cluster_algorithm' columns, and 'clustered_net' which is an igraph::communities object
#' @noRd
specify_community_detect_network <- function(network, nodes, cluster_algorithm, cluster_seed = NULL)
{
  if (!(cluster_algorithm %in% cluster_algorithm_choices)) stop("Cluster algorithm must be one of either 'Louvain' or 'Leading eigenvector'.")
  
  print(cluster_seed)
  if (!is.null(cluster_seed)) set.seed(cluster_seed)
  if (cluster_algorithm == cluster_alg_louvain_str) clustered_net <- igraph::cluster_louvain(network)
  if (cluster_algorithm == cluster_alg_eig_str)     clustered_net <- igraph::cluster_leading_eigen(network)

  ## Update nodes ----------------------------
  tibble::tibble( "id"                = clustered_net$names, 
                  "cluster_ID"        = as.factor(clustered_net$membership), 
                  "cluster_algorithm" = cluster_algorithm) %>%
    dplyr::right_join(nodes, by = "id") -> nodes 

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
#' @returns An updated nodes tibble containing additional columns 'title' and 'font.face', and the 'label' column is fixed width
#' @noRd
add_shiny_node_titles <- function(nodes, elements_by_redox)
{
  charadd <- 0
  if (elements_by_redox){
    charadd <- 2
  }
  ## Because node size for elements is determined by the LABEL SIZE, we need to ensure:
  ## if elements_by_redox is FALSE, we want length THREE for labels
  ## if elements_by_redox is TRUE, we want length FIVE for labels
  
  nodes %>%
   # dplyr::rowwise() %>%
    dplyr::mutate(font.face = "courier",
                  label = dplyr::case_when(elements_by_redox == FALSE & nchar(label) == 1 ~ paste0(" ", label, " "), ## 1 / 1 / 1
                                           elements_by_redox == FALSE & nchar(label) == 2 ~ paste0(" ", label),      ## 1 / 2
                                           elements_by_redox == FALSE & nchar(label) == 3 ~ label, 
                                           TRUE ~ label),
                  label = dplyr::case_when(elements_by_redox == TRUE & nchar(label) == 1 ~ paste0("  ", label, "  "), ## 2 / 1 / 2
                                           elements_by_redox == TRUE & nchar(label) == 2 ~ paste0("  ", label, " "),  ## 2 / 2 / 1
                                           elements_by_redox == TRUE & nchar(label) == 3 ~ paste0(" ", label, " "),   ## 1 / 3 / 1
                                           elements_by_redox == TRUE & nchar(label) == 4 ~ paste0(" ", label),        ## 1 / 4
                                           elements_by_redox == TRUE & nchar(label) == 5 ~ label, 
                                           TRUE ~ label),                                               
                  title = dplyr::case_when(group == "mineral"  ~ paste0("<p>", 
                                                                        id, "<br>", 
                                                                        ima_chemistry, "<br>", 
                                                                        paste0("Maximum known age: ", max_age, " Ga"), "<br>",
                                                                        paste0("Number of known localities: ", num_localities), "<br>",
                                                                        paste0("Mean electronegativity: ", round(mean_pauling, 3)), "<br>",
                                                                        paste0("COV electronegativity: ", round(cov_pauling, 3)), "<br>",
                                                                        "</p>"),
                                          group == "element" & elements_by_redox == TRUE ~ 
                                            paste0("<p>", 
                                              element_name, "<br>",
                                              ifelse(is.na(element_redox_network), 
                                                     "", 
                                                     paste0("Redox state: ", element_redox_network)
                                               ),"<br>",    
                                              ifelse(is.na(atomic_mass), 
                                                     "", 
                                                     paste0("Atomic mass: ", atomic_mass)), "<br>",  
                                              ifelse(is.na(pauling), 
                                                     "", 
                                                     paste0("Electronegativity: ", pauling)), "<br>",
                                              paste0("Number of known localities: ", num_localities),  "</p>"),                                              
                                          group == "element" & elements_by_redox == FALSE ~ 
                                            paste0("<p>", 
                                               element_name, "<br>", 
                                               ifelse(is.na(element_redox_network), 
                                                    "", 
                                                    paste0("Mean network redox state: ", round(element_redox_network, 3))), "<br>",  
                                               ifelse(is.na(atomic_mass), 
                                                      "", 
                                                      paste0("Atomic mass: ", atomic_mass)), "<br>",  
                                               ifelse(is.na(pauling), 
                                                      "", 
                                                      paste0("Electronegativity: ", pauling)), "<br>",
                                              paste0("Number of known localities: ", num_localities),  "</p>")                                                        
                  ), ## END title case_when                     
                 ) %>% ## END mutate
    dplyr::distinct() -> nodes_shinied
  
  ## Check there are no NAs and label length is right
  num_na_label <- sum(is.na(nodes_shinied$label))
  stopifnot(num_na_label == 0)
  
  nodes_shinied %>%
    dplyr::filter(group == "element") %>% 
    dplyr::pull(label) -> shiny_labels
  
  if (elements_by_redox) stopifnot(all(nchar(shiny_labels) == 5))
  if (!elements_by_redox) stopifnot(all(nchar(shiny_labels) == 3))
  
  
  nodes_shinied
  
}



#' Subset nodes to mineral nodes used in linear modeling, and rename all columns _except_ community cluster
#'
#' @param nodes   A tibble containing all node information and metadata
#' @returns An tibble of mineral node data used in linear models
#' @noRd
subset_mineral_nodes <- function(nodes)
{
  nodes %>%
    dplyr::filter(group == "mineral") %>%
    dplyr::select(cluster_ID, network_degree_norm, closeness, num_localities, max_age, w_mean_pauling, mean_pauling, w_cov_pauling, cov_pauling) %>% 
    rename_for_ui() %>%
    ## rename for UI **EXCEPT** cluster_ID
    dplyr::rename(cluster_ID = cluster_ID_str) %>%
    dplyr::mutate(cluster_ID = factor(cluster_ID)) 
}
