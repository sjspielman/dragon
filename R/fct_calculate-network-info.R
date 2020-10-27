#' Calculate network connectivity
#' 
#' @param graph igraph network object
#' @returns Numeric value of network connectivity
#' @noRd
calculate_connectivity <- function(graph)
{
  # cpp weirdness ??
  #igraph::vertex_connectivity(graph)
  igraph::is_connected(graph)
}


#' Calculate network modularity based on community cluster membership
#' 
#' @param membership Cluster node memberships as calculated by the `membership` attribute of a clustered igraph network
#' @returns Numeric value of network modularity, rounded to 4 digits
#' @noRd
calculate_modularity <- function(membership)
{
  round(max(membership$modularity), 4) 
}


#' Calculate the number of edges and nodes in a given network
#' 
#' @param nodes A tibble of all nodes in network
#' @param edges A tibble of all edges in network
#' @param elements_by_redox A logical indicating if element nodes are separated by redox state  (TRUE) or all redox states for a given element equate to one node (FALSE)
#' @returns List of numeric quantities "n_mineral_nodes", "n_element_nodes", "n_edges", and "n_base_elements" which is NA if elements_by_redox=FALSE
#' @noRd
calculate_number_nodes_edges <- function(nodes, edges, elements_by_redox)
{

  n_edges <- nrow(edges)
  
  nodes %>%
    dplyr::count(group) -> n_nodes
  n_mineral_nodes <- n_nodes$n[n_nodes$group == "mineral"]
  n_element_nodes <- n_nodes$n[n_nodes$group == "element"]

  n_base_elements <- NA
  if (elements_by_redox)
  {
    nodes %>% 
      dplyr::filter(group == "element") %>%
      dplyr::select(element_name) %>%
      dplyr::distinct() %>%
      nrow() -> n_base_elements
  }
  return (list("n_mineral_nodes" = n_mineral_nodes,
               "n_element_nodes" = n_element_nodes,
               "n_base_elements" = n_base_elements,
               "n_edges"         = n_edges
               )
          ) 
}
  
