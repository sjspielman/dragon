## "Selected Nodes" table



## Table in "Network Information" tabPanel
build_network_information_table <- function(nodes)
{
  nodes %>% 
    dplyr::select(id, group, max_age, num_localities, cluster_ID, network_degree, network_degree_norm, closeness, element_redox_network, pauling, mean_pauling, cov_pauling) %>% #sd_pauling
    dplyr::arrange(group, id) %>%
    dplyr::mutate(group = stringr::str_to_title(group)) %>%
    dplyr::rename(!! variable_to_title[["id"]] := id,
                  !! variable_to_title[["group"]] := group,
                  !! variable_to_title[["cluster_ID"]] := cluster_ID,
                  !! variable_to_title[["network_degree"]] := network_degree,
                  !! variable_to_title[["network_degree_norm"]] := network_degree_norm,
                  !! variable_to_title[["closeness"]] := closeness, 
                  !! variable_to_title[["max_age"]] := max_age, 
                  !! variable_to_title[["element_redox_network"]] := element_redox_network,
                  !! variable_to_title[["num_localities"]] := num_localities,
                  !! variable_to_title[["pauling"]] := pauling,
                  !! variable_to_title[["mean_pauling"]] := mean_pauling,
                  !! variable_to_title[["cov_pauling"]] := cov_pauling) %>%
  dplyr::distinct() 
}