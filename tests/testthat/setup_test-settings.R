## Prepare data for use in test suite
## Note: Testing also _does_ test these functions. 


## Read in testdata CSV files and graph  --------------------------------
true_nodes <- readr::read_csv("nodes_by_redox.csv.zip", col_types = readr::cols())
true_edges <- readr::read_csv("edges_by_redox.csv.zip", col_types = readr::cols())
true_graph <- igraph::read_graph("graph_by_redox.igraph", format = "ncol")
true_graph_louvain <- igraph::cluster_louvain(true_graph)

true_mineral_nodes <- readr::read_csv("true_mineral_nodes.csv.zip", col_types = readr::cols(cluster_ID = readr::col_factor()))
true_styled_nodes  <- readr::read_csv("styled_nodes.csv.zip", col_types = readr::cols())
true_styled_edges  <- readr::read_csv("styled_edges.csv.zip", col_types = readr::cols())
true_locality_info <- readr::read_csv("locality_info.csv.zip", col_types = readr::cols())

## Variables about the graph to be tested --------------------------------
true_modularity      <- 0.4432351 
true_connectivity    <- 1
true_n_mineral_nodes <- 70
true_n_base_elements <- 30
true_n_element_nodes <- 40
true_n_edges         <- 258



## Dynamic overrides for testing ----------------------------
true_element_color_by_dynamic_type <- "pauling"
true_element_color_by_dynamic_vals <- c("#AD0A14", "#CF1D1F", "#F34C35", "#F5533A", "#FEE5D9", "#FB7150", "#A50511", "#E23027", "#F34C35", "#FC9170", "#FB6F4F", "#FC9373", "#F5543B", "#F34C35", "#FB704F", "#FC7D5C", "#F5533A", "#F96244", "#FC9373", "#F5533A", "#E83529", "#EC392B", "#EA372A", "#FA6748", "#FB6D4D", "#FB7150", "#FB7150", "#FB7150", "#99000D", "#FC9373", "#FB6D4D", "#FECAB5", "#F5543B", "#FC9170", "#FCBFA6", "#F96144", "#F7593E", "#FB6F4F", "#F5543B", "#F45139")
true_mineral_color_by_dynamic_type <- "max_age"
true_mineral_color_by_dynamic_vals <- c("#EFF3FF", "#2775B7", "#2675B7", "#EFF3FF", "#2775B7", "#EFF3FF", "#084594", "#1B63AB", "#1B63AB", "#EFF3FF", "#EFF3FF", "#EFF3FF", "#EFF3FF", "#EFF3FF", "#084594", "#1B63AB", "#2675B7", "#084594", "#EFF3FF", "#1B63AB", "#EFF3FF", "#2775B7", "#EFF3FF", "#EFF3FF", "#1B63AB", "#084594", "#EFF3FF", "#1B63AB", "#2775B7", "#1B63AB", "#2675B7", "#084594", "#2775B7", "#EFF3FF", "#2775B7", "#084594", "#1B63AB", "#EFF3FF", "#2775B7", "#2675B7", "#1B63AB", "#EFF3FF", "#1B63AB", "#1B63AB", "#084594", "#EFF3FF", "#084594", "#2675B7", "#EFF3FF", "#EFF3FF", "#EFF3FF", "#084594", "#1B63AB", "#2675B7", "#1B63AB", "#EFF3FF", "#084594", "#EFF3FF", "#EFF3FF", "#EFF3FF", "#1B63AB", "#EFF3FF", "#1B63AB", "#084594", "#EFF3FF", "#1B63AB", "#084594", "#EFF3FF", "#2675B7", "#1B63AB")


true_element_size_by_dynamic_type <- "pauling"
true_element_size_by_dynamic_vals <- c(35.72667, 45.94769, 57.25300, 58.52232, 80.00000, 63.54519, 32.29411, 51.45335, 57.25300, 68.75550, 63.22849, 69.17642, 58.70026, 57.25300, 63.38713, 65.55007, 58.52232, 61.11054, 69.17642, 58.52232, 52.94687, 53.97350, 53.36131, 61.93779, 62.90946, 63.54519, 63.54519, 63.54519, 20.00000, 69.17642, 62.90946, 76.70333, 58.70026, 68.75550, 75.23026, 60.94308, 59.57793, 63.22849, 58.70026, 58.16397)
true_mineral_size_by_dynamic_type <- "max_age"
true_mineral_size_by_dynamic_vals <- c(5.00000, 27.51210, 27.52974, 5.00000, 27.51210, 5.00000, 30.00000, 28.52810, 28.52810, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 30.00000, 28.52810, 27.52974, 30.00000, 5.00000, 28.52810, 5.00000, 27.51210, 5.00000, 5.00000, 28.52810, 30.00000, 5.00000, 28.52810, 27.51210, 28.52810, 27.52974, 30.00000, 27.51210, 5.00000, 27.51210, 30.00000, 28.52810, 5.00000, 27.51210, 27.52974, 28.52810, 5.00000, 28.52810, 28.52810, 30.00000, 5.00000, 30.00000, 27.52974, 5.00000, 5.00000, 5.00000, 30.00000, 28.52810, 27.52974, 28.52810, 5.00000, 30.00000, 5.00000, 5.00000, 5.00000, 28.52810, 5.00000, 28.52810, 30.00000, 5.00000, 28.52810, 30.00000, 5.00000, 27.52974, 28.52810)

true_edge_color_by_dynamic <- "max_age"
true_edge_color_by_vals <- c("#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#278F48", "#278F48", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#197B3F", "#197B3F", "#278F48", "#278F48", "#278F48", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#278F48", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#197B3F", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#197B3F", "#278F48", "#278F48", "#278F48", "#278F48", "#197B3F", "#197B3F", "#278F48", "#278F48", "#278F48", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#278F48", "#278F48", "#278F48", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#278F48", "#005A32", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#197B3F", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#278F48", "#197B3F", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#197B3F", "#197B3F", "#197B3F", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#278F48", "#278F48", "#197B3F", "#197B3F")
  
  
## Variables for linear model testing --------------------------------
true_point_color   <- red
true_point_size    <- 2.33 
true_bestfit_color <- orange

## Expected variables in certain dfs -----------------------------------
true_node_names            <- sort(names(true_nodes))
true_node_names_precluster <- true_node_names[true_node_names != "cluster_ID" & true_node_names != "cluster_algorithm"]
true_edge_names            <- sort(names(true_edges))
true_styled_node_names     <- sort(names(true_styled_nodes))
true_styled_edge_names     <- sort(names(true_styled_edges))
true_locality_names        <- sort(names(true_locality_info))
true_mineral_node_names    <- sort(names(true_mineral_nodes))


initialize_data_names <- sort(c("mineral_name", "mineral_id", "mindat_id", "locality_longname" , "age_type", "rruff_chemistry", "ima_chemistry", "min_age","max_age", "chemistry_elements"))
initialize_data_age_names <- sort(c("mineral_name", "mineral_id", "max_age", "num_localities_mineral", "ima_chemistry", "rruff_chemistry", "chemistry_elements"))



expected_element_variables <- c("element_hsab", "atomic_mass", "number_of_protons", "table_period", "table_group", "atomic_radius", "pauling", "metal_type", "element_density", "element_specific_heat", "element_name", "element_redox_network")
expected_mineral_variables <- c( "mineral_id", "max_age", "ima_chemistry", "rruff_chemistry", "mean_pauling", "w_mean_pauling", "cov_pauling", "w_cov_pauling")
expected_shared_variables <- c("cluster_ID", "network_degree", "closeness", "network_degree_norm")



