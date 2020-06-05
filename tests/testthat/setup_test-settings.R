## Prepare data for use in test suite
## Note: Testing also _does_ test these functions. 

## Make testdata files accessible ---------
testdata_path <- system.file("testdata",package="dragon")
source(file.path(testdata_path, "prepare_testdata.R"))


## Read in testdata files and define some variables -------------------
true_graph <- igraph::read_graph(file.path(testdata_path, "graph_by_redox.igraph"), format = "ncol")
true_graph_louvain <- igraph::cluster_louvain(true_graph)
true_nodes <- readr::read_csv(file.path(testdata_path, "nodes_by_redox.csv"), col_types = readr::cols())
true_edges <- readr::read_csv(file.path(testdata_path, "edges_by_redox.csv"), col_types = readr::cols())
true_mineral_nodes <- readr::read_csv(file.path(testdata_path, "true_mineral_nodes.csv"), col_types = readr::cols())
true_styled_nodes  <- readr::read_csv(file.path(testdata_path, "styled_nodes.csv"), col_types = readr::cols())
true_styled_edges  <- readr::read_csv(file.path(testdata_path, "styled_edges.csv"), col_types = readr::cols())

## Variables about the graph to be tested --------------------------
true_modularity      <- 0.4432351 
true_connectivity    <- 1
true_n_mineral_nodes <- 70
true_n_base_elements <- 30
true_n_element_nodes <- 40
true_n_edges         <- 258

## Dynamic overrides for testing 
true_element_color_by_dynamic_type <- "pauling"
true_element_color_by_dynamic_vals <- c("#AD0A14", "#CF1D1F", "#F34C35", "#F5533A", "#FEE5D9", "#FB7150", "#A50511", "#E23027", "#F34C35", "#FC9170", "#FB6F4F", "#FC9373", "#F5543B", "#F34C35", "#FB704F", "#FC7D5C", "#F5533A", "#F96244", "#FC9373", "#F5533A", "#E83529", "#EC392B", "#EA372A", "#FA6748", "#FB6D4D", "#FB7150", "#FB7150", "#FB7150", "#99000D", "#FC9373", "#FB6D4D", "#FECAB5", "#F5543B", "#FC9170", "#FCBFA6", "#F96144", "#F7593E", "#FB6F4F", "#F5543B", "#F45139")
true_mineral_color_by_dynamic_type <- "max_age"
true_mineral_color_by_dynamic_vals <- c("#084594", "#C1D9ED", "#C1D9ED", "#084594", "#C1D9ED", "#084594", "#EFF3FF", "#D3E2F4", "#D3E2F4", "#084594", "#084594", "#084594", "#084594", "#084594", "#EFF3FF", "#D3E2F4", "#C1D9ED", "#EFF3FF", "#084594", "#D3E2F4", "#084594", "#C1D9ED", "#084594", "#084594", "#D3E2F4", "#EFF3FF", "#084594", "#D3E2F4", "#C1D9ED", "#D3E2F4", "#C1D9ED", "#EFF3FF", "#C1D9ED", "#084594", "#C1D9ED", "#EFF3FF", "#D3E2F4", "#084594", "#C1D9ED", "#C1D9ED", "#D3E2F4", "#084594", "#D3E2F4", "#D3E2F4", "#EFF3FF", "#084594", "#EFF3FF", "#C1D9ED", "#084594", "#084594", "#084594", "#EFF3FF", "#D3E2F4", "#C1D9ED", "#D3E2F4", "#084594", "#EFF3FF", "#084594", "#084594", "#084594", "#D3E2F4", "#084594", "#D3E2F4", "#EFF3FF", "#084594", "#D3E2F4", "#EFF3FF", "#084594", "#C1D9ED", "#D3E2F4")

true_element_size_by_dynamic_type <- "pauling"
true_element_size_by_dynamic_vals <- c(35.72667, 45.94769, 57.25300, 58.52232, 80.00000, 63.54519, 32.29411, 51.45335, 57.25300, 68.75550, 63.22849, 69.17642, 58.70026, 57.25300, 63.38713, 65.55007, 58.52232, 61.11054, 69.17642, 58.52232, 52.94687, 53.97350, 53.36131, 61.93779, 62.90946, 63.54519, 63.54519, 63.54519, 20.00000, 69.17642, 62.90946, 76.70333, 58.70026, 68.75550, 75.23026, 60.94308, 59.57793, 63.22849, 58.70026, 58.16397)
true_mineral_size_by_dynamic_type <- "max_age"
true_mineral_size_by_dynamic_vals <- c(5.00000, 27.51210, 27.52974, 5.00000, 27.51210, 5.00000, 30.00000, 28.52810, 28.52810, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 30.00000, 28.52810, 27.52974, 30.00000, 5.00000, 28.52810, 5.00000, 27.51210, 5.00000, 5.00000, 28.52810, 30.00000, 5.00000, 28.52810, 27.51210, 28.52810, 27.52974, 30.00000, 27.51210, 5.00000, 27.51210, 30.00000, 28.52810, 5.00000, 27.51210, 27.52974, 28.52810, 5.00000, 28.52810, 28.52810, 30.00000, 5.00000, 30.00000, 27.52974, 5.00000, 5.00000, 5.00000, 30.00000, 28.52810, 27.52974, 28.52810, 5.00000, 30.00000, 5.00000, 5.00000, 5.00000, 28.52810, 5.00000, 28.52810, 30.00000, 5.00000, 28.52810, 30.00000, 5.00000, 27.52974, 28.52810)

true_edge_color_by_dynamic <- "max_age"
true_edge_color_by_vals <- c("#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#D3EECD", "#D3EECD", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#D3EECD", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#D3EECD", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#D3EECD", "#D3EECD", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#EDF8E9", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#D3EECD", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#C2E7BB", "#D3EECD", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#005A32", "#D3EECD", "#D3EECD", "#D3EECD", "#EDF8E9", "#EDF8E9", "#005A32", "#005A32", "#005A32", "#C2E7BB", "#C2E7BB", "#D3EECD", "#D3EECD")

## Variables for linear model testing
true_point_color   <- red
true_point_size    <- 2.33 
true_bestfit_color <- orange