## Prepare data for use in test suite
## Note: Testing also _does_ test these functions. 

## Make testdata files accessible ---------
testdata_path <- system.file("testdata",package="dragon")

## Read in testdata CSV files and graph  --------------------------------
true_graph <- igraph::read_graph(file.path(testdata_path, "graph_by_redox.igraph"), format = "ncol")
true_graph_louvain <- igraph::cluster_louvain(true_graph)
true_nodes <- readr::read_csv(file.path(testdata_path, "nodes_by_redox.csv.zip"), col_types = readr::cols())
true_edges <- readr::read_csv(file.path(testdata_path, "edges_by_redox.csv.zip"), col_types = readr::cols())
true_mineral_nodes <- readr::read_csv(file.path(testdata_path, "true_mineral_nodes.csv.zip"), col_types = readr::cols(cluster_ID = readr::col_factor()))
true_styled_nodes  <- readr::read_csv(file.path(testdata_path, "styled_nodes.csv.zip"), col_types = readr::cols())
true_styled_edges  <- readr::read_csv(file.path(testdata_path, "styled_edges.csv.zip"), col_types = readr::cols())
true_locality_info <- readr::read_csv(file.path(testdata_path, "locality_info.csv.zip"), col_types = readr::cols())

true_node_names <- sort(names(true_nodes))
true_node_names_precluster <- true_node_names[true_node_names != "cluster_ID" & true_node_names != "cluster_algorithm"]
true_edge_names <- sort(names(true_edges))
true_styled_node_names <- sort(names(true_styled_nodes))
true_styled_edge_names <- sort(names(true_styled_edges))
true_locality_names <- sort(names(true_locality_info))
true_mineral_node_names <- sort(names(true_mineral_nodes))

## Variables about the graph to be tested --------------------------------
true_modularity      <- 0.4432351 
true_connectivity    <- 1
true_n_mineral_nodes <- 70
true_n_base_elements <- 30
true_n_element_nodes <- 40
true_n_edges         <- 258

focal <- "Fe" ## Focal element used in true data network


## Variables for style testing ---------------------------------------
blue   <- "#0000FF"
red    <- "#FF0000"
yellow <- "#FFFF00"
purple <- "#800080"
black  <- "#000000"
pink   <- "#FFC0CB"
orange <- "#FFA500"

true_cluster_palette <- "Set2"
true_n_clusters      <- 6 
true_cluster_colors  <- RColorBrewer::brewer.pal(true_n_clusters, true_cluster_palette)
true_mineral_label_size  <- 12
true_mineral_size_scale  <- 10
true_element_label_size  <- 11
true_element_size_scale  <- 20
true_mineral_size        <- 7
true_mineral_color       <- red
true_mineral_label_color <- orange
true_mineral_palette     <- "Blues"
true_element_color       <- blue
true_element_label_color <- pink
true_element_palette     <- "Reds"
true_edge_color          <- purple
true_edge_palette        <- "Greens"
true_mineral_by          <- "mean_pauling"
true_element_by          <- "pauling"
true_edge_by             <- "max_age"
true_mineral_shape       <- "square"
true_element_shape       <- "circle"
true_na_color            <- black
true_highlight_color     <- yellow
true_focal_element_name <- "Iron"
true_special_element_id <- c("P", "O-2", "H+1", "Fe+2", "Fe", "Fe+3") ## the three iron focals and P/O/H redoxes

true_custom_selection_color_1   <- orange
true_custom_selection_color_2   <- black
true_custom_selection_set_1 <- c("P", "O-2")
true_custom_selection_set_2 <- c("H+1")

true_custom_element_colors <- c("P" = true_custom_selection_color_1,
                                "O-2" = true_custom_selection_color_1,
                                "H+1" = true_custom_selection_color_2)



# Baseline list can be updated when testing other conditions. 
# Baseline conditions are **singlecolor/singlesize** with element highlights and custom selections
true_style_options <- list("color_by_cluster"  = FALSE,
                           "cluster_colors"       = true_cluster_colors,
                           "mineral_color_by"    = "singlecolor", ## num_localities
                           "mineral_color"       = true_mineral_color,
                           "element_color_by"    = "singlecolor", ## element_redox_network, element_hsab
                           "element_color"       = true_element_color,
                           "mineral_palette"     = true_mineral_palette,
                           "element_palette"     = true_element_palette,
                           "mineral_label_color" = true_mineral_label_color,
                           "element_label_color" = true_element_label_color,
                           "mineral_shape"       = true_mineral_shape, 
                           "element_shape"       = true_element_shape, 
                           ## Single element colors, etc.
                           "elements_of_interest"     = focal,
                           "elements_by_redox"        = TRUE, ## IT'S TRUE
                           "highlight_element"        = TRUE,
                           "highlight_color"          = true_highlight_color,
                           "custom_element_colors"    = true_custom_element_colors,
                           "na_color"                 = true_na_color,
                           ## Sizes
                           "element_size_by"  = "singlesize", ## num_localities 
                           "element_label_size" = true_element_label_size,
                           "element_size_scale" = true_element_size_scale,  ### used if element_label_size != singlesize eg num_localities
                           "mineral_size_by"  = "singlesize", ## num_localities 
                           "mineral_size_scale" = true_mineral_size_scale,  ### used if mineral_size_by != singlesize eg num_localities
                           "mineral_label_size" = true_mineral_label_size,
                           "mineral_size"       = true_mineral_size,
                           ## Edges
                           "edge_color_by" = "singlecolor", 
                           "edge_color"    = true_edge_color,
                           "edge_palette"  = true_edge_palette)

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

