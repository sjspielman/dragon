library(dragon)

################### THIS IS ALL ALSO IN THE SETUP TEST SUITE #############################
##########################################################################################
focal <- "Fe" ## Focal element used in true data network

## Variables for style testing ------------------------------
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
true_special_element_id <- c("P", "O-2", "H+1", "Fe+2", "Fe", "Fe+3") ## the three iron focals and P/O/H redoxes
true_custom_selection_color_1   <- orange
true_custom_selection_color_2   <- black
true_custom_selection_set_1 <- c("P", "O-2")
true_custom_selection_set_2 <- c("H+1")

true_focal_element_name <- "Iron"

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
                           "custom_selection_element" = true_custom_selection_element,
                           "custom_selection_color"   = true_custom_selection_color,
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
###########################################################################################
###########################################################################################


#### Code below created the CSV files and graph used for testing and were manually inspected for accuracy -------------------------
network_by_redox <- dragon::initialize_network(focal, 
                                               force_all_elements = FALSE, 
                                               elements_by_redox = TRUE, 
                                               age_range         = c(4, 5),
                                               max_age_type      = "Maximum",
                                               cluster_algorithm = "Louvain")
outpath <- system.file("testdata", package="dragon")
readr::write_csv(network_by_redox$locality_info, file.path(outpath, "locality_info.csv"))
readr::write_csv(network_by_redox$edges, file.path(outpath, "edges_by_redox.csv"))
readr::write_csv(network_by_redox$nodes, file.path(outpath, "nodes_by_redox.csv"))
igraph::write_graph(network_by_redox$network, file.path(outpath,"graph_by_redox.igraph"), format="ncol")

subset_mineral_nodes(network_by_redox$nodes) %>% readr::write_csv(file.path(outpath, "true_mineral_nodes.csv"))

true_styled_nodes <- style_nodes(network_by_redox$nodes, true_style_options)
true_styled_edges <- style_edges(network_by_redox$edges, true_style_options)

readr::write_csv(true_styled_nodes$styled_nodes, file.path(outpath, "styled_nodes.csv"))
readr::write_csv(true_styled_edges$styled_edges, file.path(outpath, "styled_edges.csv"))
