#library(dragon)
devtools::load_all()
library(zip)

outpath <- "../../tests/testthat/"
source(file.path(outpath, "setup_network-style.R"))


#### Code below created the CSV files and graph used for testing and were manually inspected for accuracy -------------------------
network_by_redox <- dragon::initialize_network(focal, 
                                               force_all_elements = FALSE, 
                                               elements_by_redox = TRUE, 
                                               restrict_to_elements = FALSE, 
                                               ignore_na_redox = FALSE,
                                               age_range         = c(4, 5),
                                               max_age_type      = "Maximum",
                                               cluster_algorithm = "Louvain",
                                               use_data_cache    = TRUE)

write_zip_clean <- function(df, file){
  readr::write_csv(df, file)
  zip::zipr(paste0(file, ".zip"), file)
  file.remove(file)
}

write_zip_clean(network_by_redox$locality_info, file.path(outpath, "locality_info.csv"))
write_zip_clean(network_by_redox$edges, file.path(outpath, "edges_by_redox.csv"))
write_zip_clean(network_by_redox$nodes, file.path(outpath, "nodes_by_redox.csv"))

subset_mineral_nodes(network_by_redox$nodes) %>% 
  write_zip_clean(file.path(outpath, "true_mineral_nodes.csv"))
  
true_styled_nodes <- style_nodes(network_by_redox$nodes, true_style_options)
true_styled_edges <- style_edges(network_by_redox$edges, true_style_options)
write_zip_clean(true_styled_nodes$styled_nodes, file.path(outpath, "styled_nodes.csv"))
write_zip_clean(true_styled_edges$styled_edges, file.path(outpath, "styled_edges.csv"))

igraph::write_graph(network_by_redox$network, file.path(outpath,"graph_by_redox.igraph"), format="ncol")

