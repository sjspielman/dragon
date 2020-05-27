age_data    <- initialize_data_age(initialize_data("B", FALSE), c(1, 5), "Maximum")
network_raw <- construct_network(age_data$elements_only_age, FALSE)
nodes       <- add_shiny_node_titles(network_raw$nodes, FALSE)
clustered   <- specify_community_detect_network(network_raw$graph, nodes, "Louvain", "Dark2")
nodes <- clustered$nodes 
edges <- network_raw$edges
locality_info <- age_data$locality_info


# Names from prepare
#  [1] "from"                   "to"                     "element_redox_mineral" 
#  [4] "max_age"                "num_localities_mineral" "mean_pauling"          
#  [7] "cov_pauling"            "mineral_cluster"        "mineral_closeness"     
# [10] "mineral_degree"         "mineral_id"             "ima_chemistry"         
# [13] "rruff_chemistry"        "mineral_name"           "max_age_locality"      
# [16] "min_age_locality"       "mindat_id"              "locality_longname"     
# [19] "age_type"               "element_redox_network"  "element_cluster"       
# [22] "element_closeness"      "element_degree"         "element_name"          
# [25] "element_hsab"           "TablePeriod"            "TableGroup"            
# [28] "AtomicMass"             "AtomicRadius"           "pauling"               
# [31] "MetalType"              "Density"                "SpecificHeat"          
# [34] "num_localities_element"

# Check that there are no NAs in a element-specific and a mineral specific column.


