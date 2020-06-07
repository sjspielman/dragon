
## Style UI choices ------------------------------------------------------------
element_color_by_choices =  c("Single color"            = "singlecolor",  
                              "Degree centrality"       = "network_degree_norm",
                              "Electronegativity"       = "pauling", 
                              "Redox state in network"  = "element_redox_network",
                              "HSAB theory"             = "element_hsab",
                              "Number of localities (based on mineral discovery)" = "num_localities",
                              "Atomic mass"             = "atomic_mass",
                              #"Number of protons" = "number_of_protons", # NOT FUNDAMENTALLY DIFF FROM ATOMIC MASS
                              #"Periodic Table Group"       = "table_group", # TOO MANY FOR PALETTE
                              #"Periodic Table Period"       = "table_period", # TOO MANY FOR PALETTE
                              #"Metal type"    = "metal_type", ## legend is a disaster. unless someone requests this feature, it's out
                              "Element density"                = "element_density",
                              "Element specific heat"          = "element_specific_heat")

mineral_color_by_choices <- c("Single color"                = "singlecolor",  
                             "Maximum known age"           = "max_age",      
                             "Number of known localities"  = "num_localities",
                             "Mean electronegativity"      = "mean_pauling", 
                             "COV electronegativity"       = "cov_pauling")

### These are all guaranteed to NOT be NA
element_size_by_choices <- c("Single size" = "singlesize",
                             "Normalized network degree" = "network_degree_norm", 
                             "Network closeness centrality"           = "closeness", 
                             "Number of known localities"      = "num_localities")
mineral_size_by_choices <- c("Single size" = "singlesize",
                            "Maximum known age"         = "max_age",      
                            "Number of known localities" = "num_localities")


edge_color_by_choices   <- c("Single color" = "singlecolor",  
                            "Element redox state in network" = "element_redox_network",
                            "Element redox state in mineral" = "element_redox_mineral",
                            "Number of known mineral localities" = "num_localities_mineral",
                            "Mean mineral electronegativity"     = "mean_pauling",
                            "COV mineral electronegativity"     = "cov_pauling",
                            "Maximum known age of mineral"      = "max_age")

element_shape_choices <- c("Circle"                = "circle",
                           "Square"                = "box", 
                           "Text only (no shape)"  = "text")

mineral_shape_choices <-  c("Circle"   = "dot", #### !!!!!!
                            "Square"   = "square")

## Modeling choices ----------------------------------------
model_response_choices <- c(max_age_str,
                            mean_pauling_str,
                            cov_pauling_str,
                            network_degree_norm_str,
                            closeness_str,
                            num_localities_str) 

model_predictor_choices <- c(model_response_choices, cluster_ID_str)

categorical_model_variables <- c(cluster_ID_str)

categorical_plot_choices <- c("Strip chart" = "strip",
                              "Violin plot" = "violin",
                              "Sina plot"   = "sina",
                              "Boxplot"     = "boxplot")


## Network construction choices -------------------------------------------
network_layout_choices <- list(`Force-directed` = c("Fruchterman Reingold"   = "layout_with_fr",
                                                   "GEM force-directed"      = "layout_with_gem"),
                                Other           = c("Dynamic physics layout (WARNING: Do not use if photosensitive)" = "physics",
                                                    "Sugiyama (bipartite) Layout" = "layout_with_sugiyama",
                                                    "Layout in circle"            = "layout_in_circle",
                                                    "Layout in sphere"            = "layout_on_sphere"))

physics_choices <- c("forceAtlas2Based"       = "forceAtlas2Based",
                     "Barnes-Hut"             = "barnesHut",
                     "Repulsion"              = "repulsion", 
                     "Hierarchical repulsion" = "hierarchicalRepulsion")

cluster_alg_louvain_str <- "Louvain"
cluster_alg_eig_str <- "Leading eigenvector"
allowed_cluster_algorithms <- c(cluster_alg_louvain_str, cluster_alg_eig_str)



# No longer used.
#css_string_selectedNode <- "float:right; width: 200px; font-size: 14px; color: #000; background-color: #F1F1F1; border-radius: 0px; border: solid 1px #DCDCDC; height: 34px; margin: -1.4em 0.5em 0em 0em;"

## Definitions 





# TODO THIS IS WRONG NOW
## Arrays used in selected_node_table dropdown menus ---------------------------------
selected_node_table_column_choices_mineral   <- c(mineral_id_str, 
                                                  rruff_chemistry_str, 
                                                  ima_chemistry_str, 
                                                  max_age_str, 
                                                  num_localities_mineral_str,
                                                  mean_pauling_str, 
                                                  cov_pauling_str,
                                                  mineral_cluster_ID_str,
                                                  mineral_closeness_str,
                                                  mineral_network_degree_norm_str)
selected_node_table_column_choices_locality  <- c(mindat_id_str, 
                                                  locality_longname_str, 
                                                  age_type_str, 
                                                  max_age_locality_str, 
                                                  min_age_locality_str)
selected_node_table_column_choices_element   <- c(element_name_str, 
                                                  element_redox_network_str, 
                                                  pauling_str, 
                                                  element_hsab_str, 
                                                  element_group_str, 
                                                  element_period_str, 
                                                  element_metaltype_str, 
                                                  element_mass_str,
                                                  element_radius_str,
                                                  element_specificheat_str,
                                                  element_density_str,
                                                  num_localities_element_str, 
                                                  element_cluster_ID_str,
                                                  element_closeness_str,
                                                  element_network_degree_norm_str,
                                                  element_redox_mineral_str) ## THIS ONE WILL REQUIRE APPEARING THE MINERAL NAME

