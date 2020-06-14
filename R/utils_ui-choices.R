
## Style UI choices ------------------------------------------------------------
#' UI options for selecting a variable to color element nodes by
element_color_by_choices <- setNames(c("singlecolor",
                                       "network_degree_norm", 
                                       "pauling",
                                       "element_redox_network",
                                       "element_hsab",
                                       "num_localities",
                                       "atomic_mass",
                                       "element_density",
                                       "element_specific_heat"),
                                     c(singlecolor_str, 
                                       network_degree_norm_str, 
                                       pauling_str,
                                       element_redox_network_str,
                                       element_hsab_str,
                                       num_localities_element_str,
                                       atomic_mass_str,
                                       element_density_str,
                                       element_specificheat_str))

#' UI options for selecting a variable to color mineral nodes by
mineral_color_by_choices <- setNames(c("singlecolor",
                                       "max_age", 
                                       "num_localities",
                                       "mean_pauling",
                                       "cov_pauling"),
                                     c(singlecolor_str,
                                       max_age_str,
                                       num_localities_mineral_str,
                                       mean_pauling_str,
                                       cov_pauling_str ))


#' UI options for selecting a variable to size element nodes by
element_size_by_choices <- setNames(c("singlesize",
                                      "network_degree_norm",
                                      "closeness", 
                                      "num_localities"),
                                    c(singlesize_str,
                                      network_degree_norm_str,
                                      closeness_str,
                                      num_localities_element_str))
  
#' UI options for selecting a variable to size mineral nodes by
mineral_size_by_choices <- setNames(c("singlesize",
                                      "max_age",
                                      "num_localities"),
                                    c(singlesize_str,
                                      max_age_str,
                                      num_localities_mineral_str))



#' UI options for selecting a variable to color network edges by
edge_color_by_choices <- setNames(c("singlecolor",
                                    "max_age",
                                    "mean_pauling",
                                    "cov_pauling",
                                    "element_redox_network",
                                    "element_redox_mineral", 
                                    "num_localities_mineral"),
                                  c(singlecolor_str,
                                    max_age_str,
                                    mean_pauling_str,
                                    cov_pauling_str,
                                    element_redox_network_str,
                                    element_redox_mineral_str,
                                    num_localities_mineral_str))

#' UI options for selecting an element node shape
element_shape_choices <- c("Circle"                = "circle",
                           "Square"                = "box", 
                           "Text only (no shape)"  = "text")

#' UI options for selecting a mineral node shape
mineral_shape_choices <-  c("Circle"   = "dot", #### !!!!!!
                            "Square"   = "square")

## Modeling choices ----------------------------------------
#' UI options for response variables to use in linear model
model_response_choices <- c(max_age_str,
                            mean_pauling_str,
                            cov_pauling_str,
                            network_degree_norm_str,
                            closeness_str,
                            num_localities_str) 

#' UI options for predictor variables to use in linear model
model_predictor_choices <- c(model_response_choices, cluster_ID_str)

#' List of which variables that can be used in linear models are categorical
categorical_model_variables <- c(cluster_ID_str)

#' UI options for type of plot to display when building a model with a categorical predictor variable
categorical_plot_choices <- c("Strip chart" = "strip",
                              "Violin plot" = "violin",
                              "Sina plot"   = "sina",
                              "Boxplot"     = "boxplot")


## Network construction choices -------------------------------------------
#' UI options for network layout
network_layout_choices <- list(`Force-directed` = c("Fruchterman Reingold"   = "layout_with_fr",
                                                   "GEM force-directed"      = "layout_with_gem"),
                                Other           = c("Dynamic physics layout (WARNING: Do not use if photosensitive)" = "physics",
                                                    "Sugiyama (bipartite) Layout" = "layout_with_sugiyama",
                                                    "Layout in circle"            = "layout_in_circle",
                                                    "Layout in sphere"            = "layout_on_sphere"))

#' UI options for specifically physics (non-static) network layouts
physics_choices <- c("forceAtlas2Based"       = "forceAtlas2Based",
                     "Barnes-Hut"             = "barnesHut",
                     "Repulsion"              = "repulsion", 
                     "Hierarchical repulsion" = "hierarchicalRepulsion")

#' UI options for community clustering algorithms
cluster_algorithm_choices <- c(cluster_alg_louvain_str, cluster_alg_eig_str)




# TODO THIS IS WRONG NOW
## Arrays used in selected_node_table dropdown menus ---------------------------------
# selected_node_table_column_choices_mineral   <- c(mineral_id_str, 
#                                                   rruff_chemistry_str, 
#                                                   ima_chemistry_str, 
#                                                   max_age_str, 
#                                                   num_localities_mineral_str,
#                                                   mean_pauling_str, 
#                                                   cov_pauling_str,
#                                                   mineral_cluster_ID_str,
#                                                   mineral_closeness_str,
#                                                   mineral_network_degree_norm_str)
# selected_node_table_column_choices_locality  <- c(mindat_id_str, 
#                                                   locality_longname_str, 
#                                                   age_type_str, 
#                                                   max_age_locality_str, 
#                                                   min_age_locality_str)
# selected_node_table_column_choices_element   <- c(element_name_str, 
#                                                   element_redox_network_str, 
#                                                   pauling_str, 
#                                                   element_hsab_str, 
#                                                   element_group_str, 
#                                                   element_period_str, 
#                                                   element_metaltype_str, 
#                                                   element_mass_str,
#                                                   element_radius_str,
#                                                   element_specificheat_str,
#                                                   element_density_str,
#                                                   num_localities_element_str, 
#                                                   element_cluster_ID_str,
#                                                   element_closeness_str,
#                                                   element_network_degree_norm_str,
#                                                   element_redox_mineral_str) ## THIS ONE WILL REQUIRE APPEARING THE MINERAL NAME
# 
