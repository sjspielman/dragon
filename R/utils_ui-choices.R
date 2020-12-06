
## Style UI choices ------------------------------------------------------------

#' Combined string of mineral names and their formulas for UI choosing focal from minerals
#' @noRd
mineral_names_formulas <- function(med_data = med_data_cache){
  med_data %>% 
    dplyr::select(mineral_name, ima_chemistry) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(mineral_name) %>%
    dplyr::mutate(both = paste0(mineral_name, " (", ima_chemistry, ")")) %>% 
    dplyr::pull(both) 
}


#' UI options for selecting a color scheme for minerals in the TIMELINE
#' @noRd
mineral_timeline_color_by_choices <- setNames(c("singlecolor",
                                                "w_mean_pauling", 
                                                "mean_pauling",
                                                "w_cov_pauling",
                                                "cov_pauling",
                                                "num_localities", 
                                                "max_age"),
                                              c(singlecolor_str,
                                                w_mean_pauling_str,
                                                mean_pauling_str,
                                                w_cov_pauling_str,
                                                cov_pauling_str,
                                                num_localities_str,
                                                max_age_str))
                                            
#' UI options for selecting a variable to color element nodes by
#' @noRd
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
                                       element_specific_heat_str))

#' UI options for selecting a variable to color mineral nodes by
#' @noRd
mineral_color_by_choices <- setNames(c("singlecolor",
                                       "max_age", 
                                       "num_localities",
                                       "w_mean_pauling", 
                                       "mean_pauling",
                                       "w_cov_pauling",
                                       "cov_pauling"),
                                     c(singlecolor_str,
                                       max_age_str,
                                       num_localities_mineral_str,
                                       w_mean_pauling_str,
                                       mean_pauling_str,
                                       w_cov_pauling_str,
                                       cov_pauling_str))


#' UI options for selecting a variable to size element nodes by
#' @noRd
element_size_by_choices <- setNames(c("singlesize",
                                      "network_degree_norm",
                                      "closeness", 
                                      "num_localities"),
                                    c(singlesize_str,
                                      network_degree_norm_str,
                                      closeness_str,
                                      num_localities_element_str))
  
#' UI options for selecting a variable to size mineral nodes by
#' @noRd
mineral_size_by_choices <- setNames(c("singlesize",
                                      "max_age",
                                      "num_localities"),
                                    c(singlesize_str,
                                      max_age_str,
                                      num_localities_mineral_str))



#' UI options for selecting a variable to color network edges by
#' @noRd
edge_color_by_choices <- setNames(c("singlecolor",
                                    "max_age",
                                    "w_mean_pauling", 
                                    "mean_pauling",
                                    "w_cov_pauling",
                                    "cov_pauling",
                                    "element_redox_network",
                                    "element_redox_mineral", 
                                    "num_localities_mineral"),
                                  c(singlecolor_str,
                                    max_age_str,
                                    w_mean_pauling_str,
                                    mean_pauling_str,
                                    w_cov_pauling_str,
                                    cov_pauling_str,
                                    element_redox_network_str,
                                    element_redox_mineral_str,
                                    num_localities_mineral_str))

#' UI options for selecting an element node shape
#' @noRd
element_shape_choices <- c("Circle"                = "circle",
                           "Square"                = "box", 
                           "Text only (no shape)"  = "text")

#' UI options for selecting a mineral node shape
#' @noRd
mineral_shape_choices <-  c("Circle"   = "dot", #### !!!!!!
                            "Square"   = "square")

## Modeling choices ----------------------------------------
#' UI options for response variables to use in linear model
#' @noRd
model_response_choices <- c(max_age_str,
                            w_mean_pauling_str,
                            mean_pauling_str,
                            w_cov_pauling_str,
                            cov_pauling_str,
                            network_degree_norm_str,
                            closeness_str,
                            num_localities_str) 

#' UI options for predictor variables to use in linear model
#' @noRd
model_predictor_choices <- c(model_response_choices, cluster_ID_str)

#' List of which variables that can be used in linear models are categorical
#' @noRd
categorical_model_variables <- c(cluster_ID_str)

#' UI options for type of plot to display when building a model with a categorical predictor variable
#' @noRd
categorical_plot_choices <- c("Strip chart" = "strip",
                              "Violin plot" = "violin",
                              "Sina plot"   = "sina",
                              "Boxplot"     = "boxplot")


## Network construction choices -----------------------------------------------------------
#' UI options for network layout
#' @noRd
network_layout_choices <- list(`Force-directed` = c("Fruchterman Reingold"   = "layout_with_fr",
                                                   "GEM force-directed"      = "layout_with_gem"),
                                Other           = c("Dynamic physics layout" = "physics",
                                                    "Sugiyama (bipartite) Layout" = "layout_with_sugiyama",
                                                    "Layout in circle"            = "layout_in_circle",
                                                    "Layout in sphere"            = "layout_on_sphere"))
#' UI options for specifically physics (non-static) network layouts
#' @noRd
physics_choices <- c("forceAtlas2Based"       = "forceAtlas2Based",
                     "Barnes-Hut"             = "barnesHut",
                     "Repulsion"              = "repulsion", 
                     "Hierarchical repulsion" = "hierarchicalRepulsion")
#' UI options for community clustering algorithms
#' @noRd
cluster_algorithm_choices <- c(cluster_alg_louvain_str, cluster_alg_eig_str)


## Arrays used in selected_node_table drop-down menus --------------------------------------------------

#' Variables to always appear in selected node table
#' @noRd
selected_node_table_constant <- c(element_str, mineral_name_str)

#' Variables choices for mineral attributes to appear in the selected node table
#' @noRd
selected_node_table_column_choices_mineral   <- c(mineral_id_str,
                                                  rruff_chemistry_str,
                                                  ima_chemistry_str,
                                                  max_age_str,
                                                  num_localities_mineral_str,
                                                  w_mean_pauling_str,
                                                  mean_pauling_str,
                                                  w_cov_pauling_str,
                                                  cov_pauling_str)
## NOT CURRENTLY USED
# Variables choices for mineral locality attributes to appear in the selected node table
# @noRd
#selected_node_table_column_choices_locality  <- c(mindat_id_str,
#                                                  locality_longname_str,
#                                                  age_type_str,
#                                                  max_age_locality_str,
#                                                  min_age_locality_str)

#' Variables choices for element attributes to appear in the selected node table
#' @noRd
selected_node_table_column_choices_element   <- c(pauling_str,
                                                  element_redox_network_str,
                                                  element_redox_mineral_str,
                                                  element_hsab_str, 
                                                  element_metal_type_str) 

## BOTH ELEMENT AND MINERAL COLUMNS HAVE TO APPEAR WHEN SELECTED
#' Variables choices for network-level node attributes to appear in the selected node table
#' @noRd
selected_node_table_column_choices_network <- c(cluster_ID_str,    
                                                closeness_str,
                                                network_degree_norm_str)
#' List of choices for how igraph can format the output network
#' @noRd
igraph_output_format_choices <- c("GraphML"                       = "graphml", 
                                  "GML (Graph Modeling Language)" = "gml", 
                                  "DOT"                           = "dot",
                                  "Pajek"                         = "pajek", 
                                  "ncol"                          = "ncol", 
                                  "Large Graph Layout (LGL)"      = "lgl",
                                  "LEDA"                          = "leda",
                                  "Edge list"                     = "edgelist")