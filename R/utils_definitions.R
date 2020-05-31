

original_options <- options(scipen=0, htmlwidgets.TOJSON_ARGS = NULL)
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) ## Setting for DT to show NA cells as NA rather than blank
options(scipen=3)                                      ## Sci when more than 3 digits
on.exit(options(original_options), add=TRUE)


## Explicit pipe definitions
`%>%`  <- magrittr::`%>%`
`%<>%` <- magrittr::`%>%`


css_string_selectedNode <- "float:right; width: 200px; font-size: 14px; color: #000; background-color: #F1F1F1; border-radius: 0px; border: solid 1px #DCDCDC; height: 34px; margin: -1.4em 0.5em 0em 0em;"

## Definitions 

## Variable names as shown in UI menus ------------------------------------------------------
element_redox_mineral_str   <- "Element redox in mineral" 
element_redox_network_str   <- "Mean element redox in network"
max_age_str                 <- "Maximum known age (Ga) of mineral" 
min_age_str                 <- "Minimum known age (Ga) of mineral" 
max_age_locality_str        <- "Maximum age (Ga) of mineral at locality" 
min_age_locality_str        <- "Minimum age (Ga) of mineral at locality" 
age_type_str                <- "Age type at locality" 
num_localities_mineral_str  <- "Number of known mineral localities" 
num_localities_element_str  <- "Number of known element localities" 
num_localities_str          <- "Number of known localities" 
network_degree_norm_str     <- "Degree centrality (normalized)" 
mineral_network_degree_norm_str <- "Mineral network degree centrality (normalized)" 
element_network_degree_norm_str <- "Element network degree centrality (normalized)" 
network_degree_str          <- "Degree centrality" 
closeness_str               <- "Closeness centrality"
mineral_closeness_str       <- "Mineral network closeness centrality"
element_closeness_str       <- "Element network closeness centrality"
pauling_str                 <- "Element electronegativity" 
mean_pauling_str            <- "Mean mineral electronegativity"  
cov_pauling_str             <- "COV mineral electronegativity"
id_str                      <- "Node name" 
cluster_ID_str              <- "Community cluster" 
mineral_cluster_ID_str      <- "Mineral community cluster"
element_cluster_ID_str      <- "Element community cluster"
group_str                   <- "Node type"
element_str                 <- "Element"
element_name_str            <- "Full element name"
mineral_name_str            <- "Mineral"
mineral_name_node_table_str <- "Minerals containing element"
element_node_table_str      <- "Elements in mineral"
mineral_id_str              <- "Mineral ID"
mindat_id_str               <- "Mindat ID"
locality_longname_str       <- "Locality name"
age_type_str                <- "Age type"
rruff_chemistry_str         <- "RRUFF formula"
ima_chemistry_str           <- "IMA formula"
element_hsab_str            <- "Element HSAB theory"
element_mass_str            <- "Element atomic mass"
element_radius_str          <- "Element atomic radius"
element_protons_str         <- "Number of protons"
element_group_str           <- "Element group"
element_period_str          <- "Element period"
element_metaltype_str       <- "Element metal type"
element_density_str         <- "Element density"
element_specificheat_str    <- "Element specific heat"



## UI variable names matched to their actual variables
variable_to_title <-  c("element_redox_mineral" = element_redox_mineral_str, 
                        "element_redox_network" = element_redox_network_str,
                        "max_age" = max_age_str, 
                        "min_age" = min_age_str, 
                        "age_type" = age_type_str,
                        "num_localities_mineral" = num_localities_mineral_str, 
                        "num_localities_element" = num_localities_element_str, 
                        "num_localities" = num_localities_str, # MODELING ONLY
                        "locality_longname"      = locality_longname_str,
                        "network_degree_norm" = network_degree_norm_str, 
                        "element_network_degree_norm" = element_network_degree_norm_str, 
                        "mineral_network_degree_norm" = mineral_network_degree_norm_str, 
                        "network_degree" = network_degree_str, 
                        "closeness" = closeness_str,
                        "mineral_closeness" = mineral_closeness_str,
                        "element_closeness" = element_closeness_str,
                        "pauling" = pauling_str, 
                        "mean_pauling" = mean_pauling_str,  
                        "cov_pauling" = cov_pauling_str,
                        "id" = id_str, 
                        "cluster_ID" = cluster_ID_str, 
                        "mineral_cluster_ID" = mineral_cluster_ID_str,
                        "element_cluster_ID" = element_cluster_ID_str,
                        "group" = group_str,
                        "element" = element_str,
                        "element_name" = element_name_str,
                        "mineral_name" = mineral_name_str,
                        "mineral_id" = mineral_id_str,
                        "mindat_id" = mindat_id_str,
                        "age_type" = age_type_str,
                        "rruff_chemistry" = rruff_chemistry_str,
                        "ima_chemistry" = ima_chemistry_str,
                        "element_hsab" = element_hsab_str, 
                        "NumberofProtons" = element_protons_str, 
                        "AtomicMass" = element_mass_str,
                        "AtomicRadius" = element_radius_str,
                        "TableGroup" = element_group_str,
                        "TablePeriod" = element_period_str, 
                        "MetalType" = element_metaltype_str,
                        "Density" = element_density_str,
                        "SpecificHeat" = element_specificheat_str, 
                        "max_age_locality" = max_age_locality_str,
                        "min_age_locality" = min_age_locality_str)
element_hsab_levels  <- c("Hard acid", "Int. acid", "Soft acid", "Soft base", "Int. base", "Hard base")


cluster_alg_louvain_str <- "Louvain"
cluster_alg_eig_str <- "Leading eigenvector"
allowed_cluster_algorithms <- c(cluster_alg_louvain_str, cluster_alg_eig_str)


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

## Elements --------------------------------------------------------------------------
all_elements <- c("Ag", "Al", "As", "Au", "B", "Ba", "Be", "Bi", "Br", "C", "Ca", "Cd", "Ce", "Cl", "Co", "Cr", "Cs", "Cu", "Dy", "Er", "F", "Fe", "Ga", "Gd", "Ge", "H", "Hf", "Hg", "I", "In", "Ir", "K", "La", "Li", "Mg", "Mn", "Mo", "N", "Na", "Nb", "Nd", "Ni", "O", "Os", "P", "Pb", "Pd", "Pt", "Rb", "Re", "REE", "Rh", "Ru", "S", "Sb", "Sc", "Se", "Si", "Sm", "Sn", "Sr", "Ta", "Te", "Th", "Ti", "Tl", "U", "V", "W", "Y", "Yb", "Zn", "Zr")

allowed_cluter_algorithms <- c("Louvain", "Leading eigenvector")

## THESE SHOULD BE ORDERED
ordinal_color_variables <- c("element_hsab",  "TablePeriod", "TableGroup") # WE HAVE TURNED OFF TABLEPERIOD AND GROUP FOR COLORING SINCE TOO MANY COLORS. Keep this for now, can't hurt.

#############################################################

# mwahahahaha
error_choices <- c("Oh no!", "Sorry, that's not gonna work.", "Try again!", "Womp womp :(", "No dice!", "Uh oh!", "Woopsies!")


model_response_choices <- c(max_age_str,
                            mean_pauling_str,
                            cov_pauling_str,
                            network_degree_norm_str,
                            closeness_str,
                            num_localities_str) 

model_predictor_choices <- c(model_response_choices, cluster_ID_str)


## mediocre matching here.
vis_to_gg_shape <- list("circle"  = 19,
                        "dot"     = 19,
                        "ellipse" = 19,
                        "box"     = 15,
                        "text"    = 19,
                        "square"  = 15,
                        "star"    = 8,
                        "diamond" = 18,
                        "triangle" = 17)

element_size_scale_divisor <- 1
mineral_size_scale_divisor <- 10
mineral_size_min <- 5
mineral_size_max <- 30
element_size_min <- 1
element_size_max <- 4
lighten_darken_factor <- 0.3

