## Variable names or other strings as shown in UI menus ------------------------------------------------------

#' UI display string for Louvain community clustering
cluster_alg_louvain_str <- "Louvain"
#' UI display string for Leading eigenvector community clustering
cluster_alg_eig_str <- "Leading eigenvector"

#' UI display string for selected a single color for node or edge color
singlecolor_str <- "Single color"
#' UI display string for selected a single size for node 
singlesize_str <- "Single size"

#' UI display string for element_redox_mineral variable 
element_redox_mineral_str   <- "Element redox in mineral" 
#' UI display string for element_redox_network variable
element_redox_network_str   <- "Mean element redox in network"
#' UI display string for max_age variable
max_age_str                 <- "Maximum known age (Ga) of mineral" 
#' UI display string for min_age variable
min_age_str                 <- "Minimum known age (Ga) of mineral" 
#' UI display string for num_localities_mineral variable
num_localities_mineral_str  <- "Number of known mineral localities" 
#' UI display string for num_localities_element variable
num_localities_element_str  <- "Number of known element localities" 
#' UI display string for num_localities variable
num_localities_str          <- "Number of known localities" 
#' UI display string for network_degree_norm_ variable
network_degree_norm_str     <- "Degree centrality (normalized)" 
#' UI display string for network_degree variable
network_degree_str          <- "Degree centrality" 
#' UI display string for closeness variable
closeness_str               <- "Closeness centrality"
#' UI display string for pauling variable
pauling_str                 <- "Element electronegativity" 
#' UI display string for mean_pauling variable
mean_pauling_str            <- "Mean mineral electronegativity"  
#' UI display string for cov_pauling variable
cov_pauling_str             <- "COV mineral electronegativity"
#' UI display string for id variable
id_str                      <- "Node name" 
#' UI display string for cluster_ID variable
cluster_ID_str              <- "Community cluster" 
#' UI display string for group variable
group_str                   <- "Node type"
#' UI display string for element variable
element_str                 <- "Element"
#' UI display string for element_name variable
element_name_str            <- "Full element name"
#' UI display string for mineral_name variable
mineral_name_str            <- "Mineral"
#' UI display string for mineral_id variable
mineral_id_str              <- "Mineral ID"
#' UI display string for mindat_id variable
mindat_id_str               <- "Mindat ID"
#' UI display string for locality_longname variable
locality_longname_str       <- "Locality name"
#' UI display string for max_age_locality variable
max_age_locality_str        <- "Maximum age (Ga) of mineral at locality" 
#' UI display string for min_age_locality variable
min_age_locality_str        <- "Minimum age (Ga) of mineral at locality" 
#' UI display string for age_type variable
age_type_str                <- "Age type at locality" 
#' UI display string for rruff_chemistry variable
rruff_chemistry_str         <- "RRUFF formula"
#' UI display string for ima_chemistry variable
ima_chemistry_str           <- "IMA formula"
#' UI display string for element_hsab variable
element_hsab_str            <- "Element HSAB theory"
#' UI display string for atomic_mass variable
atomic_mass_str             <- "Element atomic mass"
#' UI display string for atomic_radius variable
atomic_radius_str           <- "Element atomic radius"
#' UI display string for number_of_protons variable
number_of_protons_str         <- "Number of protons"
#' UI display string for element_group variable (i.e. periodic table group, NOT network group)
element_group_str           <- "Element group"
#' UI display string for element_period variable
element_period_str          <- "Element period"
#' UI display string for element_metaltype variable
element_metaltype_str       <- "Element metal type"
#' UI display string for element_density variable
element_density_str         <- "Element density"
#' UI display string for element_specificheat variable
element_specificheat_str    <- "Element specific heat"


#' Variables as an array to be matched with names
variables_themselves <- c("element_redox_mineral", "element_redox_network", "max_age", "min_age", "age_type", "num_localities_mineral", "num_localities_element", "num_localities", "locality_longname", "max_age_locality", "min_age_locality" , "network_degree_norm", "network_degree", "closeness", "pauling", "mean_pauling", "cov_pauling", "id", "cluster_ID", "group", "element", "element_name", "mineral_name", "mineral_id", "mindat_id", "age_type", "rruff_chemistry", "ima_chemistry", "element_hsab", "number_of_protons", "atomic_mass", "atomic_radius", "table_group", "table_period", "metal_type", "element_density", "element_specific_heat")

#' Variables names as an array
variable_names <- c(element_redox_mineral_str, element_redox_network_str, max_age_str, min_age_str, age_type_str, num_localities_mineral_str, num_localities_element_str, num_localities_str, locality_longname_str, max_age_locality_str, min_age_locality_str, network_degree_norm_str, network_degree_str, closeness_str, pauling_str, mean_pauling_str, cov_pauling_str, id_str, cluster_ID_str, group_str, element_str, element_name_str, mineral_name_str, mineral_id_str, mindat_id_str, age_type_str, rruff_chemistry_str, ima_chemistry_str, element_hsab_str, number_of_protons_str, atomic_mass_str, atomic_radius_str, element_group_str, element_period_str, element_metaltype_str, element_density_str, element_specificheat_str) 

#' Array of variables matched to their UI display, as c(visplay_name_str = variable_name)
variable_to_title <-  setNames(variable_names, variables_themselves)

    



######################################### PENDING NODE TABLE MAYBE??? #########################################
#' UI display string for mineral_network_degree_norm variable
#mineral_network_degree_norm_str <- "Mineral network degree centrality (normalized)" 
#' UI display string for element_network_degree_norm variable
#element_network_degree_norm_str <- "Element network degree centrality (normalized)" 
#' UI display string for mineral_closeness variable
#mineral_closeness_str       <- "Mineral network closeness centrality"
#' UI display string for element_closeness variable
#element_closeness_str       <- "Element network closeness centrality"
#' UI display string for 
#mineral_cluster_ID_str      <- "Mineral community cluster"
#' UI display string for 
#element_cluster_ID_str      <- "Element community cluster"
#' UI display string for 
#mineral_name_node_table_str <- "Minerals containing element"
#' UI display string for 
#element_node_table_str      <- "Elements in mineral"

#  "mineral_cluster_ID" = mineral_cluster_ID_str,
#  "element_cluster_ID" = element_cluster_ID_str,
# "mineral_closeness" = mineral_closeness_str,
# "element_closeness" = element_closeness_str,
# "element_network_degree_norm" = element_network_degree_norm_str, 
# "mineral_network_degree_norm" = mineral_network_degree_norm_str,   
