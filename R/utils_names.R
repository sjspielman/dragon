## Variable names or other strings as shown in UI menus ------------------------------------------------------

#' UI display string for Louvain community clustering
#' @noRd
cluster_alg_louvain_str <- "Louvain"
#' UI display string for Leading eigenvector community clustering
#' @noRd
cluster_alg_eig_str <- "Leading eigenvector"

#' UI display string for specifying "selected time frame" color scheme for mineral timeline
#' @noRd
selected_age_range_str <- "Selected age range"

#' UI display string for selecting a single color for node or edge color
#' @noRd
singlecolor_str <- "Use a single color"
#' UI display string for selecting a single size for node 
#' @noRd
singlesize_str <- "Use a single size"

#' UI display string for element_redox_mineral variable 
#' @noRd
element_redox_mineral_str   <- "Element redox in mineral" 
#' UI display string for element_redox_network variable
#' @noRd
element_redox_network_str   <- "Mean element redox in network"
#' UI display string for max_age variable
#' @noRd
max_age_str                 <- "Maximum known age (Ga) of mineral" 
#' UI display string for min_age variable
#' @noRd
min_age_str                 <- "Minimum known age (Ga) of mineral" 
#' UI display string for num_localities_mineral variable
#' @noRd
num_localities_mineral_str  <- "Number of known mineral localities" 
#' UI display string for num_localities_element variable
#' @noRd
num_localities_element_str  <- "Number of known element localities" 
#' UI display string for num_localities variable
#' @noRd
num_localities_str          <- "Number of known localities" 
#' UI display string for network_degree_norm_ variable
#' @noRd
network_degree_norm_str     <- "Degree centrality (normalized)" 
#' UI display string for network_degree variable
#' @noRd
network_degree_str          <- "Degree centrality" 
#' UI display string for closeness variable
#' @noRd
closeness_str               <- "Closeness centrality"
#' UI display string for pauling variable
#' @noRd
pauling_str                 <- "Element electronegativity" 
#' UI display string for mean_pauling variable
#' @noRd
mean_pauling_str            <- "Mean mineral electronegativity"  
#' UI display string for cov_pauling variable
#' @noRd
cov_pauling_str             <- "COV mineral electronegativity"
#' UI display string for id variable
#' @noRd
id_str                      <- "Node name" 
#' UI display string for cluster_ID variable
#' @noRd
cluster_ID_str              <- "Community cluster" 
#' UI display string for group variable
#' @noRd
group_str                   <- "Node type"
#' UI display string for element variable
#' @noRd
element_str                 <- "Element"
#' UI display string for element_name variable
#' @noRd
element_name_str            <- "Full element name"
#' UI display string for mineral_name variable
#' @noRd
mineral_name_str            <- "Mineral"
#' UI display string for mineral_id variable
#' @noRd
mineral_id_str              <- "Mineral ID"
#' UI display string for mindat_id variable
#' @noRd
mindat_id_str               <- "Mindat ID"
#' UI display string for locality_longname variable
#' @noRd
locality_longname_str       <- "Locality name"
#' UI display string for max_age_locality variable
#' @noRd
max_age_locality_str        <- "Maximum age (Ga) of mineral at locality" 
#' UI display string for min_age_locality variable
#' @noRd
min_age_locality_str        <- "Minimum age (Ga) of mineral at locality" 
#' UI display string for age_type variable
#' @noRd
age_type_str                <- "Age type at locality" 
#' UI display string for rruff_chemistry variable
#' @noRd
rruff_chemistry_str         <- "RRUFF formula"
#' UI display string for ima_chemistry variable
#' @noRd
ima_chemistry_str           <- "IMA formula"
#' UI display string for element_hsab variable
#' @noRd
element_hsab_str            <- "Element HSAB theory"
#' UI display string for atomic_mass variable
#' @noRd
atomic_mass_str             <- "Element atomic mass"
#' UI display string for atomic_radius variable
#' @noRd
atomic_radius_str           <- "Element atomic radius"
#' UI display string for number_of_protons variable
#' @noRd
number_of_protons_str         <- "Number of protons"
#' UI display string for element_table_group variable 
#' @noRd
element_table_group_str           <- "Element group"
#' UI display string for element_table_period variable
#' @noRd
element_table_period_str          <- "Element period"
#' UI display string for element_metal_type variable
#' @noRd
element_metal_type_str       <- "Element metal type"
#' UI display string for element_density variable
#' @noRd
element_density_str         <- "Element density"
#' UI display string for element_specific_heat variable
#' @noRd
element_specific_heat_str    <- "Element specific heat"

#' UI display string for mineral_ndn variable
#' @noRd
mineral_network_degree_norm_str <- "Mineral degree centrality (normalized)" 

#' UI display string for element_ndn variable
#' @noRd
element_network_degree_norm_str <- "Element degree centrality (normalized)" 

#' UI display string for mineral_closeness variable
#' @noRd
mineral_closeness_str       <- "Mineral closeness centrality"

#' UI display string for element_closeness variable
#' @noRd
element_closeness_str       <- "Element closeness centrality"

#' UI display string for mineral_cluster variable
#' @noRd
mineral_cluster_ID_str      <- "Mineral community cluster"

#' UI display string for element_cluster variable
#' @noRd
element_cluster_ID_str      <- "Element community cluster"

#' Variables as an array to be matched with names
#' @noRd
variables_themselves <- c("element_redox_mineral", "element_redox_network", "max_age", "min_age", "age_type", "num_localities_mineral", "num_localities_element", "num_localities", "locality_longname","mindat_id", "max_age_locality", "min_age_locality" , "network_degree_norm", "network_degree", "closeness", "pauling", "mean_pauling", "cov_pauling", "id", "cluster_ID", "group", "element", "element_name", "mineral_name", "mineral_id",  "rruff_chemistry", "ima_chemistry", "element_hsab", "number_of_protons", "atomic_mass", "atomic_radius", "element_table_group", "element_table_period", "element_metal_type", "element_density", "element_specific_heat", "mineral_cluster_ID", "mineral_closeness", "mineral_network_degree_norm", "element_cluster_ID", "element_closeness", "element_network_degree_norm")

#' Variables names as an array
#' @noRd
variable_names <- c(element_redox_mineral_str, element_redox_network_str, max_age_str, min_age_str, age_type_str, num_localities_mineral_str, num_localities_element_str, num_localities_str, locality_longname_str, mindat_id_str, max_age_locality_str, min_age_locality_str, network_degree_norm_str, network_degree_str, closeness_str, pauling_str, mean_pauling_str, cov_pauling_str, id_str, cluster_ID_str, group_str, element_str, element_name_str, mineral_name_str, mineral_id_str, rruff_chemistry_str, ima_chemistry_str, element_hsab_str, number_of_protons_str, atomic_mass_str, atomic_radius_str, element_table_group_str, element_table_period_str, element_metal_type_str, element_density_str, element_specific_heat_str, mineral_cluster_ID_str, mineral_closeness_str, mineral_network_degree_norm_str, element_cluster_ID_str, element_closeness_str, element_network_degree_norm_str) 

#' Array of variables matched to their UI display, as c(variable_name = display_name_str)
#' @noRd
variable_to_title <-  setNames(variable_names, variables_themselves)

#' Array of UI display matched to their variable, as c(display_name_str = variable_name)
#' @noRd
title_to_variable <-  setNames(variables_themselves, variable_names)


#' Rename columns with presentable UI names
#' @importFrom rlang :=
#' @param df A tibble to rename
#' @return Renamed tibble
#' @noRd
rename_for_ui <- function(df)
{
  old_names <- names(df)
  
  for (original_name in old_names){
    ui_name <- as.symbol( variable_to_title[[original_name]] )
    df %>%
      dplyr::rename({{ui_name}} := original_name) -> df
  }
  df
}
