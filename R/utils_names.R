

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



## UI variable names matched to their actual variables -------------------------------------------------------
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
                        "number_of_protons" = element_protons_str, 
                        "atomic_mass" = element_mass_str,
                        "atomic_radius" = element_radius_str,
                        "table_group" = element_group_str,
                        "table_period" = element_period_str, 
                        "metal_type" = element_metaltype_str,
                        "element_density" = element_density_str,
                        "element_specific_heat" = element_specificheat_str, 
                        "max_age_locality" = max_age_locality_str,
                        "min_age_locality" = min_age_locality_str)

