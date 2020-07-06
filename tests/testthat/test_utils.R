test_that("utils_names::rename_for_ui() works", {
  original <- tibble::tribble(
    ~element_redox_mineral, ~element_redox_network, ~max_age, ~min_age, ~age_type, ~num_localities_mineral, ~num_localities_element, ~num_localities, ~locality_longname, ~max_age_locality, ~min_age_locality, ~network_degree_norm, ~network_degree, ~closeness, ~pauling, ~mean_pauling, ~cov_pauling, ~id, ~cluster_ID, ~group, ~element, ~element_name, ~mineral_name, ~mineral_id, ~mindat_id, ~rruff_chemistry, ~ima_chemistry, ~element_hsab, ~number_of_protons, ~atomic_mass, ~atomic_radius, ~element_table_group, ~element_table_period, ~element_metal_type, ~element_density, ~element_specific_heat, ~mineral_cluster_ID, ~mineral_closeness, ~mineral_network_degree_norm, ~element_cluster_ID, ~element_closeness, ~element_network_degree_norm,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  )
  
  renamed <- rename_for_ui(original)
  expected_names <- sort(c(element_redox_mineral_str, element_redox_network_str, max_age_str, min_age_str, age_type_str, num_localities_mineral_str, num_localities_element_str, num_localities_str, locality_longname_str,  mindat_id_str, max_age_locality_str, min_age_locality_str, network_degree_norm_str, network_degree_str, closeness_str, pauling_str, mean_pauling_str, cov_pauling_str, id_str, cluster_ID_str, group_str, element_str, element_name_str, mineral_name_str, mineral_id_str, rruff_chemistry_str, ima_chemistry_str, element_hsab_str, number_of_protons_str, atomic_mass_str, atomic_radius_str, element_table_group_str, element_table_period_str, element_metal_type_str, element_density_str, element_specific_heat_str, mineral_cluster_ID_str, mineral_closeness_str, mineral_network_degree_norm_str, element_cluster_ID_str, element_closeness_str, element_network_degree_norm_str))
  expect_true( all(sort(names(renamed)) == expected_names) )
  
})