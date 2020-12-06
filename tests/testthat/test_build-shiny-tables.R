test_that("fct_build-shiny-tables::build_final_node_table() works", {

  raw_node_table <- prepare_raw_node_table(true_edges, true_nodes)
  display <- c(element_str, mineral_name_str, max_age_str, w_cov_pauling_str, cov_pauling_str, element_hsab_str, closeness_str) ## last one gets dup'd
  output_display <- c(element_str, mineral_name_str, max_age_str, w_cov_pauling_str, cov_pauling_str, element_hsab_str, element_closeness_str, mineral_closeness_str)
  
  
  selected <- c("Fe", "Os")
  built_test <- build_final_node_table(raw_node_table, selected, display)
  expect_equal(sort(names(built_test)), sort(output_display))
  built_test %>% 
    dplyr::filter(!(Element %in% selected) ) -> be_zero
  expect_true(nrow(be_zero) == 0)
  expect_true(nrow(built_test) != 0)
  
  selected <- c("All cluster 1 elements")
  cluster_1_nodes <- c("C", "As", "S",  "Ni", "Fe", "P",  "Te", "N",  "Co")
  built_test <- build_final_node_table(raw_node_table, selected, display)
  expect_equal(sort(names(built_test)), sort(output_display))
  built_test %>% 
    dplyr::filter(!(Element %in% cluster_1_nodes) ) -> be_zero
  expect_true(nrow(be_zero) == 0)
  expect_true(nrow(built_test) != 0)  
  
  selected <- c("Fe+3", "All cluster 1 elements")
  cluster_1_nodes_plus <- c("Fe+3", cluster_1_nodes)
  built_test <- build_final_node_table(raw_node_table, selected, display)
  expect_equal(sort(names(built_test)), sort(output_display))
  built_test %>% 
    dplyr::filter(!(Element %in% cluster_1_nodes_plus) ) -> be_zero
  expect_true(nrow(be_zero) == 0)
  expect_true(nrow(built_test) != 0)
  
  
})
  
test_that("fct_build-shiny-tables::prepare_raw_node_table() works", {
  prepared_test <- prepare_raw_node_table(true_edges, true_nodes)
  expected_names <- sort(c(element_str,
                           mineral_name_str,
                           element_redox_mineral_str,
                           mineral_id_str,
                           max_age_str,
                           w_mean_pauling_str,
                           w_cov_pauling_str,
                           mean_pauling_str,
                           cov_pauling_str,
                           ima_chemistry_str,
                           rruff_chemistry_str,
                           mineral_cluster_ID_str,
                           mineral_closeness_str,
                           mineral_network_degree_norm_str,
                           num_localities_mineral_str,
                           element_cluster_ID_str,
                           element_closeness_str,
                           element_network_degree_norm_str,
                           num_localities_element_str,
                           element_redox_network_str,
                           pauling_str,
                           element_hsab_str,
                           element_metal_type_str))
    expect_equal(sort(names(prepared_test)), expected_names)
})



test_that("fct_build-shiny-tables::build_element_exploration_table() works", {
  element_table <- build_element_exploration_table(true_nodes)
  expected_names <- sort(c(element_str, 
                           element_name_str, 
                           cluster_ID_str, 
                           closeness_str, 
                           network_degree_norm_str, 
                           pauling_str, 
                           element_redox_network_str, 
                           num_localities_str, 
                           element_hsab_str, 
                           element_metal_type_str, ##
                           atomic_mass_str, 
                           number_of_protons_str, 
                           element_table_period_str, ##
                           element_table_group_str,  ##
                           atomic_radius_str, 
                           element_specific_heat_str, 
                           element_density_str))
  expect_equal(sort(names(element_table)), expected_names)
})


test_that("fct_build-shiny-tables::build_mineral_exploration_table() works", {
  mineral_table <- build_mineral_exploration_table(true_nodes, true_locality_info)
  expected_names <- sort(c(mineral_name_str, 
                           mineral_id_str, 
                           ima_chemistry_str,
                           rruff_chemistry_str,
                           max_age_str,
                           w_mean_pauling_str,
                           w_cov_pauling_str,
                           mean_pauling_str,
                           cov_pauling_str,
                           cluster_ID_str, 
                           closeness_str, 
                           network_degree_norm_str, 
                           num_localities_str, 
                           min_age_locality_str,
                           max_age_locality_str,
                           mindat_id_str,
                           locality_longname_str,
                           age_type_str))
  expect_equal(sort(names(mineral_table)), expected_names)
})


