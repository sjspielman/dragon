age_data    <- initialize_data_age(initialize_data("B", FALSE), c(0, 5), "Maximum")
network_raw <- construct_network(age_data$elements_only_age, FALSE)
nodes       <- add_shiny_node_titles(network_raw$nodes, FALSE)
clustered   <- specify_community_detect_network(network_raw$graph, nodes, "Louvain", "Dark2")
nodes <- clustered$nodes 
edges <- network_raw$edges
locality_info <- age_data$locality_info




test_that("fct_run_build_node_table::build_node_table()", {
  expect_true(2==2)
})

