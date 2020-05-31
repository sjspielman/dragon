
## Build very old small
network <- initialize_network("B", 
                              force_all_elements = FALSE, 
                              elements_by_redox = TRUE, 
                              age_range         = c(3.6, 5),
                              max_age_type      = "Maximum",
                              cluster_algorithm = "Louvain")

true_mod <- 0.314 ## tolerance argument to expect_equal
true_conn <- 1
true_mineral_nodes <- 2
true_base_elements <- 8
true_element_nodes <- 9
true_edges <- 11

test_that("fct_calculate_network_info::calculate_connectivity() works", {
  expect_true(igraph::vertex_connectivity(network$network) == true_conn)
})

test_that("fct_calculate_network_info::calculate_modularity() works", {
  expect_equal(calculate_modularity(network$clustering), true_mod, tolerance = 1e-3)
})

test_that("fct_calculate_network_info::calculate_number_nodes_edges() works", {
  output <- calculate_number_nodes_edges(network$nodes, network$edges, TRUE)
  expect_equal(output$n_mineral_nodes, true_mineral_nodes)
  expect_equal(output$n_element_nodes, true_element_nodes)
  expect_equal(output$n_base_elements, true_base_elements)
  expect_equal(output$n_edges, true_edges)
})
