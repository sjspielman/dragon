test_that("fct_calculate_network_info::calculate_connectivity() works", {
  expect_true(igraph::vertex_connectivity(true_graph) == true_connectivity)
})


test_that("fct_calculate_network_info::calculate_modularity() works", {
  expect_equal(calculate_modularity(true_clustering), true_modularity)
})



test_that("fct_calculate_network_info::calculate_number_nodes_edges() works", {
  output <- calculate_number_nodes_edges(true_nodes, true_edges, TRUE)
  expect_equal(output$n_mineral_nodes, true_n_mineral_nodes)
  expect_equal(output$n_element_nodes, true_n_element_nodes)
  expect_equal(output$n_base_elements, true_n_base_elements)
  expect_equal(output$n_edges, true_n_edges)
})
