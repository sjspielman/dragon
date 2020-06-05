## Test that calculate_output_node_positions() works -----------------------------------------------
test_that("fct_export_network::calculate_output_node_positions() works with NULL positions", {
  position_tibble <- calculate_output_node_positions(styled_nodes, NULL, network$network, "layout_with_fr", 1)
  true_names <- sort(c(names(styled_nodes), "x", "y"))
  expect_equal(sort(names(position_tibble)), true_names)
})


## Test that initialize_network() works -----------------------------------------------
test_that("fct_export_network::visnetwork_to_igraph() works", {
  position_tibble <- calculate_output_node_positions(styled_nodes, NULL, network$network, "layout_with_fr", 1)

  igraph_version <- visnetwork_to_igraph(position_tibble, 
                                         styled_edges, 
                                         1, ## baseline_output_element_size
                                         1, ## baseline_output_element_label_size
                                         1, ## baseline_output_mineral_size
                                         FALSE) # frame nodes
  
  # check names
  expect_equal(sort(names(igraph_version)), sort(c("igraph_network", "coords", "vis_aspect_ratio")))
  
  # check vis_aspect_ratio
  expect_true(is.numeric(igraph_version$vis_aspect_ratio))
  expect_true(igraph_version$vis_aspect_ratio > 0)
  
  # check coordinates
  if (R.Version()$major < 4) expect_true(class(igraph_version$coords) == "matrix") 
  if (R.Version()$major >= 4) expect_true(class(igraph_version$coords) == c("matrix", "array"))
  expect_equal(igraph_version$coords[,1], position_tibble$x)
  expect_equal(igraph_version$coords[,2], -1*position_tibble$y) ## NEGATIVE!!!!
  
  # check graph itself---------------
  graph <- igraph_version$igraph_network
  expect_true(class(graph) == "igraph")
  
  # node color
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$group =="element" & 
                                           !(igraph::V(graph)$name %in% custom_selection_element) &
                                           !(igraph::V(graph)$name == focal)] == blue))
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$name %in% custom_selection_element] == purple))
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$name == focal] == yellow))
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$group =="mineral"] == red))
  
  # frame should be NA
  expect_true(all(is.na(igraph::V(graph)$frame.color)))
  
  # edge color
  expect_true(all(igraph::E(graph)$color == purple))
  
  # sizes are _awful_, let's make sure they're numbers and element is bigger than mineral
  expect_true(all(is.numeric(igraph::V(graph)$size)))
  
  expect_true(min(igraph::V(graph)$size[igraph::V(graph)$group =="element"]) > min(igraph::V(graph)$size[igraph::V(graph)$group =="mineral"]))
  
  # font size
  expect_true(all(is.numeric(igraph::V(graph)$font.size)))
  expect_true(all(igraph::V(graph)$font.color[igraph::V(graph)$group =="element"] == black))
  expect_true(all(igraph::V(graph)$font.color[igraph::V(graph)$group =="mineral"] == purple))
  
  #shape
  expect_true(all(igraph::V(graph)$shape[igraph::V(graph)$group =="element"] == "circle"))
  expect_true(all(igraph::V(graph)$shape[igraph::V(graph)$group =="mineral"] == "square"))
  
})



