## Test that fct_export_network() works -----------------------------------------------
test_that("fct_export_network::* works ", {
  
  ## Build, test the position tibble
  position_tibble <- calculate_output_node_positions(true_styled_nodes, 
                                                     NULL, 
                                                     true_graph, 
                                                     "layout_with_fr", 
                                                     1)
                                                                    
  true_names <- sort(c(names(true_styled_nodes), "x", "y"))
  expect_equal(sort(names(position_tibble)), true_names)   
  
  ## Build, test the exporting     
  igraph_version <- visnetwork_to_igraph(position_tibble, 
                                         true_styled_edges, 
                                         1, ## baseline_output_element_size
                                         0.65, ## baseline_output_element_label_size
                                         1) ## baseline_output_mineral_size
  
  # check names
  expect_equal(sort(names(igraph_version)), sort(c("igraph_network", "coords", "vis_aspect_ratio")))
  
  # check vis_aspect_ratio
  expect_true(is.numeric(igraph_version$vis_aspect_ratio))
  expect_true(igraph_version$vis_aspect_ratio > 0)
  
  # check coordinates
  if (R.Version()$major < 4) expect_true(class(igraph_version$coords) == "matrix") 
  if (R.Version()$major >= 4) expect_true(inherits(igraph_version$coords, "matrix"))
  ### TIMES 10!
  expect_equal(igraph_version$coords[,1], position_tibble$x * 10)
  expect_equal(igraph_version$coords[,2], -1*position_tibble$y * 10) ## NEGATIVE!!!!
  
  # check graph itself---------------
  graph <- igraph_version$igraph_network
  expect_true(class(graph) == "igraph")
  
  # node color

  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$group == "element" & 
                                           !(igraph::V(graph)$name %in% true_special_element_id)] == true_element_color))
  
  
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$name %in% true_custom_selection_set_1] == true_custom_selection_color_1))
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$name %in% true_custom_selection_set_2] == true_custom_selection_color_2))
  
  
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$name == focal] == true_highlight_color))
  expect_true(all(igraph::V(graph)$color[igraph::V(graph)$group =="mineral"] == true_mineral_color))
  
  # frame should NOT be NA
  expect_true(all(!(is.na(igraph::V(graph)$frame.color))))
  
  # edge color
  expect_true(all(igraph::E(graph)$color == true_edge_color))
  
  # sizes are _awful_, let's make sure they're numbers and element is bigger than mineral
  expect_true(all(is.numeric(igraph::V(graph)$size)))
  
  expect_true(min(igraph::V(graph)$size[igraph::V(graph)$group =="element"]) > min(igraph::V(graph)$size[igraph::V(graph)$group =="mineral"]))
  
  # font size
  expect_true(all(is.numeric(igraph::V(graph)$font.size)))
  expect_true(all(igraph::V(graph)$font.color[igraph::V(graph)$group =="element"] == true_element_label_color))
  expect_true(all(igraph::V(graph)$font.color[igraph::V(graph)$group =="mineral"] == true_mineral_label_color))
  
  #shape
  expect_true(all(igraph::V(graph)$shape[igraph::V(graph)$group =="mineral"] == true_mineral_shape))
  expect_true(all(igraph::V(graph)$shape[igraph::V(graph)$group =="element"] == true_element_shape))
  
})



