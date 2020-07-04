## Test style_nodes()   -----------------------------------------------
test_that("fct_style_network::style_nodes() returns appropriate structure and baseline stylings", {

  styled_test <- style_nodes(true_nodes, true_style_options)
  expect_equal(sort(names(styled_test)), sort(c("styled_nodes", "both_legend", "element_legend", "mineral_legend")))
    
  # Should have separate element and mineral legends
  expect_true(styled_test[["both_legend"]] == FALSE)
  expect_true(typeof(styled_test[["element_legend"]]) == "list")
  expect_true(typeof(styled_test[["mineral_legend"]]) == "list")
  
  
  styled_nodes_test <- styled_test$styled_nodes
  
  # Colors were assigned correctly: single element/mineral, custom element node colors as well
  regular_element_nodes <- styled_nodes_test$color.background[styled_nodes_test$group == "element" & !(styled_nodes_test$id %in% true_special_element_id)]  
  
  expect_true(all(regular_element_nodes == true_element_color))
  ## custom selection
  expect_true(all(styled_nodes_test$color.background[styled_nodes_test$id %in% true_custom_selection_set_1] == true_custom_selection_color_1)) 
  expect_true(all(styled_nodes_test$color.background[styled_nodes_test$id %in% true_custom_selection_set_2] == true_custom_selection_color_2)) 
  

  ## focal
  expect_true(all(styled_nodes_test$color.background[styled_nodes_test$group == "element" & styled_nodes_test$element_name == true_focal_element_name] == true_highlight_color)) 
  expect_true(all(styled_nodes_test$color.background[styled_nodes_test$group == "mineral"] == true_mineral_color))
  
  # Shapes are assigned correctly
  expect_true(all(styled_nodes_test$shape[styled_nodes_test$group == "element"] == true_element_shape))
  expect_true(all(styled_nodes_test$shape[styled_nodes_test$group == "mineral"] == true_mineral_shape))

  # Sizes are assigned correctly
  expect_true(all(styled_nodes_test$size[styled_nodes_test$group == "element"] == true_element_label_size))
  expect_true(all(styled_nodes_test$font.size[styled_nodes_test$group == "element"] == true_element_label_size))
  expect_true(all(styled_nodes_test$size[styled_nodes_test$group == "mineral"] == true_mineral_size))
  expect_true(all(styled_nodes_test$font.size[styled_nodes_test$group == "mineral"] == true_mineral_label_size))
  
  # Font colors
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$group == "element"] == true_element_label_color))
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$group == "mineral"] == true_mineral_label_color))

  
})



#Test style_nodes(), cluster colors  -----------------------------------------------
test_that("fct_style_network::style_nodes() cluster node colors", {
  
  #seems *not* shallow?
  style_options_here <- true_style_options
  style_options_here[["color_by_cluster"]] <- TRUE
  style_options_here[["highlight_element"]] <- FALSE
  style_options_here[["custom_element_colors"]] <- ""

  styled_test <- style_nodes(true_nodes, style_options_here)
  styled_nodes_test <- styled_test$styled_nodes
 
  # Should have a both legend and NA for element, mineral legends
  expect_true(typeof(styled_test[["both_legend"]]) == "list")
  expect_true(styled_test[["element_legend"]] == FALSE)
  expect_true(styled_test[["mineral_legend"]] == FALSE)

  # Node colors should be the cluster colors
  expect_equal( sort(unique(styled_nodes_test$color.background)), sort(true_cluster_colors) )

})


#Test style_nodes(), node dynamic colors  -----------------------------------------------
test_that("fct_style_network::style_nodes() node dynamic colors, sizes", {
  
  style_options_here <- true_style_options
  # Override colors for dynamic
  style_options_here[["highlight_element"]] <- FALSE
  style_options_here[["custom_element_colors"]] <- ""
  style_options_here[["element_color_by"]] <- true_element_color_by_dynamic_type
  style_options_here[["mineral_color_by"]] <- true_mineral_color_by_dynamic_type
  # Override sizes for dynamic
  style_options_here[["mineral_size_by"]] <- true_mineral_size_by_dynamic_type
  style_options_here[["element_size_by"]] <- true_element_size_by_dynamic_type
    
  styled_test <- style_nodes(true_nodes, style_options_here)
  styled_nodes_test <- styled_test$styled_nodes
  
  # Should have separate mineral, element legend
  expect_true(styled_test[["both_legend"]] == FALSE)
  expect_true(typeof(styled_test[["element_legend"]]) == "list")
  expect_true(typeof(styled_test[["mineral_legend"]]) == "list")
  
  # Colors were assigned correctly. 
  expect_equal(styled_nodes_test$color.background[styled_nodes_test$group == "element"], true_element_color_by_dynamic_vals) 
  expect_equal(styled_nodes_test$color.background[styled_nodes_test$group == "mineral"], true_mineral_color_by_dynamic_vals) 

  # Sizes were assigned correctly
  expect_equal(styled_nodes_test$size[styled_nodes_test$group == "element"], true_element_size_by_dynamic_vals, tolerance = 1e-3)
  expect_equal(styled_nodes_test$size[styled_nodes_test$group == "mineral"], true_mineral_size_by_dynamic_vals, tolerance = 1e-3)
  
})



test_that("fct_style_network::style_nodes() node font color with TEXT", {

  style_options_here <- true_style_options
  style_options_here[["element_shape"]] <- "text"

  styled_test <- style_nodes(true_nodes, style_options_here)
  styled_nodes_test <- styled_test$styled_nodes
  
  # element font color when shape is text should be the NODE COLOR, and we also check for highlighted. mineral should still be mineral color
  regular_element_nodes <- styled_nodes_test$font.color[styled_nodes_test$group == "element" & !(styled_nodes_test$id %in% true_special_element_id)]
  expect_true(all((regular_element_nodes == true_element_color)))
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$group == "element" &
                                          styled_nodes_test$element_name == true_focal_element_name] == true_highlight_color))                
 
 
 
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$id %in% true_custom_selection_set_1] == true_custom_selection_color_1)) 
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$id %in% true_custom_selection_set_2] == true_custom_selection_color_2)) 
  expect_true(all(styled_nodes_test$font.color[styled_nodes_test$group == "mineral"] == true_mineral_label_color))             
})






## Test style_edges()   -----------------------------------------------
test_that("fct_style_network::style_edges()", {

  styled_test <- style_edges(true_edges, true_style_options)
  expect_equal(sort(names(styled_test)), sort(c("edge_legend", "styled_edges")))
  
  ## single edge color
  expect_true(all(styled_test$styled_edges$color == true_edge_color))
  edge_legend <- styled_test[["edge_legend"]]
  expect_true(edge_legend == FALSE)
  
  ## dynamic edge color
  style_options_here <- true_style_options
  style_options_here[["edge_color_by"]] <-true_edge_color_by_dynamic
  styled_test <- style_edges(true_edges, style_options_here)
  
  expect_equal(sort(names(styled_test)), sort(c("edge_legend", "styled_edges")))  
  expect_equal(styled_test$styled_edges$color, true_edge_color_by_vals)
  edge_legend <- styled_test[["edge_legend"]]
  expect_true(typeof(edge_legend) == "list")
})







































