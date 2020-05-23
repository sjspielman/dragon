## TEST THE Au network all ages: 6 clusters 


## Setup ----------------------------------------------------------------------------------------
blue   <- "#0000FF"
red    <- "#FF0000"
yellow <- "#FFFF00"
purple <- "#800080"
black  <- "#000000"
focal <- "Au"
custom_selection_element <- "Bi"
size_scale <- 30
age_data    <- initialize_data_age(initialize_data(focal, FALSE), c(0, 5), "Maximum")
network_raw <- construct_network(age_data$elements_only_age, TRUE)
nodes       <- add_shiny_node_titles(network_raw$nodes, FALSE)
clustered     <- specify_community_detect_network(network_raw$graph, nodes, "Louvain", "Dark2")
cluster_colors <- clustered$cluster_colors
full_nodes <- clustered$nodes

style_options <- list("color_by_cluster"    = F,
                      "cluster_colors"       = cluster_colors,
                      "color_mineral_by"    = "singlecolor", ## num_localities
                      "mineral_color"       = red,
                      "color_element_by"    = "element_redox_network", ## element_redox_network, element_hsab
                      "element_color"       = blue,
                      "mineral_palette"     = "Reds",
                      "element_palette"     = "Blues",
                      "mineral_label_color" = black,
                      "element_label_color" = black,
                      "mineral_shape"       = "square", 
                      "element_shape"       = "circle", 
                      ## Single element colors, etc.
                      "elements_of_interest"     = focal,
                      "elements_by_redox"        = F,
                      "highlight_element"        = T,
                      "highlight_color"          = yellow,
                      "custom_selection_element" = custom_selection_element,
                      "custom_selection_color"   = purple,
                      ## Sizes
                      "element_size_type"  = "singlesize", ## num_localities 
                      "element_label_size" = size_scale,
                      "element_size_scale" = size_scale,  ### used if element_label_size != singlesize eg num_localities
                      "mineral_size_type"  = "singlesize", ## num_localities 
                      "mineral_size_scale" = size_scale,  ### used if mineral_size_type != singlesize eg num_localities
                      "mineral_label_size" = size_scale,
                      "mineral_size"       = size_scale,
                      ## Edges
                      "color_edge_by" = "singlecolor", # mean_pauling
                      "edge_color"    = purple,
                      "edge_palette"  = "Greens"
                     )



## Test style_nodes(), cluster colors  -----------------------------------------------
test_that("fct_style_network::style_nodes_colors_legend() clusters are colored", {
  

  style_options[["color_by_cluster"]] <- T
  node_attr <- style_nodes_colors_legend(full_nodes, list(), style_options)
  expect_true(all(is.na(node_attr[["both_legend"]])) == FALSE)
  expect_equal(sort(names(node_attr[["colors"]])), sort(c("id", "label", "color.background")))
  expect_equal( sort(unique(node_attr[["colors"]]$color.background)) , sort(cluster_colors) )
  style_options[["color_by_cluster"]] <- F  
  
})


## Test style_nodes(), single colors  -----------------------------------------------
test_that("fct_style_network::style_nodes_colors_legend() singles are colored", {
  

  node_attr <- style_nodes_colors_legend(full_nodes, list(), style_options)
  expect_true(all(is.na(node_attr[["both_legend"]])) == TRUE)
  expect_true(all(is.na(node_attr[["element_legend"]])) == FALSE)
  expect_true(all(is.na(node_attr[["mineral_legend"]])) == FALSE)
  
  expect_equal(sort(unique(node_attr[["colors"]]$color.background)), sort(c(blue,red)))
  
})

  ## $colors
  colors <- styled$colors
  expect_true(sort(names(colors)), sort(c("label", "id", "color.background")))
  expect_true(nrow(colors) == nrow(na.omit(colors)))
  expect_true(all(colors$color.background[colors$group == "element" & 
                                            colors$id != custom_selection_element & 
                                            colors$id != focal]) == blue) 
  expect_true(all(colors$color.background[colors$id == custom_selection_element]) == purple) 
  expect_true(all(colors$color.background[colors$id == focal]) == yellow) 
  expect_true(all(colors$color.background[colors$group == "mineral"]) == red) 
  
  ## $sizes
  sizes <- styled$sizes
  expect_true(sort(names(sizes)), sort(c("label", "id", "group", "size", "font.size")))
})

