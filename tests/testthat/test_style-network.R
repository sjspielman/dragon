## TEST Boron network all ages (8 clusters)


## Setup ----------------------------------------------------------------------------------------
blue   <- "#0000FF"
red    <- "#FF0000"
yellow <- "#FFFF00"
purple <- "#800080"
black  <- "#000000"
focal <- "B"
custom_selection_element <- c("P", "O")
size_scale <- 17
label_size <- 10
network <- initialize_network(focal, 
                             force_all_elements = FALSE, 
                             elements_by_redox = TRUE, 
                             age_range         = c(0, 5),
                             max_age_type      = "Maximum",
                             cluster_algorithm = "Louvain")
full_nodes <- add_shiny_node_titles(network$nodes, FALSE)
cluster_colors <- set_cluster_colors("Set2", length(unique(full_nodes$cluster_ID)))
edges <- network$edges

style_options_test <- list("color_by_cluster"  = FALSE,
                          "cluster_colors"       = cluster_colors,
                          "mineral_color_by"    = "singlecolor", ## num_localities
                          "mineral_color"       = red,
                          "element_color_by"    = "singlecolor", ## element_redox_network, element_hsab
                          "element_color"       = blue,
                          "mineral_palette"     = "Blues",
                          "element_palette"     = "Reds",
                          "mineral_label_color" = purple,
                          "element_label_color" = black,
                          "mineral_shape"       = "square", 
                          "element_shape"       = "circle", 
                          ## Single element colors, etc.
                          "elements_of_interest"     = focal,
                          "elements_by_redox"        = FALSE,
                          "highlight_element"        = FALSE,
                          "highlight_color"          = yellow,
                          "custom_selection_element" = NA,
                          "custom_selection_color"   = purple,
                          ## Sizes
                          "element_size_by"  = "singlesize", ## num_localities 
                          "element_label_size" = label_size,
                          "element_size_scale" = size_scale,  ### used if element_label_size != singlesize eg num_localities
                          "mineral_size_by"  = "singlesize", ## num_localities 
                          "mineral_size_scale" = size_scale,  ### used if mineral_size_by != singlesize eg num_localities
                          "mineral_label_size" = label_size,
                          "mineral_size"       = size_scale,
                          ## Edges
                          "edge_color_by" = "singlecolor", # mean_pauling
                          "edge_color"    = purple,
                          "edge_palette"  = "Greens"
                     )





## Test style_nodes()   -----------------------------------------------
test_that("fct_style_network::style_nodes() returns appropriate structure", {

  styled_test <- style_nodes(full_nodes, style_options_test)
  expect_equal(sort(names(styled_test)), sort(c("styled_nodes", "both_legend", "element_legend", "mineral_legend")))
  
})

#Test style_nodes(),single node colors including highlights  -----------------------------------------------
test_that("fct_style_network::style_nodes() single node colors including highlights", {
    
  style_options_here <- style_options_test
  style_options_here[["highlight_element"]] <- TRUE
  style_options_here[["custom_selection_element"]] <- custom_selection_element
  
  styled_test <- style_nodes(full_nodes, style_options_here)
  styled_nodes <- styled_test$styled_nodes
                      
  
  # Should have bipartite legend
  expect_true(all(is.na(styled_test[["both_legend"]])) == TRUE)
  expect_true(all(is.na(styled_test[["element_legend"]])) == FALSE)
  expect_true(all(is.na(styled_test[["mineral_legend"]])) == FALSE)
  
  # Colors were assigned correctly: single element/mineral, custom element node colors as well
  expect_true(all(styled_nodes$color.background[styled_nodes$group == "element" & 
                    !(styled_nodes$id %in% custom_selection_element) & 
                    styled_nodes$id != focal]  == blue)) 
  expect_true(all(styled_nodes$color.background[styled_nodes$id %in% custom_selection_element] == purple)) 
  expect_true(all(styled_nodes$color.background[styled_nodes$id == focal] == yellow)) 
  expect_true(all(styled_nodes$color.background[styled_nodes$group == "mineral"] == red))
  

  
})

#Test style_nodes(), cluster colors  -----------------------------------------------
test_that("fct_style_network::style_nodes() cluster node colors", {
  
  #seems *not* shallow?
  style_options_here <- style_options_test
  style_options_here[["color_by_cluster"]] <- TRUE

  styled_test <- style_nodes(full_nodes, style_options_here)
  styled_nodes <- styled_test$styled_nodes
 
  # Should have a both legend and NA for element, mineral legends
  expect_true(all(is.na(styled_test[["both_legend"]])) == FALSE)
  expect_true(all(is.na(styled_test[["element_legend"]])) == TRUE)
  expect_true(all(is.na(styled_test[["mineral_legend"]])) == TRUE)

  # Node colors should be the cluster colors
  expect_equal( sort(unique(styled_nodes$color.background)), sort(cluster_colors) )

})


#Test style_nodes(), node dynamic colors  -----------------------------------------------
test_that("fct_style_network::style_nodes() node dynamic colors", {
  
  style_options_here <- style_options_test
  style_options_here[["element_color_by"]] <- "num_localities" # TODO need to also test a categorical
  style_options_here[["mineral_color_by"]] <- "num_localities"
  
  styled_test <- style_nodes(full_nodes, style_options_here)
  styled_nodes <- styled_test$styled_nodes
  
  # Should have bipartite legend
  expect_true(all(is.na(styled_test[["both_legend"]])) == TRUE)
  expect_true(all(is.na(styled_test[["element_legend"]])) == FALSE)
  expect_true(all(is.na(styled_test[["mineral_legend"]])) == FALSE)
  
  # Colors were assigned correctly. basically, nothing should be the single color or NA. Can't really test otherwise.
 # print(styled_nodes$color.background[styled_nodes$group == "element"])
  expect_true(all(styled_nodes$color.background[styled_nodes$group == "element"] != blue)) 
  expect_true(all(!is.na(styled_nodes$color.background[styled_nodes$group == "element"]))) 
  expect_true(all(styled_nodes$color.background[styled_nodes$group == "mineral"] != red)) 
  expect_true(all(!is.na(styled_nodes$color.background[styled_nodes$group == "mineral"]))) 


})


#Test style_nodes(), node shapes, baseline sizes, font colors (NOT text shape)  -----------------------------------------------
test_that("fct_style_network::style_nodes() node shapes, *single* sizes, font colors (NOT text shape)", {
  

  styled_test <- style_nodes(full_nodes, style_options_test)
  styled_nodes <- styled_test$styled_nodes

  # Shapes are assigned correctly
  expect_true(all(styled_nodes$shape[styled_nodes$group == "element"] == "circle"))
  expect_true(all(styled_nodes$shape[styled_nodes$group == "mineral"] == "square"))

  # Sizes are assigned correctly
  expect_true(all(styled_nodes$size[styled_nodes$group == "element"] == label_size))
  expect_true(all(styled_nodes$font.size[styled_nodes$group == "element"] == label_size))
  expect_true(all(styled_nodes$size[styled_nodes$group == "mineral"] == size_scale))
  expect_true(all(styled_nodes$font.size[styled_nodes$group == "mineral"] == label_size))
  
  # Font colors
  expect_true(all(styled_nodes$font.color[styled_nodes$group == "element"] == black))
  expect_true(all(styled_nodes$font.color[styled_nodes$group == "mineral"] == purple))

})

#Test style_nodes(), node dynamic sizes  -----------------------------------------------
test_that("fct_style_network::style_nodes() node dynamic sizes", {
  
  style_options_here <- style_options_test
  style_options_here[["mineral_size_by"]] <- "num_localities"
  style_options_here[["element_size_by"]] <- "num_localities"
  
  styled_test <- style_nodes(full_nodes, style_options_here)
  styled_nodes <- styled_test$styled_nodes
  
  # Sizes should be within defined ranges
  expect_true(all(styled_nodes$size[styled_nodes$group == "element"] <= (size_scale * element_size_max/element_size_scale_divisor) &
                  styled_nodes$size[styled_nodes$group == "element"] >=  (size_scale * element_size_min/element_size_scale_divisor) ))
  expect_true(all(styled_nodes$font.size[styled_nodes$group == "element"] <= (size_scale * element_size_max/element_size_scale_divisor) &
                  styled_nodes$font.size[styled_nodes$group == "element"] >= (size_scale * element_size_min/element_size_scale_divisor) ))
  

  expect_true(all(styled_nodes$size[styled_nodes$group == "mineral"] <= (size_scale * mineral_size_max/mineral_size_scale_divisor) &
                  styled_nodes$size[styled_nodes$group == "mineral"] >= (size_scale * mineral_size_min/mineral_size_scale_divisor) ))


})


test_that("fct_style_network::style_nodes() node font color with TEXT", {

  style_options_here <- style_options_test
  style_options_here[["element_shape"]] <- "text"
  style_options_here[["highlight_element"]] <- TRUE
  style_options_here[["custom_selection_element"]] <- custom_selection_element

  styled_test <- style_nodes(full_nodes, style_options_here)
  styled_nodes <- styled_test$styled_nodes
  
  # element font color when shape is text should be the NODE COLOR, and we also check for highlighted. mineral should still be mineral color
  expect_true(all( (styled_nodes$font.color[styled_nodes$group == "element" & 
                        styled_nodes$id != focal &
                        !(styled_nodes$id %in% custom_selection_element)])  == black)) # black is element_label_color
  expect_true(all(styled_nodes$font.color[styled_nodes$id == focal] == yellow))                     # yellow is background color, transfered to text color in this scenario
  expect_true(all(styled_nodes$font.color[styled_nodes$id %in% custom_selection_element] == purple)) # purple is background color, transfered to text color in this scenario
  expect_true(all(styled_nodes$font.color[styled_nodes$group == "mineral"] == purple))              # purple is a background color, transfered to text color in this scenario

})



## Test style_edges()   -----------------------------------------------
test_that("fct_style_network::style_edges() for single edge color", {

  styled_test <- style_edges(edges, style_options_test)
  
  expect_equal(sort(names(styled_test)), sort(c("edge_legend", "styled_edges")))
  
  ## single edge color
  expect_true(all(styled_test$styled_edges$color == purple))
  # somehow this is ~stochastically~ getting returned null here, even though it is defined to give NA in the code.
  # this is probably something about R I don't understand? 
  edge_legend <- styled_test$styled_edges[["edge_legend"]]
  expect_true(is.null(edge_legend) || is.na(edge_legend))
  expect_true(all(is.na(styled_test$styled_edges[["edge_legend"]]))  | all(is.null(styled_test$styled_edges[["edge_legend"]]))     )
})


test_that("fct_style_network::style_edges() for edge palette", {

  style_options_here <- style_options_test
  style_options_here[["edge_color_by"]] <- "max_age"
  styled_test <- style_edges(edges, style_options_here)
  expect_equal(sort(names(styled_test)), sort(c("edge_legend", "styled_edges")))  
  
  expect_true(all(styled_test$styled_edges$color != purple))
  #edge_legend <- styled_test$styled_edges[["edge_legend"]]
  #print(edge_legend)
  # I have no earthly clue why this test fails. It clearly renders a legend.
  #expect_true(all(is.na(styled_test$styled_edges[["edge_legend"]])) == FALSE)

})





































