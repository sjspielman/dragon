## Test initialize_data(), single element -----------------------------------------------
test_that("fct_build_network::initialize_data() with a single element", {
  
  elements_of_interest <- "Cd"
  test_output <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)

  ## The number of rows WITH Cd should be all of them
  test_output %>%
    dplyr::filter(stringr::str_detect(rruff_chemistry, "Cd")) -> rows_with_cd
  expect_equal(nrow(rows_with_cd), nrow(test_output))
  
  ## The number of rows WITHOUT Cd should be 0
  test_output %>%
    dplyr::filter(!(stringr::str_detect(rruff_chemistry, "Cd"))) -> rows_without_cd
  expect_equal(nrow(rows_without_cd), 0)  
  
  ## Check the column names
  expected_names <- c("mineral_name", "mineral_id", "mindat_id", "locality_longname" , "age_type", "rruff_chemistry", "ima_chemistry", "min_age","max_age", "chemistry_elements")
  expect_equal(sort(names(test_output)), sort(expected_names))  
})


## Test initialize_data(), multiple elements unforced -----------------------------------
test_that("fct_build_network::initialize_data() with multiple unforced elements", {
  elements_of_interest <- c("Cd", "Sn")
  test_output <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  
  ## The number of rows WITH Cd or Sn should be all of them
  test_output %>%
    dplyr::rowwise() %>%
    dplyr::filter(stringr::str_detect(rruff_chemistry, "Cd") | stringr::str_detect(rruff_chemistry, "Sn")) -> rows_with_cd_sn
  expect_equal(nrow(rows_with_cd_sn), nrow(test_output))
  
  ## The number of rows WITHOUT Cd or Sn should be 0
  test_output %>%
    dplyr::filter(!(stringr::str_detect(rruff_chemistry, "Cd")) & !(stringr::str_detect(rruff_chemistry, "Sn"))) -> rows_without_cd_sn
  expect_equal(nrow(rows_without_cd_sn), 0)  
  
  ## Check the column names
  expected_names <- c("mineral_name", "mineral_id", "mindat_id", "locality_longname" , "age_type", "rruff_chemistry", "ima_chemistry", "min_age","max_age", "chemistry_elements")
  expect_equal(sort(names(test_output)), sort(expected_names))  
  
})

## Test initialize_data(), multiple elements forced -----------------------------------
test_that("fct_build_network::initialize_data() with multiple forced elements", {
  elements_of_interest <- c("As", "Cd", "Cu")
  test_output <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, TRUE)
  
  ## The number of rows WITH should be all of them
  test_output %>%
    dplyr::rowwise() %>%
    dplyr::filter(stringr::str_detect(rruff_chemistry, "As") | 
                  stringr::str_detect(rruff_chemistry, "Cd") |
                  stringr::str_detect(rruff_chemistry, "Cu")) -> rows_with
  expect_equal(nrow(rows_with), nrow(test_output))
  
  ## The number of rows WITHOUT should be 0
  test_output %>%
    dplyr::filter(!(stringr::str_detect(rruff_chemistry, "As")) & 
                  !(stringr::str_detect(rruff_chemistry, "Cd")) &
                    !(stringr::str_detect(rruff_chemistry, "Cu"))) -> rows_without
  expect_equal(nrow(rows_without), 0)  
  
  ## Check the column names
  expected_names <- c("mineral_name", "mineral_id", "mindat_id", "locality_longname" , "age_type", "rruff_chemistry", "ima_chemistry", "min_age","max_age", "chemistry_elements")
  expect_equal(sort(names(test_output)), sort(expected_names))  
  
})

## Test initialize_data_age(), using Maximum age ----------------------------------------
test_that("fct_build_network::initialize_data_age() using maximum known age", {
  elements_of_interest <- c("Cd")
  age_range <- c(1, 2.5)
  test_input <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  test_output <- initialize_data_age(test_input, age_range, "Maximum")
  
  ## Should return two df's
  expect_equal(sort(names(test_output)), c("elements_only_age", "locality_info"))
  
  ## Test elements_only_age is correct
  elements_only_age_output <- test_output$elements_only_age 
  expected_names_one <- c("mineral_name", "mineral_id", "max_age", "num_localities_mineral", "ima_chemistry", "rruff_chemistry", "chemistry_elements")
  expect_equal(sort(names(elements_only_age_output)), sort(expected_names_one))  
  output_age_range <- range(elements_only_age_output$max_age)
  expect_true(output_age_range[1] >= age_range[1] & output_age_range[2] <= age_range[2])
  
  ## Test locality info is correct, assuming MAXIMUM!!
  locality_output <- test_output$locality_info 
  expect_equal(sort(names(locality_output)), sort(true_locality_names))  
  locality_max_age_range <- range(locality_output$max_age_locality)
  expect_true(locality_max_age_range[1] >= age_range[1] & locality_max_age_range[2] <= age_range[2])

})



## Test initialize_data_age(), using Minimum age ----------------------------------------
test_that("fct_build_network::initialize_data_age() using minimum known age", {
  elements_of_interest <- c("Cd")
  age_range <- c(1, 2.5)
  test_input <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  test_output <- initialize_data_age(test_input, age_range, "Minimum")
  
  ## Should return two df's
  expect_equal(sort(names(test_output)), c("elements_only_age", "locality_info"))
  
  ## Test elements_only_age is correct
  elements_only_age_output <- test_output$elements_only_age 
  expected_names_one <- c("mineral_name", "mineral_id", "max_age", "num_localities_mineral", "ima_chemistry", "rruff_chemistry", "chemistry_elements")
  expect_equal(sort(names(elements_only_age_output)), sort(expected_names_one))  
  output_age_range <- range(elements_only_age_output$max_age)
  expect_true(output_age_range[1] >= age_range[1] & output_age_range[2] <= age_range[2])
  
  ## Test locality info is correct, assuming MINIMUM!!
  locality_output <- test_output$locality_info 
  expect_equal(sort(names(locality_output)), true_locality_names)  
  locality_min_age_range <- range(locality_output$min_age_locality)
  expect_true(locality_min_age_range[1] >= age_range[1] & locality_min_age_range[2] <= age_range[2])
  
})



## Test construct_network(), using elements_by_redox = FALSE --------------------------------
test_that("fct_build_network::construct_network() with elements_by_redox = F", {
  elements_of_interest <- c("Cd")
  age_range <- c(1, 2.5)
  age_data <- initialize_data_age(initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE), age_range, "Maximum")
  test_output <- construct_network(age_data$elements_only_age, FALSE, element_redox_states_cache)
  
  ## Length of 3 with correct names
  expected_names_one <- c("edges", "nodes", "network")
  expect_equal(sort(names(test_output)), sort(expected_names_one)) 
  
  ## Edges tests
  test_edges <- test_output$edges
  expect_equal(sort(names(test_edges)), true_edge_names) 
  expect_equal(test_edges$from, test_edges$mineral_name)
  
  # Without redox there should be NO +/- in `to`
  expect_true( sum(stringr::str_detect(test_edges$to, "\\+|-")) == 0)

  ## Nodes tests
  test_nodes <- test_output$nodes
  expect_equal(sort(names(test_nodes)), true_node_names_precluster) 
  expect_true(length(test_nodes$id) == length(unique(test_nodes$id)) )
  
  ## Edges, nodes compatible
  edges_to_nodes <- unique(c(test_edges$to, test_edges$from ))
  expect_equal(sort(edges_to_nodes), sort(test_nodes$id)) 
  
  ## No NAs in various fields "known" to be silly
  expect_true(sum(is.na(test_nodes$label)) == 0)
  expect_true(sum(is.na(test_nodes$id)) == 0)
  expect_true(sum(is.na(test_nodes$title)) == 0)
  expect_true(all(test_nodes$group %in% c("element", "mineral")))

   
})


## Test construct_network(), using elements_by_redox = TRUE --------------------------------
test_that("fct_build_network::construct_network() with elements_by_redox = T", {
  elements_of_interest <- c("Fe") ## Cd is disconnected
  age_range <- c(3, 4)
  age_data <- initialize_data_age(initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE), age_range, "Maximum")
  test_output <- construct_network(age_data$elements_only_age, TRUE, element_redox_states_cache)
  
  ## Length of 3 with correct names
  expected_names_one <- c("edges", "nodes", "network")
  expect_equal(sort(names(test_output)), sort(expected_names_one)) 
  
  ## Edges tests
  test_edges <- test_output$edges
  expect_equal(sort(names(test_edges)), true_edge_names) 
  expect_equal(test_edges$from, test_edges$mineral_name)
  
  # Without redox there SHOULD BE +/- in `to`
  expect_true( sum(stringr::str_detect(test_edges$to, "\\+")) != 0)
  
  ## Nodes tests
  test_nodes <- test_output$nodes
  expect_equal(sort(names(test_nodes)), true_node_names_precluster) 
  expect_true(length(test_nodes$id) == length(unique(test_nodes$id)) )

  ## Edges, nodes compatible
  edges_to_nodes <- unique(c(test_edges$to, test_edges$from ))
  expect_equal(sort(edges_to_nodes), sort(test_nodes$id)) 
  
  ## No NAs in various fields "known" to be silly
  expect_true(sum(is.na(test_nodes$label)) == 0)
  expect_true(sum(is.na(test_nodes$id)) == 0)
  expect_true(sum(is.na(test_nodes$title)) == 0)
  expect_true(all(test_nodes$group %in% c("element", "mineral")))
  
})




## Test specify_community_detect_network(), using Louvain --------------------------------
test_that("fct_build_network::specify_community_detect_network() with Louvain community clustering", {
  age_data <- initialize_data_age(initialize_data(med_data_cache, element_redox_states_cache, "Fe", FALSE), c(3, 4), "Maximum")
  network_raw <- construct_network(age_data$elements_only_age, TRUE, element_redox_states_cache)
  test_cluster <- specify_community_detect_network(network_raw$network, network_raw$nodes, "Louvain")
  
  ## Length of 3 with correct names
  expected_names_one <- c("nodes", "clustered_net")
  expect_equal(sort(names(test_cluster)), sort(expected_names_one)) 
  
  ## Check that node nodes contains the added cluster columns
  expected_names_nodes<- c("id", "cluster_ID", "cluster_algorithm", "title", "font.face", "label", "group", "network_degree", "closeness", "network_degree_norm", "mineral_id", "max_age", "ima_chemistry", "rruff_chemistry", "mean_pauling", "cov_pauling", "element_hsab", "atomic_mass", "number_of_protons", "table_period", "table_group", "atomic_radius", "pauling", "metal_type", "element_density", "element_specific_heat", "element_name", "element_redox_network", "num_localities")
  expect_equal(sort(names(test_cluster$nodes)), true_node_names) 
  
  
  ## Same lengths all around
  expect_true( length(test_cluster$clustered_net) == length(unique(test_cluster$nodes$cluster_ID)) )
  
})



## Test that initialize_network() works -----------------------------------------------
test_that("fct_build_network::initialize_network() works", {
  output <- initialize_network("Fe", 
                     force_all_elements = FALSE, 
                     elements_by_redox = FALSE, 
                     age_range         = c(0, 5),
                     max_age_type      = "Maximum",
                     cluster_algorithm = "Louvain")
  expect_equal(sort(names(output)), sort(c("edges", "nodes", "network", "clustering", "locality_info")))

  ## Crack full network
  output_all <- initialize_network("all", 
                     force_all_elements = FALSE, 
                     elements_by_redox = TRUE, 
                     age_range         = c(0, 5),
                     max_age_type      = "Maximum",
                     cluster_algorithm = "Louvain")
  expect_equal(sort(names(output_all)), sort(c("edges", "nodes", "network", "clustering", "locality_info")))
})

## Test that subset_mineral_nodes() works -----------------------------------------------
test_that("fct_build_network::subset_mineral_nodes() works", {

  test_mineral_nodes <- subset_mineral_nodes(true_nodes)
  expect_equal(sort(names(test_mineral_nodes)), true_mineral_node_names)
  expect_true(nrow(test_mineral_nodes) == true_n_mineral_nodes)
  
})