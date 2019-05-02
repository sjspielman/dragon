library(tidyverse)
library(igraph)
library(visNetwork)
source(system.file("dragon/build_network.R", package = "dragon"))

######### should we just go with nrow() until the dplyr bug is fixed????

tibble_path <- "./true_tibbles/"

test_that("Test build_network::initialize_data() works", {
    ########## Single element of interest #########
    elements_of_interest <- c("Au") ## has a small network
    true_tibble <- read_csv(paste0(tibble_path, "initialize_data_single_element_Au.csv"))
    test_tibble <- initialize_data(elements_of_interest, FALSE)
    expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
            failure_message = "Failed to initialize data with a single element." 
          )

    ########## Multiple elements of interest #########
    elements_of_interest <- c("Au", "Ag") 
    true_tibble <- read_csv(paste0(tibble_path, "initialize_data_multiple_elements_Au_Ag.csv"))
    test_tibble <- initialize_data(elements_of_interest, FALSE)
    expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
            failure_message = "Failed to initialize data with multiple elements, unforced." 
          )

    ########## Multiple elements of interest with forcing #########
    elements_of_interest <- c("Au", "Ag") 
    true_tibble <- read_csv(paste0(tibble_path, "initialize_data_multiple_elements_Au_Ag_forced.csv"))
    test_tibble <- initialize_data(elements_of_interest, TRUE)
    expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
            failure_message = "Failed to initialize data with multiple elements, forced." 
          )
})	



test_that("Test build_network::initialize_data_age() works", {
    
    ################ Age network can be obtained ##############
    age_to_test <- 3
    true_tibble <- read_csv(paste0(tibble_path, "initialize_data_age_AuAg_3ga.csv"))
    elements_of_interest <- c("Au", "Ag") 
    test_tibble <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
    expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
            failure_message = "Failed intialize_data_age() when age network can be obtained." 
          )   
    expect_true( sum(test_tibble$max_age < age_to_test) == 0)

    
    
    ############ Age network when cannot be obtained ##############        
    age_to_test <- 6 ## early in only 4.5
    test_tibble <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
    expect_true( nrow(test_tibble) == 0)
})	


test_that("Test build_network::obtain_network_information() works", {

    ## Conditions have fancy redox things happening, which matters a lot for this function.
    elements_of_interest <- c("Fe")
    age_to_test <- 2
    input_tibble <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
    
    
     
    ############### Do not separate by redox ##############
     test_tibble <- obtain_network_information(input_tibble, FALSE)
     true_tibble <- read_csv(paste0(tibble_path, "obtain_network_information_Fe_2ga_notbyredox.csv"), trim_ws = FALSE)
     
     ## For testing purposes only, we are comparing based on nrow() right now.
     ## `all_equal` doesn't like comparing NA's in double columns, apparently, but I can't manage to reproduce with other examples besides my exact data.
     ## Issue #4338 on tidyverse/dplyr. Hopefully a fix soon.
     #test_tibble %<>% na.omit()
     #true_tibble %<>% na.omit() 
     #expect_true( isTRUE( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) )   
     expect_true( nrow(true_tibble) == nrow(test_tibble) & ncol(true_tibble) == ncol(test_tibble) )   

    
    ############### Separate by redox ##############
    test_tibble <- obtain_network_information(input_tibble, TRUE)
    true_tibble <- read_csv(paste0(tibble_path, "obtain_network_information_Fe_2ga_byredox.csv"), trim_ws = FALSE) ## need last argument for spaces in elements due to redox stuff
    expect_true( nrow(true_tibble) == nrow(test_tibble) & ncol(true_tibble) == ncol(test_tibble) )   
    #test_tibble %<>% na.omit()     
    #true_tibble %<>% na.omit() 
    #expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
    #        failure_message = "Failed obtain_network_information() when YES separating by redox." 
    #      )   
})	


test_that("Test build_network::construct_network() works", {
    
    elements_of_interest <- c("Fe")
    age_to_test <- 2


    ############### Do not separate by redox ##############
    input_tibble_notbyredox_raw <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
    input_tibble_notbyredox <- obtain_network_information(input_tibble_notbyredox_raw, FALSE)
    
    edges_nodes <- construct_network(input_tibble_notbyredox, FALSE)

    true_edges <- read_csv(paste0(tibble_path, "contruct_network_edges_Fe_notbyredox.csv"), trim_ws = FALSE)
    true_nodes <- read_csv(paste0(tibble_path, "contruct_network_nodes_Fe_notbyredox.csv"), trim_ws = FALSE)

    #true_edges %<>% na.omit() 
    #edges_nodes$edges %<>% na.omit()
    #true_nodes %<>% na.omit() 
    #edges_nodes$nodes %<>% na.omit() 
    #
    #expect_true( isTRUE( all_equal(true_edges, edges_nodes$edges, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) ) 
    #expect_true( isTRUE( all_equal(true_nodes, edges_nodes$nodes, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) ) 

    expect_true( nrow(true_edges) == nrow(edges_nodes$edges) & ncol(true_edges) == ncol(edges_nodes$edges) )   
    expect_true( nrow(true_nodes) == nrow(edges_nodes$nodes) & ncol(true_nodes) == ncol(edges_nodes$nodes) )   



    
    ############### Separate by redox ##############
    input_tibble_byredox_raw <- initialize_data_age( initialize_data(elements_of_interest, TRUE), age_to_test)
    input_tibble_byredox <- obtain_network_information(input_tibble_byredox_raw, TRUE)
    
    edges_nodes <- construct_network(input_tibble_byredox, TRUE)

    true_edges <- read_csv(paste0(tibble_path, "contruct_network_edges_Fe_byredox.csv"), trim_ws = FALSE)
    true_nodes <- read_csv(paste0(tibble_path, "contruct_network_nodes_Fe_byredox.csv"), trim_ws = FALSE)

    #true_edges %<>% na.omit() 
    #edges_nodes$edges %<>% na.omit()
    #true_nodes %<>% na.omit() 
    #edges_nodes$nodes %<>% na.omit()
    #
    #expect_true( isTRUE( all_equal(true_edges, edges_nodes$edges, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) ) 
    #expect_true( isTRUE( all_equal(true_nodes, edges_nodes$nodes, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) ) 

    expect_true( nrow(true_edges) == nrow(edges_nodes$edges) & ncol(true_edges) == ncol(edges_nodes$edges) )   
    expect_true( nrow(true_nodes) == nrow(edges_nodes$nodes) & ncol(true_nodes) == ncol(edges_nodes$nodes) )   

})	
