############ Recreate the true tibbles #############

library(remotes)
install_github("spielmanlab/dragon")

library(tidyverse)
library(igraph)
library(visNetwork)
source(system.file("dragon/build_network.R", package = "dragon"))

elements_of_interest <- c("Au") ## has a small network
initialize_data(elements_of_interest, FALSE) %>% write_csv("initialize_data_single_element_Au.csv")

elements_of_interest <- c("Au", "Ag") 
test_tibble <- initialize_data(elements_of_interest, FALSE)
initialize_data(elements_of_interest, FALSE) %>% write_csv("initialize_data_multiple_elements_Au_Ag.csv")
initialize_data(elements_of_interest,TRUE) %>% write_csv("initialize_data_multiple_elements_Au_Ag_forced.csv")

age_to_test <- c(1,3)
initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test) %>% write_csv("initialize_data_age_AuAg_1-3ga.csv")


elements_of_interest <- c("Fe")
age_to_test <- c(2,3)
algorithm <- "Louvain"

input_tibble <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
obtain_network_information(input_tibble, FALSE) %>% write_csv("obtain_network_information_Fe_2-3ga_notbyredox.csv")
obtain_network_information(input_tibble, TRUE) %>% write_csv("obtain_network_information_Fe_2-3ga_byredox.csv")
    
input_tibble_notbyredox_raw <- initialize_data_age( initialize_data(elements_of_interest, FALSE), age_to_test)
input_tibble_notbyredox <- obtain_network_information(input_tibble_notbyredox_raw, FALSE)
edges_nodes <- construct_network(input_tibble_notbyredox, FALSE, algorithm)
write_csv(edges_nodes$edges, "contruct_network_edges_Fe_notbyredox.csv")
write_csv(edges_nodes$nodes, "contruct_network_nodes_Fe_notbyredox.csv")

input_tibble_byredox_raw <- initialize_data_age( initialize_data(elements_of_interest, TRUE), age_to_test)
input_tibble_byredox <- obtain_network_information(input_tibble_byredox_raw, TRUE)
edges_nodes <- construct_network(input_tibble_byredox, TRUE, algorithm)
write_csv(edges_nodes$nodes, "contruct_network_nodes_Fe_byredox.csv")
write_csv(edges_nodes$edges, "contruct_network_edges_Fe_byredox.csv")
