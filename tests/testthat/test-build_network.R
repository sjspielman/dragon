library(tidyverse)
library(igraph)
source(system.file("inst/dragon/", "build_network.R", package = "dragon"))



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
    age_to_test <- 6 ## impossible Earth age
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
     
     ## For testing purposes only, recode  NA = 1e5. 
     ## `all_equal` doesn't like comparing NA's in double columns, apparently, but I can't manage to reproduce with other examples besides my exact data.
     ## Issue #4338 on tidyverse/dplyr. Hopefully a fix soon.
     test_tibble %>% replace_na(list(sd_pauling = 1e5)) -> test_tibble
     true_tibble %>% replace_na(list(sd_pauling = 1e5)) -> true_tibble
     expect_true( isTRUE( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T, convert = TRUE) ) )   

    
    ############### Separate by redox ##############
    test_tibble <- obtain_network_information(input_tibble, TRUE)
    true_tibble <- read_csv(paste0(tibble_path, "obtain_network_information_Fe_2ga_byredox.csv"), trim_ws = FALSE) ## need last argument for spaces in elements due to redox stuff
    test_tibble %>% replace_na(list(sd_pauling = 1e5, mean_element_redox = 1e5, redox_sign = 1e5)) -> test_tibble
    true_tibble %>% replace_na(list(sd_pauling = 1e5, mean_element_redox = 1e5, redox_sign = 1e5)) -> true_tibble
    expect( all_equal(true_tibble, test_tibble, ignore_col_order = T, ignore_row_order = T),
            failure_message = "Failed obtain_network_information() when YES separating by redox." 
          )   
})	






















obtain_network_information <- function(elements_only_age, elements_by_redox)
{      
    network_information <- elements_only_age %>%    
                            select(mineral_name, num_localities, max_age, chemistry_elements) %>%
                            unique() %>%
                            separate_rows(chemistry_elements,sep=" ") %>%
                            rename(element = chemistry_elements) %>%
                            left_join(element_redox_states) %>% 
                            left_join(electronegativity, by = "element") %>%
                            group_by(mineral_name) %>%
                            mutate(mean_pauling = mean(pauling),
                                   sd_pauling   = sd(pauling))

    if (elements_by_redox)
    {
        network_information %>%
            mutate(base_element = element, 
                   redox_sign = case_when(redox == 0 ~ "+",
                                          redox == abs(redox) ~ "+",
                                          redox != abs(redox) ~ "-"),
                   element = ifelse(is.na(redox), paste0(element, "  "), 
                                                  paste0(element, redox_sign, abs(redox)))) %>%
            unique() %>%
            ungroup() -> network_information       
    }
    
    ### STARTS multiple redox states, one per element per mineral, regardless of if/else above
    network_information %<>%
        select(element, redox) %>%
        unique() %>%
        group_by(element) %>%
        summarize(mean_element_redox = mean(redox, na.rm=TRUE)) %>%
        right_join(network_information) %>%
        mutate(redox = ifelse(is.nan(redox), NA, redox),
               mean_element_redox = ifelse(is.nan(mean_element_redox), NA, mean_element_redox))
    ### ENDS with two redox columns: redox is the element in the mineral (sometimes unknown), and mean_element_redox will be a unique per node. 

    network_information    
}
