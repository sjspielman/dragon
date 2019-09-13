form_file_path <- function(filename)
{
    #return(paste0("~/Projects/dragon/inst/extdata/", filename))
    return( system.file("extdata", filename, package = "dragon") ) 
}

locality1            <- read_csv(form_file_path("rruff_locality1.csv.zip")) ## mineral_name,mindat_id,age_type,min_age,max_age
locality2            <- read_csv(form_file_path("rruff_locality2.csv.zip")) ## mineral_name,mindat_id,age_type,min_age,max_age
locality_long        <- read_csv(form_file_path("locality_longnames.csv.zip")) ## mindat_id, locality_longname
rruff_raw            <- read_csv(form_file_path("rruff_chemistry.csv")) 
rruff_separated      <- read_csv(form_file_path("rruff_separated_elements.csv"))

             
element_redox_states <- read_csv(form_file_path("rruff_redox_states.csv"))
element_info         <- read_csv(form_file_path("element_information.csv")) 
geo_timeline         <- read_csv(form_file_path("geo_timeline.csv"))
extinctions          <- read_csv(form_file_path("extinctions.csv"))

hsab_levels <- c("Hard acid", "Int. acid", "Soft acid", "Soft base", "Int. base", "Hard base")

locality <- bind_rows(locality1, locality2) %>% distinct()
rruff    <- left_join(rruff_raw, locality) %>%
             left_join(locality_long) %>%
             left_join(rruff_separated) %>%
             mutate(max_age = max_age/1000, min_age = min_age/1000) 
total_max_age <- round( max(rruff$max_age) + 0.1, 1)

remove(locality1, locality2, rruff_raw)
                            
community_detect_network <- function(network, cluster_algorithm)
{
    if (cluster_algorithm == "Louvain")               return (cluster_louvain(network))
    if (cluster_algorithm == "Leading eigenvector")   return (cluster_leading_eigen(network))    
}


initialize_data <- function(elements_of_interest, force_all_elements)
{ 

    ## Must have all elements
    if (force_all_elements)
    {
        n_elements <- length(elements_of_interest)
        rruff_separated %>%
            group_by(mineral_name) %>%
            mutate(has_element = if_else( sum(chemistry_elements %in% elements_of_interest) == n_elements, TRUE, FALSE)) %>% 
            filter(has_element == TRUE) %>%
            select(mineral_name) %>%
            inner_join(rruff) -> elements_only
    } else 
    { ## Has at least one element
        rruff_separated %>%
            group_by(mineral_name) %>%
            mutate(has_element = if_else( chemistry_elements %in% elements_of_interest, TRUE, FALSE)) %>% 
            filter(has_element == TRUE) %>%
            dplyr::select(mineral_name) %>%
            inner_join(rruff) %>%
            dplyr::select(-chemistry_elements) -> elements_only
 
    }  
    elements_only
}

initialize_data_age <- function(elements_only, age_limit, max_age_type)
{
    lb <- age_limit[1]
    ub <- age_limit[2]
    
    if (max_age_type == "Minimum")
    {
        elements_only %<>% dplyr::mutate(age_check = min_age) 
    } else {
        elements_only %<>% dplyr::mutate(age_check = max_age) 
    }
    
    elements_only %<>% 
        filter(age_check >= lb, age_check <= ub) %>% 
        group_by(mineral_name) %>%
        mutate(num_localities_mineral = n()) %>%
        ungroup() 
    elements_only %>% 
        dplyr::select(mineral_name, mineral_id, max_age, min_age, mindat_id, locality_longname, age_type) %>%
        rename(max_age_locality = max_age) %>%
        rename(min_age_locality = min_age) %>%
        ungroup() -> locality_info
    elements_only %>%
        group_by(mineral_name) %>%
        summarize(overall_max = max(max_age)) %>% 
        rename(max_age = overall_max) %>%
        left_join(elements_only %>% dplyr::select(-min_age, -max_age)) %>%
        left_join(rruff_separated) %>%
        dplyr::select(mineral_name, mineral_id, ima_chemistry, rruff_chemistry, chemistry_elements, num_localities_mineral, max_age) %>%
        ungroup() %>% 
        distinct() -> elements_only_age
#     > names(elements_only_age)
#     [1] "mineral_name"           "mineral_id"             "ima_chemistry"         
#     [4] "rruff_chemistry"        "chemistry_elements"     "num_localities_mineral"
#     [7] "max_age"  

    list("elements_only_age" = elements_only_age, "locality_info" = locality_info)

}

obtain_network_information <- function(elements_only_age, elements_by_redox)
{      

    chem <- rruff %>% 
            dplyr::select(mineral_name, ima_chemistry, rruff_chemistry) %>%
            distinct()
    network_information <- elements_only_age %>%    
                            dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, chemistry_elements) %>%
                            distinct() %>%
                            separate_rows(chemistry_elements,sep=" ") %>%
                            rename(element = chemistry_elements) %>%
                            left_join(element_info, by = "element") %>%
                            left_join(element_redox_states, by = c("element", "mineral_name")) %>%
                            left_join(chem, by = "mineral_name") %>%
                            group_by(mineral_name) %>%
                            mutate(mean_pauling = mean(pauling),
                                   #sd_pauling   = sd(pauling),
                                   cov_pauling = sd(pauling) / mean_pauling ) %>%
                            ungroup()

    if (elements_by_redox)
    {
        network_information %<>%
            mutate(base_element = element, 
                   element_redox_mineral_sign = case_when(element_redox_mineral == 0 ~ "+",
                                                          element_redox_mineral == abs(element_redox_mineral) ~ "+",
                                                          element_redox_mineral != abs(element_redox_mineral) ~ "-"),
                   element = ifelse(is.na(element_redox_mineral), paste0(element, "  "), 
                                                  paste0(element, element_redox_mineral_sign, abs(element_redox_mineral)))) %>%
            dplyr::select(-element_redox_mineral_sign) %>%
            distinct() %>%
            ungroup()    
    }

    #network_information %<>%
    #    group_by(element) %>%
    #    mutate(num_localities_element = sum(num_localities_mineral)) %>%
    #    ungroup()

    network_information %<>%
        #dplyr::select(element, element_redox_mineral) %>%
        #distinct() %>%
        group_by(element) %>%
        mutate(element_redox_network = mean(element_redox_mineral, na.rm=TRUE),#### TODO: KEEP OR CHUCK NA.RM???
               num_localities_element = sum(num_localities_mineral)) %>% 
        ungroup() %>%
        group_by(element, mineral_name) %>%
        mutate(element_redox_mineral = mean(element_redox_mineral, na.rm=TRUE), #### TODO: KEEP OR CHUCK NA.RM???
               element_redox_mineral = ifelse(is.nan(element_redox_mineral), NA, element_redox_mineral),
               element_redox_network = ifelse(is.nan(element_redox_network), NA, element_redox_network)) %>%
        ungroup() %>%
        distinct()
#     > names(network_information)
#      [1] "mineral_name"           "mineral_id"             "num_localities_mineral"
#      [4] "max_age"                "element"                "element_hsab"          
#      [7] "AtomicMass"             "NumberofProtons"        "TablePeriod"           
#     [10] "TableGroup"             "AtomicRadius"           "pauling"               
#     [13] "MetalType"              "Density"                "SpecificHeat"          
#     [16] "element_name"           "element_redox_mineral"  "ima_chemistry"         
#     [19] "mean_pauling"           "cov_pauling"            "element_redox_network" 
#     [22] "num_localities_element"

    network_information    
}


construct_network   <- function(network_information, elements_by_redox)
{

    network_data <- network_information %>% 
                        dplyr::select(mineral_name, element) %>%
                        distinct()
    
    element_network <- graph.data.frame(network_data, directed=FALSE)
    V(element_network)$type <- bipartite_mapping(element_network)$type 
    #clustered_net <- community_detect_network(element_network, cluster_algorithm)
    deg <- igraph::degree(element_network)
    closeness <- igraph::closeness(element_network)


    ### 1 row per VERTEX, to be joined with nodes
    mineral_information <- network_information %>% 
        dplyr::select(mineral_name, mineral_id, num_localities_mineral, max_age, mean_pauling, cov_pauling, rruff_chemistry, ima_chemistry) %>% #sd_pauling,
        rename(item = mineral_name) %>%
        distinct() 

    
    ## TWO COLUMNS, ID IS NOW UNIQUE NODE
    element_information <- network_information %>% 
        dplyr::select(-mineral_name, -mineral_id, -num_localities_mineral, -max_age, -mean_pauling, -cov_pauling, -rruff_chemistry, -ima_chemistry, -element_redox_mineral) %>% 
        rename(id = element) %>%
        distinct()
    element_information$element_hsab <- factor(element_information$element_hsab, levels = hsab_levels)
    element_information$TablePeriod <- factor(element_information$TablePeriod)
    element_information$TableGroup <- factor(element_information$TableGroup)

    #vertex_information <- left_join( tibble("item" = clustered_net$names, "cluster_ID"= as.numeric(clustered_net$membership)),
    #                                 tibble("item" = names(deg), "network_degree"= as.numeric(deg), "closeness" = as.numeric(closeness)) ) %>%

    vertex_information <- tibble("item" = names(deg), "network_degree"= as.numeric(deg), "closeness" = as.numeric(closeness)) %>%
        mutate(type = ifelse(item %in% network_information$mineral_name, "mineral", "element")) %>%
        group_by(type) %>%
        mutate(network_degree_norm = network_degree / max(network_degree)) %>%
        ungroup() %>%        
        left_join(mineral_information) %>%  
        rename(id = item) %>% ##???
        left_join(element_information) %>%
        mutate(num_localities = ifelse(is.na(num_localities_mineral), num_localities_element, num_localities_mineral)) %>%
        dplyr::select(-num_localities_mineral, -num_localities_element)
    
    net <- toVisNetworkData(element_network)

    #write_csv(as_tibble(net$edges), "edges.csv")
    #write_csv(network_information, "info.csv")
    #print.data.frame(head(as_tibble(net$edges)))
   # print.data.frame(head(network_information))
    
    edges <- as_tibble(net$edges) %>%
                bind_cols(network_information) %>% 
                left_join(mineral_information) %>% 
                dplyr::select(-item) %>%
                distinct()
    

    charadd <- 0
    if (elements_by_redox){
        charadd <- 2
    }
    
    nodes <- as_tibble(net$nodes) %>%
                mutate(type = ifelse(type == FALSE, "mineral", "element")) %>% 
                left_join(vertex_information) %>%
                ungroup() %>%
                rename(group = type) %>% 
                mutate(label = case_when(group == "mineral"                     ~ label,
                                         group == "element" & nchar(label) == 1+charadd ~ paste0(" ", label, " "),
                                         group == "element" & nchar(label) == 2+charadd ~ paste0(" ", label),
                                         group == "element" & nchar(label) == 3+charadd ~ label),  
                       title = case_when(group == "mineral" ~ paste0("<p>", 
                                                                     id, "<br>", 
                                                                     ima_chemistry, "<br>", 
                                                                     paste0("Maximum known age: ", max_age, " Ga"), "</p>"), #!!!!!!!!! HELP
                                         group == "element" & elements_by_redox == TRUE ~ "",  
                                         group == "element" & elements_by_redox == FALSE ~ paste0("<p>", 
                                                                                                  element_name, "<br>", 
                                                                                                  ifelse(is.na(AtomicMass), "", paste0("Atomic mass: ", AtomicMass)), "<br>",  
                                                                                                  ifelse(is.na(pauling), "", paste0("Electronegativity: ", pauling)), "</p>")),                   
                       font.face = "courier") %>%
                distinct() 
    
    if (elements_by_redox)
    {
        nodes %<>% 
            mutate(title = ifelse(group == "mineral", title, paste0("<p>", 
                                                                     element_name, "<br>",
                                                                     ifelse(is.na(element_redox_network), "", paste0("Redox state: ", element_redox_network)), "<br>",    
                                                                     ifelse(is.na(AtomicMass), "", paste0("Atomic mass: ", AtomicMass)), "<br>",  
                                                                     ifelse(is.na(pauling), "", paste0("Electronegativity: ", pauling)), "</p>")))
    }
        
    return (list("nodes" = nodes, "edges" = edges, "graph" = element_network))
}
  
  