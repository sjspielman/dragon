rruff                <- read_csv("data/rruff_minerals.csv") %>% mutate(max_age = max_age/1000) 
element_redox_states <- read_csv("data/rruff_redox_states.csv")
rruff_separated      <- read_csv("data/rruff_separated_elements.csv")

all_elements  <- element_redox_states %>% select(element) %>% unique()
rruff_chemistry <- rruff_separated %>% select(-chemistry_elements) %>% unique()

initialize_data <- function(elements_of_interest, force_all_elements, age_limit)
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
            inner_join(rruff_sub) -> elements_only
    } else 
    { ## Has at least one element
        rruff_separated %>%
            group_by(mineral_name) %>%
            mutate(has_element = if_else( chemistry_elements %in% elements_of_interest, TRUE, FALSE)) %>% 
            filter(has_element == TRUE) %>%
            select(mineral_name) %>%
            inner_join(rruff_sub) -> elements_only
 
    }  
    
    if (nrow(elements_only) == 0) {
        showModal(modalDialog(
            title = "ERROR: There is no network that contains the selected elements as specified.",
            "You must refresh the web page.",
            easyClose = FALSE,
            footer = NULL,
            size = "l"

        ))
        Sys.sleep(3)
    }

    elements_only %<>% 
        group_by(mineral_name) %>% 
        summarize(num_localities = sum(at_locality)) %>%
        left_join(elements_only) %>%
        ungroup() %>% group_by(mineral_name) %>%
        mutate(overall_max_age = max(max_age)) %>%
        filter(max_age == overall_max_age) %>% 
        ungroup() %>%
        filter(max_age >= age_limit) %>%
        select(mineral_name, num_localities, max_age, chemistry_elements) %>%
        unique() %>%
        separate_rows(chemistry_elements,sep=" ") 

    elements_only %>%
        rename(element = chemistry_elements) %>%
        left_join(element_redox_states) %>% 
        group_by(mineral_name, element, max_age, num_localities) %>%
        summarize(redox = mean(redox)) %>%   ##  some minerals have a few states
        unique() %>%
        ungroup() -> network_information    
    
    network_information    
}


construct_network   <- function(network_information)
{

    network_data <- network_information %>% select(mineral_name, element)
        element_network <- graph.data.frame(network_data, directed=FALSE)
        V(element_network)$type <- bipartite_mapping(element_network)$type 
        clustered_net <- cluster_louvain(element_network)
        deg <- degree(element_network, mode="all")

        ### 1 row per VERTEX, to be joined with nodes
    minerals_as_item <- network_information %>% 
        group_by(mineral_name) %>%
        summarize(mean_redox = mean(redox)) %>%   
        left_join(network_information) %>%
        select(mineral_name, mean_redox, num_localities, max_age) %>%
        rename(item = mineral_name) %>%
        unique() 

    vertex_information <- left_join( tibble("item" = clustered_net$names, "cluster_ID"= as.numeric(clustered_net$membership)),
                                     tibble("item" = names(deg), "network_degree"= as.numeric(deg)) ) %>%
        mutate(type = ifelse(item %in% network_information$mineral_name, "mineral", "element")) %>%
        group_by(type) %>%
        mutate(network_degree_norm = network_degree / max(network_degree)) %>%
        ungroup() %>%              
        left_join(minerals_as_item) %>%
        rename(id = item, redox = mean_redox)

    net <- toVisNetworkData(element_network)

    edges <- as_tibble(net$edges) %>% bind_cols(network_information)

    nodes <- as_tibble(net$nodes) %>%
              mutate(type = ifelse(type == FALSE, "mineral", "element")) %>% 
              left_join(vertex_information)

    return (list("nodes" = nodes, "edges" = edges))
}
  
  