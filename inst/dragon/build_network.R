form_file_path <- function(filename)
{
    return( system.file("extdata", filename, package = "dragon") ) 
}

rruff                <- read_csv(form_file_path("rruff_minerals.csv.zip")) %>% mutate(max_age = max_age/1000) 
element_redox_states <- read_csv(form_file_path("rruff_redox_states.csv.zip"))
rruff_separated      <- read_csv(form_file_path("rruff_separated_elements.csv.zip"))
rruff_sub            <- rruff %>% select(-mineral_id, -mindat_id, -rruff_chemistry)
rruff_chemistry      <- rruff_separated %>% select(-chemistry_elements) %>% unique()
electronegativity    <- read_csv(form_file_path("element_electronegativities.csv.zip")) %>% select(-allen)


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
    elements_only
}

initialize_data_age <- function(elements_only, age_limit)
{
    elements_only %>% 
        group_by(mineral_name) %>% 
        summarize(num_localities = sum(at_locality)) %>%
        left_join(elements_only) %>%
        ungroup() %>% group_by(mineral_name) %>%
        mutate(overall_max_age = max(max_age)) %>%
        filter(max_age == overall_max_age) %>% 
        ungroup() %>%
        filter(max_age >= age_limit) -> elements_only_age
    
    elements_only_age
}

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


construct_network   <- function(network_information, elements_by_redox)
{

    network_data <- network_information %>% select(mineral_name, element)
    
    element_network <- graph.data.frame(network_data, directed=FALSE)
    V(element_network)$type <- bipartite_mapping(element_network)$type 
    clustered_net <- cluster_louvain(element_network)
    deg <- degree(element_network, mode="all")

    ### 1 row per VERTEX, to be joined with nodes
    minerals_as_item <- network_information %>% 
        group_by(mineral_name) %>%
        left_join(network_information) %>%
        select(mineral_name, num_localities, max_age, mean_pauling, sd_pauling) %>%
        rename(item = mineral_name) %>%
        unique() 
    
    ## TWO COLUMNS, ID IS NOW UNIQUE NODE
    network_information %>% 
        select(element, mean_element_redox, pauling) %>%
        rename(id = element, redox = mean_element_redox) %>%
        unique() -> element_redox_electro

    vertex_information <- left_join( tibble("item" = clustered_net$names, "cluster_ID"= as.numeric(clustered_net$membership)),
                                     tibble("item" = names(deg), "network_degree"= as.numeric(deg)) ) %>%
        mutate(type = ifelse(item %in% network_information$mineral_name, "mineral", "element")) %>%
        group_by(type) %>%
        mutate(network_degree_norm = network_degree / max(network_degree)) %>%
        ungroup() %>%        
        left_join(minerals_as_item) %>%  
        rename(id = item) %>%
        left_join(element_redox_electro, by="id")

    
    net <- toVisNetworkData(element_network)

    edges <- as_tibble(net$edges) %>% bind_cols(network_information) %>% left_join(minerals_as_item)

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
                       title = id,                      
                       font.face = "courier")
    return (list("nodes" = nodes, "edges" = edges))
}
  
  