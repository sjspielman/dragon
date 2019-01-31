rruff                <- read_csv("data/rruff_minerals.csv") %>% mutate(max_age = max_age/1000) 
rruff_sub            <- rruff %>% select(-mineral_id, -mindat_id, -rruff_chemistry)
element_redox_states <- read_csv("data/rruff_redox_states.csv")
rruff_separated      <- read_csv("data/rruff_separated_elements.csv")

rruff_chemistry <- rruff_separated %>% select(-chemistry_elements) %>% unique()

initialize_network <- function(elements_of_interest, force_all_elements, select_all_elements, age_limit){ 


    if(select_all_elements){
        elements_of_interest <- unique(rruff_redox_states$element)
    }
    if (is.null(elements_of_interest)) {
        showModal(modalDialog(
            title = "ERROR: No elements were selected.",
            "You must refresh the web page",
            easyClose = FALSE,
            footer = NULL,
            size = "l"
        ))
        Sys.sleep(3)
#         updateSelectInput(session, "elements_of_interest", "Ag")
#         elements_of_interest <- input$elements_of_interest 
    }
    
    network_info <- subset.rruff(elements_of_interest, force_all_elements, select_all_elements, age_limit)   
    thenetwork   <- build.network(network_info) 
    
    return (thenetwork)
}

  
  
subset.rruff <- function(elements_of_interest, force_all_elements, select_all_elements, age_limit)
{
    if (select_all_elements)
    {
        elements_only <- rruff_sub
    } else 
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
 #        updateCheckboxInput(session, "force_all_elments", FALSE)
#         updateSelectInput(session, "elements_of_interest", "Ag")
#         force_all_elments <- input$force_all_elments
#         elements_of_interest <- input$elements_of_interest
#         
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
    
    if (nrow(elements_only) == 0) {
        showModal(modalDialog(
            title = "ERROR: There is no network that contains the selected element at the eon specified.",
            "You must refresh the web page.",
            easyClose = FALSE,
            footer = NULL,
            size = "l"
        ))
        Sys.sleep(3)
#         updateCheckboxInput(session, "select_all_elments", FALSE)
#         updateCheckboxInput(session, "force_all_elments", FALSE)
#         updateSelectInput(session, "include_age", "present")
#         updateSelectInput(session, "elements_of_interest", "Ag")
    }
    
    
    ### 1 row per EDGE, to be joined with edges
    mineral.element.information <- elements_only %>%
        rename(element = chemistry_elements) %>%
        left_join(element_redox_states) %>% 
        replace_na(list(redox = 0)) %>% 
        select(-n) %>%
        group_by(mineral_name, element, max_age, num_localities) %>%
        summarize(redox = mean(redox)) %>%   ##  some minerals have a few states
        unique() %>%
        ungroup()
    
    return(mineral.element.information)

}

build.network <- function(mineral.element.information)
{
    
  ## Build the network here so can obtain information for cluster, degree (save in separate tibble since both minerals and elements need a row)
  network.data <- mineral.element.information %>% select(mineral_name, element)
  element.network <- graph.data.frame(network.data, directed=FALSE)
  V(element.network)$type <- bipartite_mapping(element.network)$type 
  clustered.net <- cluster_louvain(element.network)
  deg <- degree(element.network, mode="all")

  ### 1 row per VERTEX, to be joined with nodes
  minerals.as.item <- mineral.element.information %>% 
    group_by(mineral_name) %>%
    summarize(mean_redox = mean(redox)) %>%   
    left_join(mineral.element.information) %>%
    select(mineral_name, mean_redox, num_localities, max_age) %>%
    rename(item = mineral_name) %>%
    unique() 
  
   vertex.information <- left_join( tibble("item" = clustered.net$names, "cluster_ID"= as.numeric(clustered.net$membership)),
                                   tibble("item" = names(deg), "network_degree"= as.numeric(deg)) ) %>%
    mutate(type = ifelse(item %in% mineral.element.information$mineral_name, "mineral", "element")) %>%
    group_by(type) %>%
    mutate(network_degree_norm = network_degree / max(network_degree)) %>%
    ungroup() %>%              
    left_join(minerals.as.item) %>%
    rename(id = item, redox = mean_redox)

  net <- toVisNetworkData(element.network)
  
  edges <- as_tibble(net$edges) %>% 
    bind_cols(mineral.element.information)
  
  nodes <- as_tibble(net$nodes) %>%
    mutate(type = ifelse(type == FALSE, "mineral", "element")) %>% 
    left_join(vertex.information)
  
  
  return (list("nodes" = nodes, "edges" = edges))
}