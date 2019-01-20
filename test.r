library(shiny)
library(colourpicker)
library(tidyverse)
library(cowplot)
library(igraph)
library(visNetwork)


get_redox_

rruff <- read_csv("data/rruff_minerals.csv")
element_of_interest = "V"
# Subset database to element_of_interest with age and locality specifications applied
element_only <- rruff %>% 
                    mutate(has_element = if_else(str_detect(rruff_chemistry, element_of_interest), TRUE, FALSE)) %>% 
                    filter(has_element == TRUE) %>% 
                    select(-has_element, -mineral_id, -mindat_id)

element_only <- element_only %>%
                    group_by(mineral_name) %>% 
                    summarize(num_localities = sum(at_locality)) %>%
                    left_join(element_only) %>%
                    ungroup() %>% group_by(mineral_name) %>%
                    mutate(overall_max_age = max(max_age)) %>%
                    filter(max_age == overall_max_age) %>% 
                    ungroup() %>%
                    filter(max_age >= 3000) %>%
                    select(mineral_name, num_localities, max_age, rruff_chemistry, chemistry_elements) %>%
                    unique() %>%
                    separate_rows(chemistry_elements,sep=" ") 


####### Get element redox states
element_only %>% select(mineral_name, rruff_chemistry) %>% unique() -> mineral_chem
element_redox_states <- tibble("mineral_name" = as.character(), "element" = as.character(), "redox" = as.double(), "n" = as.integer())
for (mineral in mineral_chem$mineral_name)
{
    mineral_chem %>%
        filter(mineral_name == mineral) -> mindat


    temp <- as.tibble(as.data.frame(str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
    if(nrow(temp) == 0)
    {
        temp2 <- tibble("mineral_name" = mineral, "element" = NA, "redox" = NA, "n" = 1)
    } else
    {
        temp$X3 <- as.double(temp$X3)
        temp %>% mutate(thesign = if_else(X4 == "+", 1, -1), 
                        redox   = X3 * thesign) %>% 
                        select(redox, X2) %>%
                        mutate(mineral_name = mineral, n=1:n()) -> temp2
        temp2 <- temp2[c(3, 2, 1, 4)]
        names(temp2) <- c("mineral_name", "element", "redox", "n")
    }

    element_redox_states <- bind_rows( element_redox_states, temp2 )
}
### 1 row per EDGE
mineral.element.information <- element_only %>% 
                                rename(element = chemistry_elements) %>%
                                left_join(element_redox_states) %>% 
                                replace_na(list(redox = 0)) %>% 
                                group_by(mineral_name, element, max_age, num_localities) %>%
                                summarize(redox = mean(redox)) %>%   ##  some minerals have a few states
                                unique() %>%
                                ungroup()

## Build the network here so can obtain information for cluster, degree (save in separate tibble since both minerals and elements need a row)
network.data <- mineral.element.information %>% select(mineral_name, element)
element.network <- graph.data.frame(network.data, directed=FALSE)
V(element.network)$type <- bipartite_mapping(element.network)$type 
clustered.net <- cluster_louvain(element.network)
deg <- degree(element.network, mode="all")

### 1 row per VERTEX
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
                                 rename(id = item)
                                 
                                 
net <- toVisNetworkData(element.network)

edges <- as.tibble(net$edges) %>% 
            bind_cols(mineral.element.information)

nodes <- as.tibble(net$nodes) %>%
            mutate(type = ifelse(type == FALSE, "mineral", "element")) %>% 
            left_join(vertex.information)
            





