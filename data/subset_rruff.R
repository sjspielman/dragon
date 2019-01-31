###  This script produces files used in app from original database information
library(tidyverse)

rruff <- read_csv("rruff_minerals.csv")

rruff %>%
    select(mineral_name, rruff_chemistry, chemistry_elements) %>%
    unique() %>%
    separate_rows(chemistry_elements, sep = " ") -> rruff_elements 
write_csv(rruff_elements, "rruff_separated_elements.csv")


rruff_elements %>% 
    select(mineral_name, rruff_chemistry) %>% 
    unique() -> mineral_chem
    
    
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

rruff_elements %>% 
    rename(element = chemistry_elements) %>% 
    left_join(element_redox_states) %>% 
    select(-n) %>%
    mutate(redox = if_else(element == "O", -2, redox),
           redox = if_else(element == "H", 1, redox)) %>%
    replace_na(list(redox = 0)) -> rruff_redox
write_csv(rruff_redox, "rruff_redox_states.csv")


    
    
    
    
    
    
    
    
    
    
