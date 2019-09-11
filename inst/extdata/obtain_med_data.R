### This script produces files used in dragon from original MED database information ###

library(tidyverse)


m1 <- read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_mineral.csv", guess_max=10000)
m2 <- read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_locality_age_cache_alt.csv", guess_max=10000)

left_join(m1, m2) %>% 
   select(mineral_name, 
          mineral_id, 
          mindat_id,    ### locality id  
          at_locality, 
          locality_longname, 
          age_type,
          rruff_chemistry, 
          ima_chemistry,
          min_age,
          max_age, 
          chemistry_elements) %>% 
   na.omit() %>%
   filter(at_locality == 1) %>%
   dplyr::select(-at_locality) -> rruff

rruff %>% 
    dplyr::select(mineral_name, ima_chemistry) %>%
    distinct() %>%
    rowwise() %>%
    mutate(ima_chemistry = str_replace_all(ima_chemistry, "_(\\d+\\.*\\d*)_", "<sub>\\1</sub>")) %>%
    ungroup() %>%
    left_join(rruff %>% dplyr::select(-ima_chemistry)) -> rruff 

rruff %>%
    dplyr::select(mineral_name, mineral_id, ima_chemistry, rruff_chemistry) %>%
    distinct() -> rruff_chemistry
write_csv(rruff_chemistry, "rruff_chemistry.csv")


rruff %>%
    dplyr::select(mineral_name, mindat_id, age_type, min_age, max_age) %>%
    distinct() -> rruff_locality

total <- nrow(rruff_locality)
if ((total %% 2) == 1) total <- total + 1

rruff_locality %>%
    head(total/2) %>%
    write_csv("rruff_locality1.csv")

rruff_locality %>%
    tail(total/2) %>%
    write_csv("rruff_locality2.csv")


system("zip rruff_locality1.csv.zip rruff_locality1.csv")
system("rm rruff_locality1.csv")

system("zip rruff_locality2.csv.zip rruff_locality2.csv")
system("rm rruff_locality2.csv")


rruff %>%
    dplyr::select(mindat_id, locality_longname) %>%
    distinct() %>%
    write_csv("locality_longnames.csv")

system("zip locality_longnames.csv.zip locality_longnames.csv")
system("rm locality_longnames.csv")

rruff %>%
#    select(mineral_name, rruff_chemistry, ima_chemistry, chemistry_elements) %>%
    select(mineral_name, chemistry_elements) %>%
    distinct() %>%
    separate_rows(chemistry_elements, sep = " ") -> rruff_elements 
write_csv(rruff_elements, "rruff_separated_elements.csv")


rruff %>% 
    select(mineral_name, rruff_chemistry) %>% 
    distinct() -> mineral_chem
    
    
element_redox_states <- tibble("mineral_name" = as.character(), "element" = as.character(), "element_redox_mineral" = as.double(), "n" = as.integer())
for (mineral in mineral_chem$mineral_name)
{
    mineral_chem %>%
        filter(mineral_name == mineral) -> mindat


    temp <- as_tibble(as.data.frame(str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
    if(nrow(temp) == 0)
    {
        temp2 <- tibble("mineral_name" = mineral, "element" = NA, "element_redox_mineral" = NA, "n" = 1)
    } else
    {
        temp$X3 <- as.double(temp$X3)
        temp %>% mutate(thesign = if_else(X4 == "+", 1, -1), 
                        redox   = X3 * thesign) %>% 
                        select(redox, X2) %>%
                        mutate(mineral_name = mineral, n=1:n()) -> temp2
        temp2 <- temp2[c(3, 2, 1, 4)]
        names(temp2) <- c("mineral_name", "element", "element_redox_mineral", "n")
    }

    element_redox_states <- bind_rows( element_redox_states, temp2 )
}

# a.            Group 1 metals: always +1
# b.            Group 2 metals: always +2
# c.            Oxygen: always -2
# d.            Hydrogen: always +1
# e.            Flourine: always -1
# f.            Chlorine: always -1
# g.            Silicon: always +4
group1_elements <- c("H", "Li", "Na", "K", "Rb", "Cs", "Fr")
group2_elements <- c("Be", "Mg", "Ca", "Sr", "Ba", "Ra")
rruff_elements %>% 
    rename(element = chemistry_elements) %>% 
    left_join(element_redox_states) %>% 
    select(-n) %>%
    mutate(element_redox_mineral = if_else(element == "O", -2, element_redox_mineral),   
           element_redox_mineral = if_else(element %in% group1_elements, 1, element_redox_mineral),  
           element_redox_mineral = if_else(element %in% group2_elements, 2, element_redox_mineral),
           element_redox_mineral = if_else(element  == "Si", 4, element_redox_mineral),
           element_redox_mineral = if_else(element == "Cl", -1, element_redox_mineral),   
           element_redox_mineral = if_else(element == "F", -1, element_redox_mineral)) -> rruff_redox
write_csv(rruff_redox, "rruff_redox_states.csv")




    
    
    
    
    
    
    
    
    
