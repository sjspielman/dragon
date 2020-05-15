## code to prepare interal datasets goes here

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(usethis)

## Download MED tables-------------------------------------------
m1 <- readr::read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_mineral.csv", guess_max=10000)
m2 <- readr::read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_locality_age_cache_alt.csv", guess_max=10000)

## 
left_join(m1, m2) %>% 
    dplyr::select(mineral_name, 
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
    dplyr::filter(at_locality == 1) %>%
    dplyr::select(-at_locality) -> rruff_raw

rruff_raw %>%
    dplyr::select(mineral_name, ima_chemistry) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ima_chemistry = stringr::str_replace_all(ima_chemistry, 
                                                           "_(\\d+\\.*\\d*)_", 
                                                           "<sub>\\1</sub>")) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(rruff_raw %>% 
                        dplyr::select(-ima_chemistry)
                     ) %>%
    mutate(max_age = max_age/1000, min_age = min_age/1000) -> rruff 
                    


## One row per element per mineral -----------------------------------
rruff %>%
    dplyr::select(mineral_name, chemistry_elements) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(chemistry_elements, sep = " ") -> rruff_separated 


## Parse the redox ---------------------------------------------------
rruff %>% 
    dplyr::select(mineral_name, rruff_chemistry) %>% 
    dplyr::distinct() -> mineral_chem
    
element_redox_states <- tibble("mineral_name" = as.character(), "element" = as.character(), "element_redox_mineral" = as.double(), "n" = as.integer())
for (mineral in mineral_chem$mineral_name)
{
    mineral_chem %>%
        dplyr::filter(mineral_name == mineral) -> mindat


    temp <- as_tibble(as.data.frame(stringr::str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
    if(nrow(temp) == 0)
    {
        temp2 <- tibble("mineral_name" = mineral, "element" = NA, "element_redox_mineral" = NA, "n" = 1)
    } else
    {
        temp$X3 <- as.double(temp$X3)
        temp %>% 
            dplyr::mutate(thesign = if_else(X4 == "+", 1, -1), 
                          redox   = X3 * thesign) %>% 
            dplyr::select(redox, X2) %>%
            dplyr::mutate(mineral_name = mineral, n=1:n()) -> temp2
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
rruff_separated %>% 
    dplyr::rename(element = chemistry_elements) %>% 
    dplyr::left_join(element_redox_states) %>% 
    dplyr::select(-n) %>%
    dplyr::mutate(element_redox_mineral = if_else(element == "O", -2, element_redox_mineral),   
                  element_redox_mineral = if_else(element %in% group1_elements, 1, element_redox_mineral),  
                  element_redox_mineral = if_else(element %in% group2_elements, 2, element_redox_mineral),
                  element_redox_mineral = if_else(element  == "Si", 4, element_redox_mineral),
                  element_redox_mineral = if_else(element == "Cl", -1, element_redox_mineral),   
                  element_redox_mineral = if_else(element == "F", -1, element_redox_mineral)) -> element_redox_states

## WHAT DO?
element_info  <- readr::read_csv("/Users/spielman/Projects/dragon/inst/extdata/element_information.csv")
geo_timeline  <- readr::read_csv("/Users/spielman/Projects/dragon/inst/extdata/geo_timeline.csv")
extinctions   <- readr::read_csv("/Users/spielman/Projects/dragon/inst/extdata/extinctions.csv")

                   
                   
usethis::use_data(rruff,
                  element_redox_states,
                  element_info,
                  geo_timeline,
                  extinctions,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

