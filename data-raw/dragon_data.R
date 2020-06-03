## code to prepare interal datasets goes here

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(usethis)
library(ggplot2)
library(RColorBrewer)
library(tibble)

## LAST UPDATED ON 6/3/20 ##

## Download MED tables-------------------------------------------
m1 <- readr::read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_mineral.csv", guess_max=10000)
m2 <- readr::read_tsv("http://rruff.info/mineral_list/MED/exporting/tbl_locality_age_cache_alt.csv", guess_max=10000)

left_join(m1, m2) %>% 
  dplyr::filter(at_locality == 1) %>%
  dplyr::select(mineral_name, 
                mineral_id, 
                mindat_id,    ### locality id  
                locality_longname, 
                age_type,
                rruff_chemistry, 
                ima_chemistry,
                min_age,
                max_age, 
                chemistry_elements) %>% 
  ## there are no NA's that I see, but hey
  tidyr::drop_na() %>%
  ## MY -> GY
  mutate(max_age = max_age/1000, min_age = min_age/1000) %>%
  ## ima_chemistry formula to HTML
  dplyr::rowwise() %>%
  dplyr::mutate(ima_chemistry = stringr::str_replace_all(ima_chemistry, 
                                                         "_(\\d+\\.*\\d*)_", 
                                                         "<sub>\\1</sub>")) %>%
  ungroup() -> rruff    


## Parse the redox ---------------------------------------------------
rruff %>% 
    dplyr::select(mineral_name, rruff_chemistry) %>% 
    dplyr::distinct() -> mineral_chem
    
element_redox_states_raw <- tibble("mineral_name" = as.character(), "element" = as.character(), "element_redox_mineral" = as.double(), "n" = as.integer())
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

    element_redox_states_raw <- bind_rows( element_redox_states_raw, temp2 )
}

## Merge with elements unknown redox state (NA) -----------------------------------
group1_elements <- c("H", "Li", "Na", "K", "Rb", "Cs", "Fr")
group2_elements <- c("Be", "Mg", "Ca", "Sr", "Ba", "Ra")
rruff %>%
  dplyr::select(mineral_name, chemistry_elements) %>%
  dplyr::distinct() %>%
  tidyr::separate_rows(chemistry_elements, sep = " ") %>%
  dplyr::rename(element = chemistry_elements) %>%
  dplyr::left_join(element_redox_states_raw) %>% 
  dplyr::select(-n) %>%
  dplyr::mutate(element_redox_mineral = case_when(element == "O"               ~ -2,
                                                  element == "Si"              ~ 4, 
                                                  element %in% c("Cl", "F")    ~ -1, 
                                                  element %in% group1_elements ~ 1,
                                                  element %in% group2_elements ~ 2,
                                                  TRUE                         ~ as.numeric(element_redox_mineral)
                                                 ) ## END case_when
                ) -> element_redox_states


element_redox_states %>% filter(element_redox_mineral == 0) -> num_zero_redox
if(nrow(num_zero_redox) != 0) stop("Redox states of 0 recovered.")

## WHAT DO?
element_info  <- readr::read_csv("/Users/spielman/Projects/dragon/inst/extdata/element_information.csv")
                   
                   
                   
                   
                   
## Prepare palettes for display in dropdown menus ----------------------------------------
first_part  <- "<img src='www/palette_png/"
middle_part  <- ".png' width=110px><div class='palette-style'>"
last_part <- "</div></img>"
RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) %>%
  dplyr::mutate(img = paste0(first_part, name, middle_part, name, last_part)) %>%
  dplyr::arrange(desc(category), name)-> palettes_of_interest

palettes_of_interest %>%
  dplyr::filter(category == "qual") -> qual_palettes_ui

palettes_of_interest %>%
  dplyr::filter(category != "qual") -> sd_palettes_ui

       
                   
                   
usethis::use_data(rruff,
                  element_redox_states,
                  element_info,
                  qual_palettes_ui,
                  sd_palettes_ui,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

