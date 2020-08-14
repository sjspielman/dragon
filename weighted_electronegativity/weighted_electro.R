library(tidyverse)
library(dragon)
source("weighted_electro_functions.R")
hydroxy_standin <- "Aa" # Placeholder for (OH) groups to be re-counted at the end


### Step 1: Get info from med_data_cache and replace where known from experts --------------------------------------------
dragon:::med_data_cache %>%
  dplyr::select(chem = rruff_chemistry, mineral_name) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(chem = case_when(mineral_name == "Ferrotochilinite" ~ "(Fe^2+^S^2-^)_6_(Fe^2+^(OH)_2_)_5_",
                                 mineral_name == "Cousinite"        ~  "MgU_2_(MoO_4_)_2_(OH)_6",  #  "MgU_2_(MoO_4_)_2_(OH)_6_(?)"   
                                 mineral_name == "Pigotite"         ~  "Al_4_C_6_H_5_O_10", # "Al_4_C_6_H_5_O_10_(?)" 
                                 TRUE ~ chem)) -> med_data
  

### Step 2: Remove all the bullshit, **and replace (OH)**  -----------------------------------------------------------------
med_data %>% 
  # Remove redox, [box], {, }, remove leading/trailing whitespace, and actually all spaces
  dplyr::mutate(chem = stringr::str_replace_all(chem, "\\^[ \\d\\+-]+\\^",""), #redox
                chem = stringr::str_replace_all(chem, "\\[box\\]", ""), # box
                chem = stringr::str_replace_all(chem, "\\{", "("), # { --> (
                chem = stringr::str_replace_all(chem, "\\}", ")"), # } --> )
                chem = stringr::str_replace_all(chem, "\\[", "("), # [ --> (
                chem = stringr::str_replace_all(chem, "\\]", ")"), # ) --> ]
                # strip ends
                chem = stringr::str_trim(chem, side = "both"),
                # remove all spaces
                chem = stringr::str_replace_all(chem, "\\s*", ""),
                ## !!! CHANGE ALL (OH) to hydroxy_standin !!!!! 
                chem = stringr::str_replace_all(chem, "\\(OH\\)", hydroxy_standin)) -> med_cleaned

### Step 3: Subset to rows that we are NOT dealing with until further instruction. (There are 691) -----------------------------------------
med_cleaned %>%
  dplyr::filter(stringr::str_detect(chem, "REE") |
                  stringr::str_detect(chem, "~") |
                  stringr::str_detect(chem, "≈") |
                  stringr::str_detect(chem, "[^A-Z]x") | 
                  stringr::str_detect(chem, "[^A-Z]n") |
                  stringr::str_detect(chem, ",") ) -> med_ignore_temp
                 # Types of comma weirdness that there could be, dealt with in line above:
                  # stringr::str_detect(chem, ",,") | 
                 # stringr::str_detect(chem, ",\\)") |
                 # stringr::str_detect(chem, "\\(,") |
                 # stringr::str_detect(chem, "\\w,[^\\w]") |
                 # stringr::str_detect(chem, ",[^\\w]") )  


### Step 4: Remove the `med_ignore_temp`, and clean the fractions/ranges to a single number -----------------------------------------
med_cleaned %>%
  # Remove ambiguous rows for later
  dplyr::anti_join(med_ignore_temp) %>%
  # Clean the ranges and fractions 
  dplyr::mutate(chem = purrr::map(chem, replace_number_ranges_fractions)) %>%
  tidyr::unnest(cols = "chem") -> med_cleaned_rangefrac


### Step 5: Extract and save the amount of complexed waters for each mineral into `mineral_water_counts` ------------------------------------------------------
med_cleaned_rangefrac %>% 
  dplyr::mutate(water_count = stringr::str_extract(chem, "·(\\d+\\.*\\d*)*\\(*H_2_O\\)*")) %>%
  tidyr::unnest(cols = "water_count") %>% 
  tidyr::replace_na(list(water_count = 0)) %>%
  dplyr::mutate(water_count = stringr::str_replace(water_count, "·", ""),
                water_count = stringr::str_replace(water_count, "\\(*H_2_O\\)*", ""),
                water_count = ifelse(water_count == "", 1, water_count)) %>%
  dplyr::mutate(water_count = as.numeric(water_count),
                O = water_count,
                H = water_count *2) %>%
  dplyr::select(-water_count, -chem) %>%
  tidyr::pivot_longer(O:H, names_to = "element", values_to = "count") -> mineral_water_counts

# Remove waters from mineral formulas and just chuck every other · (they are not meaningful)
med_cleaned_rangefrac %>%
  dplyr::mutate(chem = stringr::str_replace(chem, "·(\\d+\\.*\\d*)*\\(*H_2_O\\)*", ""),
                chem = stringr::str_replace(chem, "·", "")) -> med_cleaned_rangefrac_nowater
  

### Step 6: All parentheses get iteratively replaced with non-parentheses versions -----------------------------------------------
full_counts <- tibble::tibble(element = as.character(),
                              count   = as.numeric(),
                              mineral_name = as.character())

for (min_name in sort(med_cleaned_rangefrac_nowater$mineral_name)){
  
  print(min_name)
  med_cleaned_rangefrac_nowater %>%
    dplyr::filter(mineral_name == min_name) %>%
    dplyr::pull(chem) %>%
    parse_all_paren() %>%
    parse_clean_formula() %>%
    dplyr::mutate(mineral_name = min_name) -> raw_counts

  
  # Return the hydroxy counts and immediately plop in with the full counts
  raw_counts %>% 
    dplyr::filter(element == hydroxy_standin) %>%
    dplyr::pull(count) -> hydroxy_count_raw
  
  if (length(hydroxy_count_raw) > 0) {
    final_hydroxy_count <- sum(hydroxy_count_raw)
    hydroxy_tibble <- tibble::tribble(~element, ~count, ~mineral_name, 
                                      "O", final_hydroxy_count, min_name, 
                                      "H", final_hydroxy_count, min_name)
    raw_counts %>%
      dplyr::filter(element != hydroxy_standin) %>%
      dplyr::bind_rows(hydroxy_tibble) -> raw_counts
    
  } 
  
  ## Merge it up
  full_counts %>%
    dplyr::bind_rows(raw_counts) -> full_counts
  
  
}

full_counts %>%
  dplyr::bind_rows(mineral_water_counts) %>%
  dplyr::group_by(mineral_name, element) %>%
  dplyr::summarize(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(mineral_name, element) -> final_counts_possible

### Step 7: Re-merge data with the ones that aren't done (element count will be NA)
med_ignore_temp %>%
  dplyr::select(mineral_name) %>%
  dplyr::distinct() %>%
  dplyr::full_join(final_counts_possible) %>% 
  dplyr::arrange(mineral_name, element) %>%
  dplyr::left_join(med_data_cache) %>%
  dplyr::select(mineral_name, element, count, rruff_chemistry) %>% 
  dplyr::distinct() -> final_counts_possible_and_na

readr::write_csv(final_counts_possible_and_na, "weighted_electronegativity_8-14-20.csv")
  





























