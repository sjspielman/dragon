library(tidyverse)
# CONCLUSION:
# IMA is mindat 
# chemistry_elements is IMA and therefore reliable
# rruff chemistries are NOT reliable for tallying elements. 
# We must rely on ima formulas for counting number of elements


### Absolutely the gnarliest code which is in dire, dire need of functions. but it works yo.
dragon:::med_data_cache %>%
  dplyr::select(rruff = rruff_chemistry, ima = ima_chemistry, mineral_name, chemistry_elements) %>%
  dplyr::distinct() -> med_data_raw
med_data_raw %>%
  select(mineral_name, rruff, ima, chemistry_elements) %>%
  # Replace REE with Ee since REE is not regex atom name
  mutate(rruff = str_replace_all(rruff, "REE", "Ee"),
         ima = str_replace_all(ima, "REE", "Ee"),
         chemistry_elements = str_replace_all(chemistry_elements, "REE", "Ee")) %>%
  distinct() %>%
  group_by(mineral_name) %>%
  # Create new sorted chemistry_elements
  mutate(split_form = str_split(chemistry_elements, " ")) %>%  
  unnest(cols = c("split_form"))  %>% distinct() %>%
  arrange(split_form) %>% 
  mutate(chemistry_elements = paste(split_form, collapse = " ")) %>% 
  distinct() %>% 
  # Extract and create sorted rruff_chemistry elements
  mutate(rruff_el= str_match_all(rruff, "[A-Z][a-z]*")) %>%
  unnest(cols = c("rruff_el")) %>% 
  mutate(rruff_el2 = paste(unique(rruff_el),collapse = " ")) %>%
  select(-rruff_el) %>%
  mutate(split_form = str_split(rruff_el2, " ")) %>%  
  unnest(cols = c("split_form"))  %>% distinct() %>%
  arrange(split_form) %>% 
  mutate(rruff_elements = paste(split_form, collapse = " ")) %>% 
  distinct() %>% 
  select(-split_form) %>%
  # Extract and create sorted ima_chemistry elements
  mutate(ima_el= str_match_all(ima, "[A-Z][a-z]*")) %>%
  unnest(cols = c("ima_el")) %>% 
  mutate(ima_el2 = paste(unique(ima_el),collapse = " ")) %>%
  select(-ima_el) %>%
  mutate(split_form = str_split(ima_el2, " ")) %>%  
  unnest(cols = c("split_form"))  %>% distinct() %>%
  arrange(split_form) %>% 
  mutate(ima_elements = paste(split_form, collapse = " ")) %>% 
  # Clean up and ungroup (mineral_name grouping)
  select(-split_form, -rruff_el2, -ima_el2) %>%
  distinct() %>%
  ungroup() -> present_elements

Pb_9_Ca_6_(Si_2_O_7_)_4_(SiO_4_)O Pb^2+^_3_Ca_2_(SiO_4_)(Si_2_O_7_)

# ima elements always agree with the chemistry_elements
# rruff elements do not always agree: 100 different elements. So which is right?
present_elements %>%
  arrange(mineral_name) %>%
  mutate(rruff_ima = rruff_elements == ima_elements,
         rruff_default = rruff_elements == chemistry_elements,
         ima_default = chemistry_elements == ima_elements,
         all_same = ((rruff_elements == ima_elements) & (rruff_elements == chemistry_elements))) %>%
  filter(all_same == FALSE) -> disagree 


eli %>% 
  select(mineral_name, mindat = Mindat) %>% 
  right_join(disagree) %>% 
  select(mineral_name, mindat, rruff, ima) %>%
  mutate(mindat = str_replace_all(mindat, "REE", "Ee"), 
         ima = str_replace_all(ima, "<sub>", "_"), 
         ima = str_replace_all(ima, "<sup>", "^"), 
         ima = str_replace_all(ima, "</sub>", "_"),
         ima = str_replace_all(ima, "</sup>", "^")) %>% 
  filter(mindat != ima) # no rows!
  
  
anti_join(final_counts_possible_ima, final_counts_possible_rruff) %>%
  filter(!(mineral_name %in% disagree$mineral_name)) %>% View()
# 
disagree_counts 
  
final_counts_possible_ima
final_counts_possible_rruff
