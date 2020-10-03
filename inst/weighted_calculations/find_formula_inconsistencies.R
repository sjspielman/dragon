library(tidyverse)
# Determine if _present elements_ are compatible among formulas : element_presence_conflict T for conflict, F for no conflict
# goal:
## for incompatible minerals, option to exclude from network 
## the only need here is to preserve the fact that there may be conflicts

dragon:::med_data_cache %>%
  dplyr::select(rruff = rruff_chemistry, ima = ima_chemistry, mineral_name, chemistry_elements) %>%
  dplyr::distinct() %>%
  # Replace REE with Ee since REE is not regex atom name
  dplyr::mutate(rruff = str_replace_all(rruff, "REE", "Ee"),
                ima = str_replace_all(ima, "REE", "Ee"),
                chemistry_elements = str_replace_all(chemistry_elements, "REE", "Ee")) %>%
  group_by(mineral_name) -> input_med


identify_elements <- function(df, formula_column)
{
  
  df %>%
    dplyr::mutate(el= str_match_all({{formula_column}}, "[A-Z][a-z]*")) %>%
    tidyr::unnest(cols = c("el")) %>% 
    dplyr::mutate(el2 = paste(unique(el),collapse = " ")) %>%
    dplyr::select(-el)
}

sort_elements <- function(df, element_column)
{
  df %>%
    dplyr::mutate(split_form = stringr::str_split({{element_column}}, " ")) %>%  
    tidyr::unnest(cols = c("split_form")) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(split_form) %>%
    dplyr::mutate(output_column = paste(split_form, collapse = " ")) %>% 
    dplyr::select(-split_form, -{{element_column}}) %>%
    dplyr::distinct()
}

identify_elements(input_med, rruff) %>%
  sort_elements(el2) %>%
  dplyr::select(-chemistry_elements) %>%
  dplyr::rename(rruff_elements = output_column) -> rruff_elements

identify_elements(input_med, ima) %>%
  sort_elements(el2) %>%
  dplyr::select(-chemistry_elements) %>%
  dplyr::rename(ima_elements = output_column) -> ima_elements

sort_elements(input_med, chemistry_elements) %>%
  dplyr::rename(chemistry_elements = output_column) %>%
  dplyr::left_join(ima_elements) %>%
  dplyr::left_join(rruff_elements) %>%
  dplyr::ungroup() %>%
  dplyr::select(mineral_name, everything()) %>%
  dplyr::arrange(mineral_name) %>%
  dplyr::mutate(rruff_ima = rruff_elements == ima_elements,
                rruff_default = rruff_elements == chemistry_elements,
                ima_default = chemistry_elements == ima_elements) %>%
  dplyr::select(mineral_name, rruff_ima, rruff_default, ima_default) -> formula_element_overlap
# NAMES: 
# mineral_name
# rruff_ima     : do rruff and ima agree?
# rruff_default : do rruff and chemistry_elements agree?
# ima_default   : do ima and chemistry_elements agree?
# formula_element_overlap %>%
#   dplyr::filter(ima_default==FALSE)  ---> 0 rows

# formula_element_overlap %>%
#   dplyr::filter(rruff_default==FALSE)  ---> 100 rows for 2/3/20 med data

formula_element_overlap %>%
  dplyr::mutate(conflict = !(rruff_default)) %>%
  dplyr::select(mineral_name, conflict) -> element_presence_conflict
