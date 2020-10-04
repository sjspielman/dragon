library(tidyverse)
devtools::load_all() #library(dragon)
source("functions_calculate_weighted_pauling.R")
source("setup_calculate_weighted_pauling.R")
source("find_element_presence_conflicts.R")


# Which minerals should we calculate for? -----------------------------
element_presence_conflict %>%
  dplyr::filter(conflict == FALSE) %>%
  dplyr::pull(mineral_name) -> same_elements_ima_rruff

med_data_raw %>%
  dplyr::filter(mineral_name %in% same_elements_ima_rruff) -> med_data_raw

## Perform calculations using IMA --------------------------------------
med_data_raw %>%
  dplyr::mutate(chem = ima) %>%
  apply_manual_formula_changes() %>%
  count_elements(water_standin, hydroxy_standin, ree_standin) -> ima_counted_excluded
ima_counted_excluded$excluded %>% 
  dplyr::select(-chem) %>%
  dplyr::mutate(w_mean_pauling = NA, w_cov_pauling = NA) -> excluded_ima
calculate_weighted_values(ima_counted_excluded$counts, pauling_values) %>%
  dplyr::bind_rows(excluded_ima) %>%
  dplyr::arrange(mineral_name) -> ima_weighted_values




## Perform calculations using RRUFF --------------------------------------
med_data_raw %>%
  dplyr::mutate(chem = rruff) %>%
  apply_manual_formula_changes() %>%
  count_elements(water_standin, hydroxy_standin, ree_standin) -> rruff_counted_excluded
rruff_counted_excluded$excluded %>% 
  dplyr::select(-chem) %>%
  dplyr::mutate(w_mean_pauling = NA, w_cov_pauling = NA) -> excluded_rruff 

calculate_weighted_values(rruff_counted_excluded$counts, pauling_values) %>%
  dplyr::bind_rows(excluded_rruff) %>%
  dplyr::arrange(mineral_name) -> rruff_weighted_values 

## Compare: how many are in agreement between RRUFF/IMA calculation versions? --------------------------------------

rruff_ima_consistent <- dplyr::inner_join(ima_weighted_values, rruff_weighted_values)


# Exploration for extent of error/differences -------------------------------

# Save for records
#element_presence_conflict %>%
#  dplyr::filter(conflict == TRUE) %>%
#  dplyr::inner_join(med_data_cache) %>%
#  dplyr::select(mineral_name, rruff_chemistry, ima_chemistry) %>%
#  readr::write_csv("~/Desktop/rruff_ima_formula_contain_different_elements.csv")
  


#ima_weighted_values %>% 
#  dplyr::filter(!(mineral_name %in% rruff_ima_consistent$mineral_name )) %>% 
#  dplyr::pull(mineral_name)-> ima_not_consistent
#rruff_weighted_values %>% 
#  dplyr::filter(!(mineral_name %in% rruff_ima_consistent$mineral_name )) %>%
#  dplyr::pull(mineral_name)-> rruff_not_consistent
#
#diff_minerals <- unique(ima_not_consistent, rruff_not_consistent) # 107
#med_data_cache %>%
#  dplyr::filter(mineral_name %in% diff_minerals) %>%
#  dplyr::select(mineral_name, rruff_chemistry, ima_chemistry) %>%
#  readr::write_csv("~/Desktop/rruff_ima_formulas_different_element_counts.csv")

#ima_weighted_values %>%
#  dplyr::rename(w_mean_pauling_ima = w_mean_pauling, 
#                w_cov_pauling_ima = w_cov_pauling) -> ima_weighted_values2
#                
#rruff_weighted_values %>%
#  dplyr::rename(w_mean_pauling_rruff = w_mean_pauling, 
#                w_cov_pauling_rruff = w_cov_pauling) -> rruff_weighted_values2
#
#dplyr::left_join(ima_weighted_values2, rruff_weighted_values2) %>%
#  dplyr::mutate(mean_error = abs(w_mean_pauling_ima-w_mean_pauling_rruff)/w_mean_pauling_ima, 
#                cov_error = abs(w_cov_pauling_ima-w_cov_pauling_rruff)/w_cov_pauling_ima) %>%
#  dplyr::select(mineral_name, mean_error, cov_error) %>%
#  dplyr::filter(mineral_name %in% diff_minerals) %>%
#  dplyr::arrange(cov_error) %>%
#  dplyr::filter(mean_error <= 0.01, cov_error <= 0.01)
                


