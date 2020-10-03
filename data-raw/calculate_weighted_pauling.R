library(tidyverse)
#library(dragon)
source("functions_calculate_weighted_pauling.R")
source("setup_calculate_weighted_pauling.R")

element_presence_conflict %>%
  dplyr::filter(conflict == FALSE) %>%
  dplyr::pull(mineral_name) -> same_elements_ima_rruff

med_data_raw %>%
  dplyr::filter(mineral_name %in% same_elements_ima_rruff) -> med_data_raw

## Calculate with RRUFF
med_data_raw %>%
  dplyr::mutate(chem = rruff) %>%
  apply_manual_formula_changes() %>%
  count_elements(water_standin, hydroxy_standin, ree_standin) -> rruff_counted_excluded

calculate_weighted_values(rruff_counted_ignored$counts, pauling_values) -> rruff_weighted_values # 4666 minerals
excluded_rruff <- rruff_counted_excluded$excluded %>% dplyr::rename(rruff = chem) # 116 minerals


## Calculate with IMA
med_data_raw %>%
  dplyr::mutate(chem = ima) %>%
  apply_manual_formula_changes() %>%
  count_elements(water_standin, hydroxy_standin, ree_standin) -> ima_counted_excluded

calculate_weighted_values(ima_counted_excluded$counts, pauling_values) -> ima_weighted_values # 4560 minerals
excluded_ima <- ima_counted_excluded$excluded %>% dplyr::rename(ima = chem) # 126


# 4467 minerals
rruff_ima_consistent <- dplyr::semi_join(ima_weighted_values, rruff_weighted_values)

ima_weighted_values %>%
  dplyr::rename(w_mean_pauling_ima = w_mean_pauling, 
                w_cov_pauling_ima = w_cov_pauling) -> ima_weighted_values2
                
rruff_weighted_values %>%
  dplyr::rename(w_mean_pauling_rruff = w_mean_pauling, 
                w_cov_pauling_rruff = w_cov_pauling) -> rruff_weighted_values2

dplyr::left_join(ima_weighted_values2, rruff_weighted_values2) %>%
  dplyr::mutate(mean_error = abs(w_mean_pauling_ima-w_mean_pauling_rruff)/w_mean_pauling_ima, 
                cov_error = abs(w_cov_pauling_ima-w_cov_pauling_rruff)/w_cov_pauling_ima) %>%
  dplyr::select(mineral_name, mean_error, cov_error) %>%
  dplyr::filter(cov_error != 0 | mean_error != 0) %>% 
  dplyr::arrange(desc(cov_error)) 
                


