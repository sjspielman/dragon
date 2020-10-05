## code to prepare internal datasets goes here. 
## takes some time to (5-15 min), mostly due to querying/downloading from MED, but also due to mildly inefficient code in my weighted calculations that I do not care to clean. It's like a built-in coffee break!

#library(dragon)
devtools::load_all()

## LAST UPDATED ON 10/5/20 WITH MED 2/3/20 ##

med_data_cache <- fetch_med_data()
element_redox_states_cache <- calculate_element_redox_states(med_data_cache)
med_cache_date <- find_most_recent_date()
     
source("calculate_weighted_pauling.R")  # produces `final_weighted_pauling`, `exclude_minerals_from_dragon`

# # Remove minerals with rruff/ima inconsistency, and incorporate weighted calculations
# med_data_cache %>%
#   dplyr::filter(!(mineral_name %in% exclude_minerals_from_dragon)) %>%
#   dplyr::left_join(final_weighted_pauling) -> med_data_cache
# 
# # Remove minerals with rruff/ima inconsistency
# element_redox_states_cache %>%
#   dplyr::filter(!(mineral_name %in% exclude_minerals_from_dragon)) -> element_redox_states_cache


usethis::use_data(med_data_cache,
                  element_redox_states_cache,
                  med_cache_date,
                  exclude_minerals_from_dragon,
                  final_weighted_pauling,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

