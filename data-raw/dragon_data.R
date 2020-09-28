## code to prepare internal datasets goes here

library(dragon)

## LAST UPDATED ON 8/29/20 WITH MED 2/3/20 ##

med_data_cache <- fetch_med_data()
element_redox_states_cache <- calculate_element_redox_states(med_data_cache)
med_cache_date <- find_most_recent_date()
     
source("calculate_weighed_pauling.R")  # produces `final_weighted_pauling`   

usethis::use_data(med_data_cache,
                  element_redox_states_cache,
                  med_cache_date,
                  final_weighted_pauling,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

