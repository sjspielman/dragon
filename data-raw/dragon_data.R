## code to prepare internal datasets goes here

library(dragon)

## LAST UPDATED ON 6/14/20 WITH MED 2/3/20 ##

med_data_cache <- fetch_med_data()
element_redox_states_cache <- calculate_element_redox_states(med_data_cache)
med_cache_date <- find_most_recent_date()
     
usethis::use_data(med_data_cache,
                  element_redox_states_cache,
                  med_cache_date,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

