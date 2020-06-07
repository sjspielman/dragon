## code to prepare internal datasets goes here

library(dragon)

## LAST UPDATED ON 6/7/20 WITH MED 2/3/20 ##

rruff_data_cache <- fetch_rruff_data()
element_redox_states_cache <- calculate_element_redox_states(rruff_data_cache)
rruff_cache_date <- find_most_recent_date()
     
usethis::use_data(rruff_data_cache,
                  element_redox_states_cache,
                  rruff_cache_date,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

