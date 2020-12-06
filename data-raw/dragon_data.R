## code to prepare internal datasets goes here. 
## takes some time to (5-15 min), mostly due to querying/downloading from MED, but also due to mildly inefficient code in my weighted calculations that I do not care to clean. It's like a built-in mandatory coffee break!

#library(dragon)
devtools::load_all()

## LAST UPDATED ON 10/5/20 WITH MED 2/3/20 ##

# Obtain MED data
med_data_cache <- fetch_med_data()
element_redox_states_cache <- calculate_element_redox_states(med_data_cache)
med_cache_date <- find_most_recent_date()

# Tabulate element counts and calculate weighted mean and COV MEE
source("calculate_weighted_pauling.R")  # produces `final_weighted_pauling`

# Palettes UI tibbles
source("build_palettes.R") # produces `qual_palettes_ui` and `sd_palettes_ui`


# Save to R/sysdata.Rda
usethis::use_data(med_data_cache,
                  element_redox_states_cache,
                  med_cache_date,
                  final_weighted_pauling,
                  qual_palettes_ui,
                  sd_palettes_ui,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")





############## NOT DOING THIS FOR 1.1 ##################
# Remove minerals with rruff/ima inconsistency
#med_data_cache %>%
  #dplyr::filter(!(mineral_name %in% exclude_minerals_from_dragon)) %>%  -> med_data_cache
#element_redox_states_cache %>%
#  dplyr::filter(!(mineral_name %in% exclude_minerals_from_dragon)) -> element_redox_states_cache
###########################################################
