## code to prepare internal datasets goes here. 
## takes some time to (5-15 min), mostly due to querying/downloading from MED, but also due to mildly inefficient code in my weighted calculations that I do not care to clean. It's like a built-in coffee break!

#library(dragon)
devtools::load_all()

## LAST UPDATED ON 10/5/20 WITH MED 2/3/20 ##

med_data_cache <- fetch_med_data()
element_redox_states_cache <- calculate_element_redox_states(med_data_cache)
med_cache_date <- find_most_recent_date()
    
    

# Tibble containing all RColorBrewer palettes used in dragon node coloring
RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) %>%
  dplyr::mutate(img = paste0("<img src='www/palette_png/", 
                             name, 
                             ".png' width=110px><div class='palette-style'>",
                             name, 
                             "</div></img>")) %>%
  dplyr::arrange(dplyr::desc(category), name)-> palettes_of_interest

palettes_of_interest %>% dplyr::filter(category == "qual") -> qual_palettes_ui

palettes_of_interest %>% dplyr::filter(category != "qual") -> sd_palettes_ui

usethis::use_data(med_data_cache,
                  element_redox_states_cache,
                  med_cache_date,
                  qual_palettes_ui,
                  sd_palettes_ui,
                  internal = TRUE, overwrite = TRUE, compress = "bzip2")

