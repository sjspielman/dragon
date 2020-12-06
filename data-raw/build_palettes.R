# Create tibbles with palettes for UI display

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

# qualitative only
palettes_of_interest %>% dplyr::filter(category == "qual") -> qual_palettes_ui

# sequential and diverging only
palettes_of_interest %>% dplyr::filter(category != "qual") -> sd_palettes_ui
