## Construct HTML formatted string used in Shiny UI for palette selection --------------------

#' Tibble containing all RColorBrewer palettes used in dragon
RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) %>%
  dplyr::mutate(img = paste0("<img src='www/palette_png/", 
                             name, 
                             ".png' width=110px><div class='palette-style'>",
                             name, 
                             "</div></img>")) %>%
  dplyr::arrange(desc(category), name)-> palettes_of_interest


#' Tibble containing all qualitative RColorBrewer palettes used in dragon
palettes_of_interest %>%
  dplyr::filter(category == "qual") -> qual_palettes_ui

#' Tibble containing all sequential and diverging RColorBrewer palettes used in dragon
palettes_of_interest %>%
  dplyr::filter(category != "qual") -> sd_palettes_ui

       