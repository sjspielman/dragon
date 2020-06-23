## Construct HTML formatted string used in Shiny UI for palette selection --------------------

## DON'T USE #' !!!!
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


#' Tibble containing all qualitative RColorBrewer palettes
#' @noRd
palettes_of_interest %>%
  dplyr::filter(category == "qual") -> qual_palettes_ui

# Tibble containing all sequential and diverging RColorBrewer palettes
palettes_of_interest %>%
  dplyr::filter(category != "qual") -> sd_palettes_ui

