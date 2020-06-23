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

## DON'T USE #' !!!!
# Tibble containing all RColorBrewer palettes used in mineral timeline coloring
RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) %>%
  dplyr::mutate(img = paste0("<img src='www/palette_png/", 
                             name, 
                             "_timeline.png' width=110px><div class='palette-style'>",
                             name, 
                             "</div></img>")) %>%
  dplyr::arrange(dplyr::desc(category), name)-> palettes_of_interest_mineral_timeline


#' Tibble containing all qualitative RColorBrewer palettes used in dragon node cluster coloring
#' @noRd
palettes_of_interest %>%
  dplyr::filter(category == "qual") -> qual_palettes_ui

# Tibble containing all sequential and diverging RColorBrewer palettes used in dragon node coloring
palettes_of_interest %>%
  dplyr::filter(category != "qual") -> sd_palettes_ui

# Tibble containing all sequential and diverging RColorBrewer palettes used mineral timeline coloring
palettes_of_interest_mineral_timeline %>%
  dplyr::filter(category != "qual") -> sd_palettes_timeline_ui
