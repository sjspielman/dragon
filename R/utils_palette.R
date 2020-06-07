## Construct HTML formatted string used in Shiny UI for palette selection
first_part  <- "<img src='www/palette_png/"
middle_part <- ".png' width=110px><div class='palette-style'>"
last_part   <- "</div></img>"
RColorBrewer::brewer.pal.info %>% 
  tibble::rownames_to_column("name") %>%
  dplyr::filter(colorblind == TRUE) %>%
  dplyr::mutate(img = paste0(first_part, name, middle_part, name, last_part)) %>%
  dplyr::arrange(desc(category), name)-> palettes_of_interest

palettes_of_interest %>%
  dplyr::filter(category == "qual") -> qual_palettes_ui

palettes_of_interest %>%
  dplyr::filter(category != "qual") -> sd_palettes_ui

       