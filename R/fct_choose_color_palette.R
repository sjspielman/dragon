### Code to setup a palette picker, modified from https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

prep_palette_display <- function(target_category)
{
  
  RColorBrewer::brewer.pal.info %>% 
    tibble::rownames_to_column("palette") %>%
    dplyr::filter(colorblind == TRUE,
           category %in% target_category) %>%
    dplyr::arrange(desc(category)) -> palettes
  
  palettes.hex <- palettes %>% dplyr::mutate(colorlist = purrr::map2(maxcolors, palette, RColorBrewer::brewer.pal))
  palette.list <- setNames(as.list(palettes.hex$colorlist), palettes.hex$palette)
  palette.linear.gradient <- unlist(lapply(X = palette.list, FUN = linear_gradient))
  palette.label.colors <- dplyr::case_when(palettes$category == "seq" ~"black",
                                           palettes$category == "div" ~"white",
                                           palettes$category == "qual" ~"black")
  palette.labels <- 
    if ("qual" %in% target_category){
      palette.labels <- list("Qualitative" = palettes$palette[palettes$category == "qual"]) 
    } else {
      palette.labels <- list("Sequential" = palettes$palette[palettes$category == "seq"], "Diverging" = palettes$palette[palettes$category == "div"]) 
    }
  
  return(list("palette_names" = palette.labels, "linear_gradient" = palette.linear.gradient, "label_colors" = palette.label.colors))
}