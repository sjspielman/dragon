#' Default element node color
#' @noRd
default_element_color    <- "skyblue"
#' Default mineral node color
#' @noRd
default_mineral_color    <- "firebrick3"
#' Default edge color
#' @noRd
default_edge_color       <- "grey30"
#' Default element node color palette
#' @noRd
default_element_palette  <- "Blues"
#' Default mineral node color palette
#' @noRd
default_mineral_palette  <- "Reds"
#' Default color palette for coloring nodes by community cluster
#' @noRd
default_cluster_palette  <- "Dark2"
#' Default edge color palette
#' @noRd
default_edge_palette     <- "BrBG"
#' Default edge color
#' @noRd
default_edge_color       <- "#5E5E5E"
#' Default color to highlight focal node 
#' @noRd
default_highlight_color  <- "lightgoldenrod1"
#' Default color for selected nodes 
#' @noRd
default_selection_color  <- "chartreuse3"
#' Default color for NA attributes, used when a palette over an attribute is specified
#' @noRd
default_na_color         <- "#DCDCDC"
#' Default element node label color
#' @noRd
default_element_label_color <- "black"
#' Default mineral node label color
#' @noRd
default_mineral_label_color <- "black"
#' Default element node shape
#' @noRd
default_element_shape       <- "circle"
#' Default mineral node shape
#' @noRd
default_mineral_shape       <- "dot"
#' Constant for reducing element size by a factor to appear nicely in browser
#' @noRd
element_size_scale_divisor <- 1
#' Constant for reducing mineral size by a factor to appear nicely in browser
#' @noRd
mineral_size_scale_divisor <- 10
#' Minimum-allowed mineral node size
#' @noRd
mineral_size_min <- 5
#' Maximum-allowed mineral node size
#' @noRd
mineral_size_max <- 30
#' Minimum-allowed element node size
#' @noRd
element_size_min <- 1
#' Maximum-allowed element node size
#' @noRd
element_size_max <- 4
#' Scalar for adding node border and hover colors, which are lightened/darkened node colors by this factor
#' @noRd
lighten_darken_factor <- 0.3
#' All variable options for coloring nodes by which are discrete ordinal rather than numeric continuous 
#' @noRd
ordinal_color_variables <- c("element_hsab") # the rest have too many categories, but keep variable in case.
