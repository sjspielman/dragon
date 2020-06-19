#' Build legend of network color attributes for display.
#'
#' @param edge_styler A list of edge stylings as created by dragon::style_edges()
#' @param node_styler A list of node stylings as created by dragon::style_nodes()
#'
#' @return A plot list of legends for display created with cowplot::plot_grid()
#' @noRd
build_legend <- function(edge_styler, node_styler)
{
  ## If logical, it's FALSE and there is no legend
  ## If list, there is a legend
  legend_scale <- 0.5 # I'm fairly certain this does absolutely nothing.
  finallegend <- NULL
  if (typeof(node_styler$both_legend) == "logical")
  {   
    stopifnot(node_styler$both_legend == FALSE)
    ## Mineral, element
    if (typeof(edge_styler$edge_legend) == "logical") 
    { 
      stopifnot(edge_styler$edge_legend == FALSE)
      finallegend <- cowplot::plot_grid(node_styler$element_legend, node_styler$mineral_legend, nrow=1, scale=legend_scale)
    } else {
      ### mineral, element, edge
      finallegend <- cowplot::plot_grid(node_styler$element_legend, node_styler$mineral_legend, edge_styler$edge_legend, nrow=1, scale=legend_scale)
    }
  } else ## both_legend is NOT NA
  {
    ## NO EDGES
    if (typeof(edge_styler$edge_legend) == "logical") 
    { 
      stopifnot(edge_styler$edge_legend == FALSE)
      finallegend <- node_styler$both_legend
    } else { ### BOTHNODES, EDGES
      ### both, edge
      finallegend <- cowplot::plot_grid(node_styler$both_legend, edge_styler$edge_legend, nrow=1, scale=legend_scale)
    }
  }   
  return(finallegend)
}
