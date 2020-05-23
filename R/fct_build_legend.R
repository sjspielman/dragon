build_legend <- function(edge_styler, node_styler)
{

  finallegend <- NULL
  if (is.na(node_styler$both_legend)) 
  {   ## Mineral, element
    if (is.na(edge_styler$edge_legend)) 
    { 
      finallegend <- cowplot::plot_grid(node_styler$element_legend, node_styler$mineral_legend, nrow=1)
    } else {
      ### mineral, element, edge
      finallegend <- cowplot::plot_grid(node_styler$element_legend, node_styler$mineral_legend, edge_styler$edge_legend, nrow=1, scale=0.75)
    }
  } else ## both_legend is NOT NA
  {
    ## NO EDGES
    if (is.na(edge_styler$edge_legend)) 
    { 
      finallegend <- node_styler$both_legend
    } else { ### BOTHNODES, EDGES
      ### both, edge
      finallegend <- cowplot::plot_grid(node_styler$both_legend, edge_styler$edge_legend, nrow=1, scale=0.75)
    }
  }   
  return(finallegend)
}
