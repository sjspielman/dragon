##### Global variables, functions, strings used in dragon #####

## Legend titles and renamed columns for DT
variable_to_title <- c("redox"               = quo("Mean Redox State"), 
                        "max_age"             = quo("Maximum Age (Ga)"), 
                        "num_localities"      = quo("Number of known localities"), 
                        "network_degree_norm" = quo("Degree centrality (normalized)"), 
                        "network_degree"      = quo("Degree centrality"), 
                        "closeness"           = quo("Closeness centrality"),
                        "pauling"             = quo("Electronegativity"), 
                        "mean_pauling"        = quo("Mean electronegativity"),  
                        "sd_pauling"          = quo("Std Dev electronegativity"),
                        "cov_pauling"         = quo("COV electronegativity"),
                        "id"                  = quo("Node name"), 
                        "cluster_ID"          = quo("Community Cluster"), 
                        "group"               = quo("Node type"),
                        "element"             = quo("Element"),
                        "mineral_name"        = quo("Mineral"),
                        "mineral_id"          = quo("Mineral ID"),
                        "mindat_id"           = quo("Mindat ID"),
                        "at_locality"         = quo("At Locality"),
                        "is_remote"           = quo("Is Remote"),
                        "rruff_chemistry"     = quo("Chemistry"))
    
model_response_choices <- c("Maximum Age (Ga)", 
                            "Mean electronegativity",
                            "Std Dev electronegativity",
                            "COV electronegativity",
                            "Degree centrality (normalized)",
                            "Closeness centrality",
                            "Number of known localities")

model_predictor_choices <- c(model_response_choices, "Community Cluster")



#################################################################################################
### Code to setup a palette picker, modified from https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html
brewer.pal.info %>% 
    rownames_to_column("palette") %>%
    filter(category != "qual", colorblind == TRUE) %>%
    arrange(desc(category)) -> brewer.palettes
divseq.list <- list("Sequential" = brewer.palettes$palette[brewer.palettes$category == "seq"], "Diverging" = brewer.palettes$palette[brewer.palettes$category == "div"]) 
brewer.palettes.hex <- brewer.palettes %>% mutate(colorlist = map2(maxcolors,palette, brewer.pal))
palette.list <- setNames(as.list(brewer.palettes.hex$colorlist), brewer.palettes.hex$palette)

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
palette.linear.gradient <- unlist(lapply(X = palette.list, FUN = linear_gradient))
palette.label.colors <- ifelse(brewer.palettes$category == "seq", "black", "white")
#################################################################################################


## mediocre matching here.
vis_to_gg_shape <- list("circle"  = 19,
                        "dot"     = 19,
                        "ellipse" = 19,
                        "box"     = 15,
                        "text"    = 19,
                        "square"  = 15,
                        "star"    = 8,
                        "diamond" = 18,
                        "triangle" = 17)

na.gray <- "#DCDCDC"
geom.point.size <- 8
theme_set(theme_cowplot() + theme(legend.position = "bottom",
                                  legend.text = element_text(size=11),
                                  legend.key.size = unit(1, "cm"),
                                  legend.title = element_text(size=13),
                                  legend.box.background = element_rect(color = "white")))                                  






obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, legendtitle)
{
    
    cvar <- as.symbol(color_variable)
    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.

    dat2 %>% 
        ungroup() %>%
        dplyr::select( color_variable ) %>% 
        na.omit() -> dat_check
    
    shiny::validate(
        shiny::need(nrow(dat_check) > 0, 
        "ERROR: The specified color scheme cannot be applied due to insufficient node information in the MED database.")
    )  

    if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = factor(!!cvar), color = factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle, na.value = na.gray) + guides(colour = guide_legend(title.position="left", title.hjust = 0.5, byrow=TRUE)  )
    if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename, direction = -1, na.value = na.gray)+ guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))

    data.colors <- ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    rename(color = colour) ## god help me, hadley. 
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}





obtain_colors_legend_single <- function(group, singleshape, singlecolor)
{
    p <- tibble(x = 1, y = 1, type = group) %>% 
        ggplot(aes(x=x,y=y,color=type)) + 
            geom_point(size = geom.point.size, shape = singleshape) + 
            scale_color_manual(name = "", values=c(singlecolor), na.value=na.gray) + 
            theme(legend.text = element_text(size=16))
    data.colors <- ggplot_build(p)$data[[1]]$colour
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

  
  
  
  
  
obtain_node_sizes <- function(dat, size_variable, lowsize, highsize, size_scale = 1)
{
    
    svar <- as.symbol(size_variable)

    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.
    p <- ggplot(dat2, aes(x = label, y = !!svar, size = !!svar)) + geom_point() + scale_size(range = c(lowsize,highsize))
    
    data.size <-  ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    mutate(size = size * size_scale)

    return (data.size)
}
  