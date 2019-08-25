##### Global variables, functions, strings used in dragon #####

all_elements = c("Ag", "Al", "As", "Au", "B", "Ba", "Be", "Bi", "Br", "C", "Ca", "Cd", "Ce", "Cl", "Co", "Cr", "Cs", "Cu", "Dy", "Er", "F", "Fe", "Ga", "Gd", "Ge", "H", "Hf", "Hg", "I", "In", "Ir", "K", "La", "Li", "Mg", "Mn", "Mo", "N", "Na", "Nb", "Nd", "Ni", "O", "Os", "P", "Pb", "Pd", "Pt", "Rb", "Re", "REE", "Rh", "Ru", "S", "Sb", "Sc", "Se", "Si", "Sm", "Sn", "Sr", "Ta", "Te", "Th", "Ti", "Tl", "U", "V", "W", "Y", "Yb", "Zn", "Zr")
options(scipen=10000)

element_redox_mineral_str   <- "Element redox in mineral" 
element_redox_network_str   <- "Mean element redox in network"
max_age_str                 <- "Maximum known age (Ga) of mineral" 
max_age_locality_str        <- "Maximum age (Ga) of mineral at locality" 
num_localities_mineral_str  <- "Number of known mineral localities" 
num_localities_element_str  <- "Number of known element localities" 
num_localities_str          <- "Number of known localities" 
network_degree_norm_str     <- "Degree centrality (normalized)" 
network_degree_str          <- "Degree centrality" 
closeness_str               <- "Closeness centrality"
pauling_str                 <- "Element electronegativity" 
mean_pauling_str            <- "Mean mineral electronegativity"  
sd_pauling_str              <- "Std Dev mineral electronegativity"
cov_pauling_str             <- "COV mineral electronegativity"
id_str                      <- "Node name" 
cluster_ID_str              <- "Community cluster" 
group_str                   <- "Node type"
element_str                 <- "Element"
element_name_str            <- "Full element name"
mineral_name_str            <- "Mineral"
mineral_name_node_table_str <- "Minerals containing element"
element_node_table_str      <- "Elements in mineral"
mineral_id_str              <- "Mineral ID"
mindat_id_str               <- "Mindat ID"
locality_longname_str       <- "Locality name"
age_type_str                <- "Age type"
rruff_chemistry_str         <- "RRUFF formula"
ima_chemistry_str           <- "IMA formula"
element_hsab_str            <- "Element HSAB theory"
element_mass_str            <- "Atomic mass"
element_protons_str         <- "Number of protons"
element_group_str           <- "Element group"
element_period_str          <- "Element period"
element_metaltype_str       <- "Element metal type"
element_density_str         <- "Element density"
element_specificheat_str    <- "Element specific heat"


discrete_color_variables <- c("element_hsab", "MetalType", "TablePeriod", "TableGroup")
# 
#  [1] "id"                     "element_hsab"           "element_pH"            
#  [4] "AtomicMass"             "NumberofNeutrons"       "NumberofProtons"       
#  [7] "NumberofElectrons"      "Period"                 "Group"                 
# [10] "Radioactive"            "AtomicRadius"           "pauling"               
# [13] "MetalType"              "Density"                "MeltingPoint"          
# [16] "BoilingPoint"           "SpecificHeat"           "NumberofShells"        
# [19] "NumberofValence"        "base_element"           "element_redox_network" 

## Legend titles and renamed columns for DT
variable_to_title <-  c("element_redox_mineral" = element_redox_mineral_str, 
                        "element_redox_network" = element_redox_network_str,
                        "max_age" = max_age_str, 
                        "num_localities_mineral" = num_localities_mineral_str, 
                        "num_localities_element" = num_localities_element_str, 
                        "num_localities" = num_localities_str, # MODELING ONLY
                        "locality_longname"      = locality_longname_str,
                        "network_degree_norm" = network_degree_norm_str, 
                        "network_degree" = network_degree_str, 
                        "closeness" = closeness_str,
                        "pauling" = pauling_str, 
                        "mean_pauling" = mean_pauling_str,  
                        "sd_pauling" = sd_pauling_str,
                        "cov_pauling" = cov_pauling_str,
                        "id" = id_str, 
                        "cluster_ID" = cluster_ID_str, 
                        "group" = group_str,
                        "element" = element_str,
                        "element_name" = element_name_str,
                        "mineral_name" = mineral_name_str,
                        "mineral_id" = mineral_id_str,
                        "mindat_id" = mindat_id_str,
                        "age_type" = age_type_str,
                        "rruff_chemistry" = rruff_chemistry_str,
                        "ima_chemistry" = ima_chemistry_str,
                        "element_hsab" = element_hsab_str, 
                        "NumberofProtons" = element_protons_str, 
                        "AtomicMass" = element_mass_str,
                        "TableGroup" = element_group_str,
                        "TablePeriod" = element_period_str, 
                        "MetalType" = element_metaltype_str,
                        "Density" = element_density_str,
                        "SpecificHeat" = element_specificheat_str, 
                        "max_age_locality" = max_age_locality_str)


selected_node_table_column_choices_mineral   <- c(mineral_name_str, mineral_id_str, rruff_chemistry_str, ima_chemistry_str, max_age_str, num_localities_mineral_str, element_redox_mineral_str, mean_pauling_str, cov_pauling_str) #sd_pauling_str
selected_node_table_column_choices_element   <- c(element_str, element_name_str, element_redox_network_str, pauling_str, element_hsab_str, num_localities_element_str, element_group_str, element_period_str, element_metaltype_str)
selected_node_table_column_choices_netinfo   <- c(cluster_ID_str, network_degree_norm_str, closeness_str)
selected_node_table_column_choices_locality  <- c(mindat_id_str, locality_longname_str, max_age_locality_str)


    
model_response_choices <- c(max_age_str, 
                            mean_pauling_str,
                            cov_pauling_str,
                            network_degree_norm_str,
                            closeness_str,
                            num_localities_str) #sd_pauling_str,


model_predictor_choices <- c(model_response_choices, cluster_ID_str)

########################################################################
geo_timeline %>% 
    dplyr::select(level, interval_name, late_age, early_age) %>%
    filter(level <= 2) %>%
    rename(ymin = level) %>%
    mutate(ymin = ymin + 2,
    ymax = ymin + 1,
    label_x = (late_age+early_age)/2, 
    label_y = (ymin+ymax)/2)  -> geo_data


geo_data %>% 
    group_by(ymin) %>% 
    tally() %>% 
    pull(n) -> level_ncat

geo_data %>% arrange(ymin, early_age) %>% pull(interval_name) -> interval_name_levels
geo_data$interval_name <- factor(geo_data$interval_name, interval_name_levels)
all_geo_colors <- c( colorRampPalette(brewer.pal(9,"BuPu"))(level_ncat[1]),
                 colorRampPalette(brewer.pal(9,"BuPu"))(level_ncat[2])) #,
                # colorRampPalette(brewer.pal(9,"Greens"))(level_ncat[3]))




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






obtain_colors_legend <- function(session, dat, color_variable, variable_type, palettename, legendtitle, discrete_colors = NA)
{
    
    cvar <- as.symbol(color_variable)
    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.

    dat2 %>% 
        ungroup() %>%
        dplyr::select( color_variable ) %>% 
        na.omit() -> dat_check
    
    
            
    if (nrow(dat_check) <= 0)
    {
        createAlert(session, "alert", "bad_network", title = '<h4 style="color:black;">Error</h4>', style = "warning",
            content = '<p style="color:black;">The specified color scheme cannot be applied due to insufficient node information in the MED database. Please select a different color scheme.</p>')
        shiny::validate( shiny::need(nrow(dat_check) > 0, ""))
    }

    if (variable_type == "d")
    {
    
        p <- ggplot(dat2, aes(x = x, y = factor(!!cvar), color = factor(!!cvar))) + 
                geom_point(size = geom.point.size) + 
                 guides(colour = guide_legend(title.position="top",  title.hjust = 0.5, byrow=TRUE, nrow=2)  ) +
                 theme(legend.key.size = unit(0.05, 'lines'), legend.title = element_text(size = rel(0.8)))
        if (!(is.na(discrete_colors))) 
        {
            p <- p + scale_color_manual(name = legendtitle, na.value = na.gray, values = discrete_colors)
        } else {
            p <- p + scale_color_discrete(name = legendtitle, na.value = na.gray)
        } 
    }
    
    if (variable_type == "c")
    {
    
        p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + 
                 geom_point(size = geom.point.size) + 
                 scale_color_distiller(name = legendtitle, palette = palettename, direction = -1, na.value = na.gray) + 
                 guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5, frame.colour = "black", ticks.colour = "black"), 
                        size = guide_legend(title.position="top", title.hjust = 0.5)) +
                theme(legend.text = element_text(size = rel(0.8)))
    }
    
    
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
  