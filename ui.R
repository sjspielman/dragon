library(shiny)
library(tidyverse)
library(shinythemes)
library(colourpicker)
library(shinyWidgets)
library(RColorBrewer)


#################################################################################################
### Code to setup a palette picker, modified by https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html
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


ui <- fluidPage(theme = shinytheme("simplex"),

  # App title
  titlePanel("Exploring Mineral Chemistry Networks using the rruff database"),
    helpText("Written by Stephanie J. Spielman.", (a("Source code and instructions",
        href="https://github.com/spielmanlab/shinymineral"))),
        
  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
    
    # Element of interest?
    #textInput("element_of_interest","Enter the 1-2 letter symbol for the element you'd like to analyze",value="O"),
    
    selectInput("element_of_interest", "Select the element whose mineral network you'd like to analyze",
        c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "OH" = "OH", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "PO" = "PO", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "SiO" = "SiO", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr")
    ),
    
    
    #######################################################################################
    br(),
    h4("Mineral selection"),
    
    selectInput("include_age", "Choose a starting eon for earliest minerals to include in the network:",
          c("Include all known minerals"  = "present",
            "Paleoproteozoic (>= 1.6 Ga)" = "paleo",
            "Archean (>= 2.5 Ga)"         = "archean",
            "Hadean (>= 4 Ga)"            = "hadean")
         ),
    # causes buggyness:
    #sliderInput("include_localities","Choose the _minimum_ number of localities needed to include a mineral in the network",value=1,min=1,max=50),
    ### TODO: Other selection for minerals?
    #######################################################################################

    #######################################################################################
    br(),
    h4("Element options"),

    checkboxInput("label_element","Label element nodes?",value = TRUE),
    
    ####### Element node size ##########
    #selectInput("size_element_by", "Size element by?",
    #            c("Select element size"          = "singlesize",
    #              "Degree"                       = "degree")    
    #),
    #conditionalPanel(condition = "output.selectsize_element", 
    #    {sliderInput("element_size","Choose the size for elements",value=10,min=1,max=15)}
    #),
    sliderInput("element_size","Choose the size for element nodes",value=10,min=1,max=15),
    

    
    
    ####### Element node color ##########
    selectInput("color_element_by", "Color elements by?",
                c("Single color for all elements" = "singlecolor", 
                  "Degree"                        = "degree",
                  "Cluster ID"                    = "cluster") 
    ),
    
    conditionalPanel(condition = "output.singlecolor_element",   
        {colourInput("elementcolor", "Select element color:", value = "dodgerblue3")}
    ),

    conditionalPanel(condition = "output.palette_element",   
        {pickerInput("elementpalette", label = "Choose a palette for element colors:",
          choices = divseq.list, selected = "Blues", width = "80%",
          choicesOpt = list(
            content = sprintf(
              "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
              unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
            )
          )
        )}        
    ),
    
    ####### Element node label or shape ##########
    selectInput("shape_element_by", "Shape of element nodes?",
                c("No shape (use if you'd only like the label)"   = "none",
                  "Circle"     = "circle",
                  "Square"     = "square")    
    ),
    #######################################################################################
    

    #######################################################################################
    br(),
    h4("Mineral node options"),

    checkboxInput("label_mineral","Label mineral nodes?",value = FALSE),


    sliderInput("mineral_size","Choose the size for minerals",value=4,min=1,max=15),
        
    ####### Mineral node color ##########
    selectInput("color_mineral_by", "Color minerals by?",
           c("Single color for all minerals" = "singlecolor",  
             "Mean mineral redox state"     = "redox",        
             "Maximum mineral age"          = "maxage",      
             "Number of localities"         = "numlocalities",
             "Cluster ID"                   = "cluster") 
    ),
    
    conditionalPanel(condition = "output.singlecolor_mineral",   
        {colourInput("mineralcolor", "Select mineral color:", value = "firebrick3")}
    ),

    conditionalPanel(condition = "output.palette_mineral",   
        {pickerInput("mineralpalette", label = "Choose a palette for mineral colors:",
          choices = divseq.list, selected = "Blues", width = "80%",
          choicesOpt = list(
            content = sprintf(
              "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
              unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
            )
          )
        )}        
    ),
    
    ####### Mineral node label or shape ##########
    selectInput("shape_mineral", "Shape of mineral nodes?",
                c("No shape (use if you'd only like the label)"   = "none",
                  "Circle"     = "circle",
                  "Square"     = "square")    
    ),
    #######################################################################################


    #######################################################################################
    br(),
    h4("Edge options"),
    
    
    selectInput("color_edge_by", "Color edges by?",
           c("Single color for all edges" = "singlecolor",  
             "Element redox state" = "redox")
        ),
    conditionalPanel(condition = "output.singlecolor_edge",   
        {colourInput("edgecolor", "Select edge color:", value = "black")}
    ),

    conditionalPanel(condition = "output.palette_edge",   
        {pickerInput("edgepalette", label = "Choose a palette for edge colors:",
          choices = divseq.list, selected = "Blues", width = "80%",
          choicesOpt = list(
            content = sprintf(
              "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
              unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
            )
          )
        )}        
    ),
        #######################################################################################


    #######################################################################################
    #br(),
    #h4("Network options"),
    
    ## TODO: There are about 30 options here.
    #selectInput("networkalgorithm", "Select the algorithm to use for network display:",
    #    c(
    #
    
    
    #######################################################################################
    
    br(),br(),
    actionButton("go","Build Network",width="100%")),   
    
    
    ## To Do: Export network file
    ## Display network
    
    # Main panel for displaying outputs
    mainPanel(
      
        div(style = "height: 800px;",
            plotOutput("networkplot", height = "100%")
        ),
       div(style = "height: 100px;",
            plotOutput("networklegend", height = "100%")
       )
 
    #
    #    uiOutput("download")

    )
  )
)
