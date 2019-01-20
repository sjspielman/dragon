library(shiny)
library(tidyverse)
library(shinythemes)
library(colourpicker)
library(shinyWidgets)
library(RColorBrewer)
library(visNetwork)


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
    
    selectInput("element_of_interest", tags$b("Select the element whose mineral network you'd like to analyze"),
        c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr")
    ),
    
    
    selectInput("include_age", tags$b("Choose a starting eon for earliest minerals to include in the network:"),
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
    h3("Network options"),
    hr(),
    
    h4("Node Size"),
    
    sliderInput("mineral_size",tags$b("Size for mineral nodes"),value=5,min=1,max=15), 
    
    fluidRow(
        column(6,
            radioButtons("element_size_type", tags$b("Size scheme for elements"), 
                         c("Select a single size" = "singlesize",
                           "Size elements by degree" = "degree",
                           "Size elements by redox"  = "redox"), selected = "singlesize")
        ),
        column(6,    
            conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                {sliderInput("element_size",tags$b("Size for element nodes"),value=15,min=5,max=20)})
        )   
    ),

           
    br(),h4("Node Color"),
    ####### Element node color ##########
    
    checkboxInput("colorbycluster",tags$b("Color all nodes by cluster"),value = FALSE), ## Use default ggplot colors.
    #  TODO: palette selection for color by cluster (use the qualitative brewer scales)
    
    ## if colorbycluster is FALSE, reveal element/mineral selections
    conditionalPanel(condition = "output.not_color_by_cluster", 
        fluidRow(
                column(6,
                   uiOutput("color_element_by")
                ),
                column(6,
                    conditionalPanel(condition = "output.singlecolor_element",   
                        {colourInput("elementcolor", tags$b("Select color:"), value = "dodgerblue3")}
                    ),

                    conditionalPanel(condition = "output.palette_element",   
                        {pickerInput("elementpalette", label = tags$b("Select palette:"),
                        choices = divseq.list, selected = "Blues", width = "80%",
                        choicesOpt = list(
                            content = sprintf(
                                "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                                unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
                            )
                        )
                    )}        
                    )
                )
          ),
        fluidRow(
             column(6,
                uiOutput("color_mineral_by")
             ),
             column(6,
                 conditionalPanel(condition = "output.singlecolor_mineral",   
                     {colourInput("mineralcolor", tags$b("Select color:"), value = "firebrick3")}
                 ),
                 conditionalPanel(condition = "output.palette_mineral && input.color_element_by == 'singlecolor'",   
                     {pickerInput("mineralpalette", label = tags$b("Select palette:"),
                         choices = divseq.list, selected = "Blues", width = "80%",
                         choicesOpt = list(
                             content = sprintf(
                                 "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                                 unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
                             )
                         )
                     )}        
                 ) ## conditional
             ) ## column
       ) ## fluidrow
    ),  ## conditional
    
        
        
    br(),h4("Node Label and Shape"),
    fluidRow(
        column(6, checkboxInput("label_element",tags$b("Show element labels"),value = TRUE)),
        column(6, 
            conditionalPanel(condition = "input.label_element", 
                colourInput("elementlabelcolor",tags$b("Element label color"),value = "navy"))
        )
    ),
    fluidRow(
        column(6, checkboxInput("label_mineral",tags$b("Show mineral labels"),value = FALSE)),
        column(6, 
            conditionalPanel(condition = "input.label_mineral", 
                colourInput("minerallabelcolor",tags$b("Mineral label color"),value = "forestgreen"))
        )
    ),
    fluidRow(
       column(6,
           selectInput("elementshape", tags$b("Element node shape"), ## shapes that scale with font
                c("Circle"   = "circle",
                  "Ellipse"  = "ellipse",
                  "Box"      = "box", 
                  "Text only"     = "text"), selected = "Circle" 
            )
        ),
        column(6, 
            selectInput("mineralshape", tags$b("Mineral node shape"),
                        c("Circle"   = "dot", #### !!!!!!
                          "Square"   = "square",
                          "Dot"      = "dot", 
                          "Star"     = "star",
                          "Triangle" = "triangle",
                          "Diamond"  = "diamond"), selected = "Circle"    
            )
        )
    ),

    br(),h4("Edge Color"),  
    fluidRow(
      column(6,
        uiOutput("show_color_edge") 
      ),
    
      column(6,
          conditionalPanel(condition = "output.singlecolor_edge",   
              {colourInput("edgecolor", tags$b("Select edge color:"), value = "#5E5E5E")}
          ),

          conditionalPanel(condition = "output.palette_edge",   
              {pickerInput("edgepalette", label = tags$b("Edge color palette:"),
              choices = divseq.list, selected = "Blues", width = "80%",
              choicesOpt = list(
                  content = sprintf(
                      "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                      unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
                  )
              )
            )}        
          )
        )),
    
    selectInput("network_layout", tags$br("Select a layout algorithm for the network:"),
        c("Nicely"               = "layout_nicely",
          "Fruchterman Reingold" =  "layout_with_fr",
          "Kamada-Kawai"         = "layout_with_kk")
    ),
          
        
    
    #######################################################################################
    
    br(),br(),
    actionButton("go","Build Network",width="100%")),   
    
    
    ## To Do: Export network file
    ## Display network
    
    # Main panel for displaying outputs
    mainPanel(
         div(style = "height: 600px; width = 600px;outline: 1px solid black;",
            visNetworkOutput("networkplot", height = "100%")
        ) #,
       #div(style = "height: 200px;",
       #     plotOutput("networklegend", height = "75%")
       #),
#         div(style = "float:right",
#             uiOutput("downloadp"),
#             br(),
#             uiOutput("downloadl"),
#             br(),br(),
#             uiOutput("downloaddata")
#         )
#  
    )
  )
)
