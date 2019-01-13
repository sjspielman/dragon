library(shiny)
library(tidyverse)
library(shinythemes)
library(colourpicker)
library(shinyWidgets)
library(RColorBrewer)


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
    h3("Node options"),
    hr(),
    
    h4("Node Size"),
    fluidRow(
        column(6,sliderInput("element_size","Size for element nodes",value=10,min=1,max=15)),
        column(6,sliderInput("mineral_size","Size for mineral nodes",value=4,min=1,max=15))
    ),

    br(),h4("Node Labels"),
    fluidRow(
        column(6, checkboxInput("label_element","Show element labels",value = TRUE)),
        column(6, checkboxInput("label_mineral","Show mineral labels",value = TRUE))
    ),
    
    #checkboxInput("label_element","Check box to label element nodes",value = TRUE),
    #checkboxInput("label_mineral","Check box to label mineral nodes",value = FALSE),
       
    br(), h4("Node Color"),
    ####### Element node color ##########
       fluidRow(
            column(6,
               selectInput("color_element_by", "Element color scheme",
                c("Select color"   = "singlecolor", 
                  "Degree"         = "degree",
                  "Cluster ID"     = "cluster") 
                )
            ),
    
            column(6,
                conditionalPanel(condition = "output.singlecolor_element",   
                    {colourInput("elementcolor", "Select color:", value = "dodgerblue3")}
                ),

                conditionalPanel(condition = "output.palette_element",   
                    {pickerInput("elementpalette", label = "Select palette:",
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
                selectInput("color_mineral_by", "Mineral color scheme",
                       c("Select color" = "singlecolor",  
                         "Mean mineral redox state"     = "redox",        
                         "Maximum mineral age"          = "maxage",      
                         "Number of localities"         = "numlocalities",
                         "Cluster ID"                   = "cluster") 
                )
            ),
    
            column(6,
                conditionalPanel(condition = "output.singlecolor_mineral",   
                    {colourInput("mineralcolor", "Select color:", value = "firebrick3")}
                ),

                conditionalPanel(condition = "output.palette_mineral",   
                    {pickerInput("mineralpalette", label = "Select palette:",
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
        
        
        
    br(), h4("Node Shape"),
    fluidRow(
       column(6,
           selectInput("shape_element_by", "Element nodes",
                c("No shape (only label)"   = "none",
                  "Circle"     = "circle",
                  "Square"     = "square")    
            )
        ),
        column(6, 
            selectInput("shape_mineral", "Mineral nodes",
                        c("No shape (only label)"   = "none",
                          "Circle"     = "circle",
                          "Square"     = "square")    
            )
        )
    ),

    br(),
    h3("Edge Options"),
    hr(),    
    fluidRow(
      column(6,
          selectInput("color_edge_by", "Color edges by?",
           c("Select color" = "singlecolor",  
             "Element redox state" = "redox")
          )
      ),
    
      column(6,
          conditionalPanel(condition = "output.singlecolor_edge",   
              {colourInput("edgecolor", "Select edge color:", value = "grey50")}
          ),

          conditionalPanel(condition = "output.palette_edge",   
              {pickerInput("edgepalette", label = "Edge color palette:",
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
        

####### TODO:
#     br(),
#     h4("Network options"),
#     
#     selectInput("network_layout", "Select a layout algorithm for the network:",
#         c("
        
    
    #######################################################################################
    
    br(),br(),
    actionButton("go","Build Network",width="100%")),   
    
    
    ## To Do: Export network file
    ## Display network
    
    # Main panel for displaying outputs
    mainPanel(
          
        fluidRow(
            column(10,
                div(style = "height: 800px;",
                    plotOutput("networkplot", height = "800px")
                    )
            ),
    
            column(2,
                div(style = "height: 200px;",
                    plotOutput("networklegend",, height = "200px")
                    )
            )
        )
#         div(style = "height: 800px;",
#             plotOutput("networkplot", height = "100%")
#         ),
#        div(style = "height: 100px;",
#             plotOutput("networklegend", height = "100%")
#        )
 
    #
    #    uiOutput("download")

    )
  )
)
