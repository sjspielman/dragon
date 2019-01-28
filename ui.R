library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(DT)
library(RColorBrewer)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(cowplot)
library(igraph)


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
    helpText("Written by Stephanie J. Spielman, PhD", (a("Source code and instructions",
        href="https://github.com/spielmanlab/shinymineral"))),
        
 # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
    
    # Element of interest?
    #textInput("element_of_interest","Enter the 1-2 letter symbol for the element you'd like to analyze",value="O"),
    
   selectInput("element_of_interest", tags$b("Select the element(s) whose mineral network you'd like to analyze"),
        c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
        multiple=FALSE
    ),
    ##### TODO: ADD A SELECT ALL BUTTON ####
    ## modify network building code for several elements

    helpText("To analyze the network of a different focal element, you must refresh the site."), 
    
    #######################################################################################

    #######################################################################################
    br(),
    h3("Mineral preferences"),
    hr(),
    selectInput("include_age", tags$b("Choose a starting eon for earliest minerals to include in the network:"),
          c("Include all known minerals"  = "present",
            "Paleoproteozoic (>= 1.6 Ga)" = "paleo",
            "Archean (>= 2.5 Ga)"         = "archean",
            "Hadean (>= 4 Ga)"            = "hadean")
    ),

    
    
    br(), h3("Network options"),
    hr(),
    # Currently user's choice. 
    #helpText("NOTE: Only one", tags$i("attribute color scale"), ", including both nodes and edges, is allowed for the network. Once a single color scale has been selected, other color options default to single color selection."),

           
    br(),h4("Node Colors"),
    checkboxInput("color_by_cluster",tags$b("Click to color all nodes by network cluster"),value = FALSE), ## Use default ggplot colors.
    
    ## if color_by_cluster is FALSE, reveal element/mineral selections
    conditionalPanel(condition = "input.color_by_cluster == false", 
    fluidRow(
                column(8,
                   selectInput("color_element_by", tags$b("Select a color scheme for elements"),
                                c("Use a single color for all elements"    = "singlecolor",  
                                  "Color elements based on network degree" = "network_degree_norm"))
                   #uiOutput("color_element_by")
                
                ),
                column(4,
                    conditionalPanel(condition = "input.color_element_by == 'singlecolor'",   
                        {colourInput("elementcolor", tags$b("Select element color:"), value = "skyblue")}
                    ),

                    conditionalPanel(condition = "input.color_element_by != 'singlecolor'",   
                        {pickerInput("elementpalette", label = tags$b("Element palette:"),
                        choices = divseq.list, selected = "Blues", width = "90%",
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
             column(8,
                selectInput("color_mineral_by", tags$b("Select a color scheme for minerals"),
                                c("Use a single color for all minerals"    = "singlecolor",  
                                  "Color minerals based on mean redox state"      = "redox",        
                                  "Color minerals based on maximum age"           = "max_age",      
                                  "Color minerals based on number of localities"  = "num_localities"))
                #uiOutput("color_mineral_by")
             ),
             column(4,
                 conditionalPanel(condition = "input.color_mineral_by == 'singlecolor'",   
                     {colourInput("mineralcolor", tags$b("Select mineral color:"), value = "firebrick3")}
                 ),
                 conditionalPanel(condition = "input.color_mineral_by != 'singlecolor'",   
                 #conditionalPanel(condition = "output.palette_mineral && input.color_element_by == 'singlecolor'",   
                     {pickerInput("mineralpalette", label = tags$b("Mineral palette:"),
                         choices = divseq.list, selected = "Reds", width = "90%",
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
    
    fluidRow(
        column(8, checkboxInput("highlight_my_element",tags$b("Highlight element(s) of interest"),value = FALSE)),
        column(4,
        conditionalPanel(condition = "input.highlight_my_element == true",   
                     {colourInput("elementhighlight", tags$b("Select highlight color:"), value = "lightgoldenrod1")}
                 )
        )
    ),
    

    br(),h4("Edge Attributes"),  
    fluidRow(
      column(8,
        selectInput("color_edge_by", tags$b("Select a color scheme for edges"),
                                c("Use a single color for all edges" = "singlecolor",  
                                  "Color edges by mean element redox state" = "redox"))
        #uiOutput("show_color_edge") 
      ),
    
      column(4,
          conditionalPanel(condition = "input.color_edge_by == 'singlecolor'",   
              {colourInput("edgecolor", tags$b("Edge color:"), value = "#5E5E5E")}
          ),

          conditionalPanel(condition = "input.color_edge_by != 'singlecolor'",   
              {pickerInput("edgepalette", label = tags$b("Edge palette:"),
              choices = divseq.list, selected = "BrBG", width = "90%",
              choicesOpt = list(
                  content = sprintf(
                      "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                      unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
                  )
              )
            )}        
          )
        )),
    fluidRow(
      column(8, sliderInput("edge_weight",tags$b("Weight for edges"),value=5,min=1,max=10)
    )),
    
    br(), h4("Node Size"),
    fluidRow(
        column(8,
            radioButtons("element_size_type", tags$b("How should element nodes be sized?"), 
                         c("Single size for all element nodes" = "singlesize",
                           "Size element nodes by network degree" = "network_degree_norm")
                       ), selected = "singlesize"
        ),
        column(4,    
            conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                {sliderInput("element_label_size",tags$b("Element node size"),value=50,min=10,max=100, step=10)}) #### !!!!!!! label size!!!!!!!
        ),
        column(4,    
            conditionalPanel(condition = "input.element_size_type != 'singlesize'", 
                {sliderInput("size_scale",tags$b("Scale factor for element node size"),value=50,min=10,max=150,step=10)}) 
        )     
    ),
     fluidRow(
        column(8, htmlOutput("mineral_size_statement")), 
        column(4, sliderInput("mineral_size",tags$b("Mineral node size"),value=10,min=0,max=50, step = 5))
    ),   
    
    br(),h4("Node Labels"),
    helpText("Element label size is automatically determined based on element node size."),
    fluidRow(
            column(6, colourInput("element_label_color",tags$b("Element label color"),value = "#000000"))
    ),
       fluidRow( 
            column(6, checkboxInput("label_mineral",tags$b("Click to show mineral labels"),value = FALSE))
        ),
    conditionalPanel(condition = "input.label_mineral", {
        fluidRow(
            column(6, sliderInput("mineral_label_size",tags$b("Mineral label font size"),value=10,min=1,max=100)),
            column(6, colourInput("mineral_label_color",tags$b("Mineral label color"),value = "#000000"))
        )
    }),

    br(),h4("Node Shapes"),
    fluidRow(
       column(6,
           selectInput("element_shape", tags$b("Element node shape"), ## shapes that scale with font
                c("Circle"     = "circle",
                  "Ellipse"    = "ellipse",
                  "Box"        = "box", 
                  "Text only (no shape)"  = "text"), selected = "Circle" 
            )
        ),
        column(6, 
            selectInput("mineral_shape", tags$b("Mineral node shape"),  ## shapes that DO NOT scale with font
                        c("Circle"   = "dot", #### !!!!!!
                          "Square"   = "square",
                          "Star"     = "star",
                          "Triangle" = "triangle",
                          "Diamond"  = "diamond"), selected = "Circle"    
            )
        )
    ),
        
    br(),h4("Display"),
    selectInput("network_layout", tags$br("Select a layout algorithm for the network:"),
        c("Nicely"               = "layout_nicely",
          "Fruchterman Reingold" =  "layout_with_fr",
          "Kamada-Kawai"         = "layout_with_kk")
    ),

    #######################################################################################
    
    br(),br(),
    actionButton("go","Initialize Network",width="100%")),   
    
    
    ## To Do: Export network file
    ## Display network
    
    
    # Main panel for displaying outputs
    mainPanel(
        #div(style ="text-align:center;",
        #    h2("Network Display")
        #),
        #br(),
         div(style = "height:650px; width = 650px; outline: 1px solid black; overflow: hidden; margin-right: 10px; margin-left: 10px;", 
            visNetworkOutput("networkplot", height = "100%")
        ), 
        div(style = "float:right; padding-top:10px; padding-right:10px",
            downloadButton('downloadNetwork', 'Export network as HTML')
        ),
        plotOutput("networklegend", width = "100%", height = "100px"),
      #  br(),br(),
        div(style = "display: block; padding-top: 5em; margin-left: auto; margin-right: auto;width:75%;",
            DT::dataTableOutput("nodeTable")
        )
    )
  )
)
