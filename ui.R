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


library(shinydashboard)

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




dashboardPage(
  dashboardHeader(title = "MCNet: Visualizing Mineral Chemistry Networks using the rruff database", titleWidth = "700px",
  dropdownMenu(
        type = "notifications", 
        icon = icon("question-circle"),
        badgeStatus = NULL,
        headerText = "Information:",
        
        notificationItem("Source Code", icon = icon("github"),
          href = "http://github.com/spielmanlab/mcnet")
      )
  
  
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
  fluidRow(
     tabBox(width = 8,
            title = "Network Preferences",
            # The id lets us use input$elminselection on the server to find the current tab
            tabPanel("Element and Mineral Selection",
                        list(
                            fluidRow(
                                column(6,  
                                    selectInput("elements_of_interest", tags$b("Select the element(s) whose mineral network you'd like to analyze"),
                                                c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                                multiple=TRUE),
                                    checkboxInput("force_all_elements",tags$b("Every mineral included in the network contains all selected elements."),value = FALSE),
                                    checkboxInput("select_all_elements",tags$b("Select all elements."),value = FALSE)
                                ),
                                column(6, 
                                 selectInput("include_age", tags$b("Choose an eon for minerals to include in the network:"),
                                            c("Include all known minerals"  = "present",
                                            "Paleoproteozoic (>= 1.6 Ga)" = "paleo",
                                            "Archean (>= 2.5 Ga)"         = "archean",
                                            "Hadean (>= 4 Ga)"            = "hadean")
                                            )
                                )
                            )
                        )
                    
                    ),
            tabPanel("Node Colors", 
                        checkboxInput("color_by_cluster",tags$b("Click to color all nodes by network cluster"),value = FALSE),
                        list(
                            fluidRow(
                                column(6,checkboxInput("highlight_my_element",tags$b("Highlight element(s) of interest"),value = FALSE)
                                    ),
                                column(6, conditionalPanel(condition = "input.highlight_my_element == true",   
                                            {colourpicker::colourInput("elementhighlight", tags$b("Select highlight color:"), value = "lightgoldenrod1")})
                                      )
                            ) ## fluidRow
                        ), ## list
                        conditionalPanel(condition = "input.color_by_cluster == false", 
                            list(
                                fluidRow(
                                    column(6, 
                                        selectInput("color_element_by", tags$b("Select a color scheme for elements:"),
                                                    c("Use a single color for all elements"    = "singlecolor",  
                                                    "Color elements based on network degree" = "network_degree_norm"))),
                                    column(6, 
                                        conditionalPanel(condition = "input.color_element_by == 'singlecolor'",   
                                            {colourpicker::colourInput("elementcolor", tags$b("Element color:"), value = "skyblue")}
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
                                )
                            ),
                            list(
                                fluidRow(
                                      column(6,
                                         selectInput("color_mineral_by", tags$b("Select a color scheme for minerals:"),
                                                         c("Use a single color for all minerals"    = "singlecolor",  
                                                           "Color minerals based on mean redox state"      = "redox",        
                                                           "Color minerals based on maximum age"           = "max_age",      
                                                           "Color minerals based on number of localities"  = "num_localities"))
                                      ),
                                      column(6,
                                          conditionalPanel(condition = "input.color_mineral_by == 'singlecolor'",   
                                              {colourpicker::colourInput("mineralcolor", tags$b("Select mineral color:"), value = "firebrick3")}
                                          ),
                                          conditionalPanel(condition = "input.color_mineral_by != 'singlecolor'",   
                                              {pickerInput("mineralpalette", label = tags$b("Mineral palette:"),
                                                choices = divseq.list, selected = "Reds", width = "90%",
                                                choicesOpt = list(
                                                    content = sprintf(
                                                        "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                                                        unname(palette.linear.gradient), palette.label.colors, names(palette.linear.gradient)
                                                    )
                                                )
                                            )}        
                                        ) 
                                    )
                                ) 
                            )
                        )  ## condtional panel if cluster is False
                    ), ## end of element mineral color schemes              
            tabPanel("Node Size and Shape", 
                    list(
                        fluidRow(
                            column(6,
                                selectInput("element_size_type", tags$b("Select a size scheme for element nodes"), 
                                             c("Single size for all element nodes" = "singlesize",
                                               "Size element nodes by network degree" = "network_degree_norm")
                                           ), selected = "singlesize"
                            ),
                            column(6,    
                                conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                                    {sliderInput("element_label_size",tags$b("Element size"),value=50,min=10,max=100, step=10)}) #### !!!!!!! label size!!!!!!!
                            ),
                            column(6,    
                                conditionalPanel(condition = "input.element_size_type != 'singlesize'", 
                                    {sliderInput("size_scale",tags$b("Element size scaler"),value=50,min=10,max=150,step=10)}) 
                            )     
                        )                    
                    ), 

                    list(
                        fluidRow(
                            column(6,
                                selectInput("mineral_size_type", tags$b("Select a size scheme for mineral nodes"), 
                                     c("Single size for all mineral nodes" = "singlesize",
                                       "Size mineral nodes based on mean redox state" = "redox",
                                       "Size mineral nodes based on maximum age"           = "max_age",      
                                       "Size mineral nodes based on number of localities"  = "num_localities"
                                       )
                                   ), selected = "singlesize"
                            ),
                            column(6,       
                                conditionalPanel(condition = "input.mineral_size_type == 'singlesize'", 
                                    {sliderInput("mineral_size",tags$b("Mineral size"),value=10,min=0,max=50, step = 5)})
                            )
                        )
                    ),
                    
                    list(
                        fluidRow(
                            column(6, p("Note that element labels are scaled to element node size and cannot be sized differently.")),
                            column(6, colourpicker::colourInput("element_label_color",tags$b("Element label color"),value = "#000000"))
                        )
                    ),
                    checkboxInput("label_mineral",tags$b("Click to show mineral labels"),value = FALSE),
                    conditionalPanel(condition = "input.label_mineral", {
                        list(    
                            fluidRow( 
                                column(6, sliderInput("mineral_label_size",tags$b("Mineral label font size"),value=10,min=1,max=100)),
                                column(6, colourpicker::colourInput("mineral_label_color",tags$b("Mineral label color"),value = "#000000"))
                            )
                        )
                    }),

                    
                    list(
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
                        )          
                    
                    )
               ), ## tabPanel       
                       
            tabPanel("Edge Attributes", 
                list(
                    fluidRow(
                      column(6,
                        selectInput("color_edge_by", tags$b("Select a color scheme for edges"),
                                                c("Use a single color for all edges" = "singlecolor",  
                                                  "Color edges by mean element redox state" = "redox"))
                        #uiOutput("show_color_edge") 
                      ),
    
                      column(6,
                          conditionalPanel(condition = "input.color_edge_by == 'singlecolor'",   
                              {colourpicker::colourInput("edgecolor", tags$b("Edge color:"), value = "#5E5E5E")}
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
                        )
                    ) ## fluid
                ), ## list
                sliderInput("edge_weight",tags$b("Weight for edges"),value=5,min=1,max=10)      
            ), ## tabpanel,            
            
            tabPanel("Display", 
                list(
                    fluidRow(
                        column(6,
                            selectInput("network_layout", tags$br("Select a layout algorithm for the network:"),
                                c("Kamada-Kawai"         = "layout_with_kk",
                                  "Fruchterman Reingold" =  "layout_with_fr",
                                   "Circle"               = "layout_in_circle")
                            )
                        ),
                        column(6, numericInput("selected_degree", tags$br("Degree for highlighting network node connections:"), 2, min = 1, max = 5, step = 1)    
                        )
                    )
                )        
            )       
                                     
                                     
                

        
        ), ## tabBox
    box(width = 2, status = "primary", actionButton("go","Initialize Network",width="100%"))
  ), ## fluidRow
  
  fluidRow(
  
        div(style = "height:700px; outline: 1px solid black; overflow: hidden; margin-left: auto; margin-right: auto;width:90%;", 
            visNetworkOutput("networkplot", height = "100%")
        ), 
        div(style = "float:right; padding-top:10px; padding-right:10px;  margin-right: 75px;",
            downloadButton('downloadNetwork', 'Export network as HTML')
        ),
        div(style = "display: block; padding: 2em; margin-left: auto; margin-right:auto;width:80%;", 
            plotOutput("networklegend", width = "75%", height = "100px")
        ),
        div(style = "display: block; padding-top: 1em; margin-left: auto; margin-right: auto;width:90%;",
            DT::dataTableOutput("nodeTable")
        )
  )## fluidRow
) ## dashboardBody
) ##dashboardPage


