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




dashboardPage(skin="green",
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
     tabBox(width = 10, height = "250px",
            title = "",
            # The id lets us use input$elminselection on the server to find the current tab
            tabPanel("Network Preferences",
                        list(
                            fluidRow(
                                column(4,  
                                    #selectInput("elements_of_interest", tags$b("Select the element(s) whose mineral network you'd like to analyze"),
                                    #            c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                    #            multiple=TRUE),
                                    pickerInput("elements_of_interest", tags$b("Select element(s):"),
                                                    choices = c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                                    options = list(
                                                        `actions-box` = TRUE, 
                                                        size = 10
                                                    ), 
                                                    multiple = TRUE
                                                ),
                                    checkboxInput("force_all_elements",tags$b("Force element intersection?"),value = FALSE),
                                    sliderInput("age_limit", tags$b("Choose an age (Ga) for the youngest minerals:"), min = 0, max = 4.5, step = 0.1, value = 0)
                                ),
                                column(4, 
                                    selectInput("network_layout", tags$b("Network layout algorithm:"),
                                        c("Kamada-Kawai"         = "layout_with_kk",
                                          "Fruchterman Reingold" =  "layout_with_fr",
                                          "Circle"               = "layout_in_circle")
                                        ),
                                    br(),br(),
                                    numericInput("selected_degree", tags$b("Network degree highlighting:"), 2, min = 1, max = 5, step = 1)   
                                ),
                                column(4,
                                 br(),
                                 actionBttn("go", "Initialize Network", color = "danger", style = "bordered", block=TRUE)
                                    # style - simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite.
                                    # color - default, primary, warning, danger, success, royal.
                                )
                            )
                        ) 
                    ),
            tabPanel("Node Colors", 
                        checkboxInput("color_by_cluster",tags$b("Color all nodes by network cluster?"),value = FALSE),
                        list(
                            fluidRow(
                                column(6,checkboxInput("highlight_element",tags$b("Highlight element(s) of interest?"),value = FALSE)
                                    ),
                                column(6, conditionalPanel(condition = "input.highlight_element == true",   
                                            {colourpicker::colourInput("highlight_color", tags$b("Select highlight color:"), value = "lightgoldenrod1")})
                                      )
                            ) ## fluidRow
                        ), ## list
                        conditionalPanel(condition = "input.color_by_cluster == false", 
                            list(
                                fluidRow(
                                    column(6, 
                                        selectInput("color_element_by", tags$b("Element color scheme:"),
                                                    c("Use a single color for all elements"    = "singlecolor",  
                                                    "Color elements based on network degree" = "network_degree_norm"))),
                                    column(6, 
                                        conditionalPanel(condition = "input.color_element_by == 'singlecolor'",   
                                            {colourpicker::colourInput("element_color", tags$b("Element color:"), value = "skyblue")}
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
                                         selectInput("color_mineral_by", tags$b("Mineral color scheme:"),
                                                         c("Use a single color for all minerals"    = "singlecolor",  
                                                           "Color minerals based on mean redox state"      = "redox",        
                                                           "Color minerals based on maximum age"           = "max_age",      
                                                           "Color minerals based on number of localities"  = "num_localities"))
                                      ),
                                      column(6,
                                          conditionalPanel(condition = "input.color_mineral_by == 'singlecolor'",   
                                              {colourpicker::colourInput("mineral_color", tags$b("Select mineral color:"), value = "firebrick3")}
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
            tabPanel("Node Size", 
                    list(
                        fluidRow(
                            column(6,
                                selectInput("element_size_type", tags$b("Element node size scheme:"), 
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
                                    {sliderInput("size_scale",tags$b("Element size scaler"),value=20,min=10,max=100,step=10)}) 
                            )     
                        )                    
                    ), 

                    list(
                        fluidRow(
                            column(6,
                                selectInput("mineral_size_type", tags$b("Mineral node size scheme:"), 
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
                    )
                ),
                tabPanel("Node Labels", 
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
                    })
                ),
                tabPanel("Node Shapes", 
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
                              {colourpicker::colourInput("edge_color", tags$b("Edge color:"), value = "#5E5E5E")}
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
                sliderInput("edge_weight",tags$b("Weight for edges"),value=3,min=1,max=10)      
            ) ## tabpanel,  
                      
        ) ## tabBox
  ), ## fluidRow
  
  br(),br(),
  fluidRow(
        box(width=10, height="600px",
            div(style = "height:600px; overflow: hidden;", 
                visNetworkOutput("networkplot", height = "90%")
            )
        ),
        box(width=2, plotOutput("networklegend")),
        box(width=2, 
            downloadBttn("downloadNetwork_html", "Save as HTML",  size = "sm", style = "bordered", color = "success"),
            br(),br(),
            downloadBttn("exportNodes", "Save node data", size = "sm", style = "bordered", color = "primary"),
            br(),br(),
            downloadBttn("exportEdges", "Save edge data", size = "sm", style = "bordered", color = "primary")
        ),

        div(style = "display: block; padding-top: 1em; margin-left: auto; margin-right: auto;width:90%;",
            DT::dataTableOutput("nodeTable")
        )
  )## fluidRow
) ## dashboardBody
) ##dashboardPage


