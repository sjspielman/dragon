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
library(shinyjs)

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




dashboardPage(skin="red",
    dashboardHeader(title = tags$span(style="font-weight:500","MCNet: Visualizing Mineral Chemistry Networks using the RRUFF IMA database"), titleWidth = "800px",
        dropdownMenu(
            type = "notifications", 
            icon = icon("question-circle"),
            badgeStatus = NULL,
            headerText = "Information:",
            notificationItem("Source Code", icon = icon("github"), href = "http://github.com/spielmanlab/mcnet"))
        ),
  dashboardSidebar(width = 350,
    sidebarMenu(
            pickerInput("elements_of_interest", tags$span(style="font-weight:400", "Select element(s):"),
                                                    choices = c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                                    options = list(
                                                        `actions-box` = TRUE, 
                                                        size = 10
                                                    ), 
                                                    multiple = TRUE
                        ),
            checkboxInput("force_all_elements","Force element intersection?",value = FALSE),
            sliderInput("age_limit", tags$span(style="font-weight:400","Choose an age (Ga) for the youngest minerals:"), min = 0, max = 4.5, step = 0.1, value = 0), #   

            selectInput("network_layout", "Starting network layout:",
                c("Kamada-Kawai"         = "layout_with_kk",
                  "Fruchterman Reingold" =  "layout_with_fr",
                  "Circle"               = "layout_in_circle")
                ),
            numericInput("selected_degree", tags$b("Network degree highlighting:"), 2, min = 1, max = 5, step = 1),
            br(), 
            actionBttn("go", "Initialize Network", size="sm", color = "danger"),
        br(),
        menuItem(text = "Node Color Selection",
        fluidRow(
            column(7,checkboxInput("highlight_element","Highlight selected elements",value = FALSE)
                ),
            column(5, conditionalPanel(condition = "input.highlight_element == true",   
                        {colourpicker::colourInput("highlight_color", "Color", value = "lightgoldenrod1")
                    })
                )
        ), 
        fluidRow(
            column(7, 
                selectInput("color_element_by", "Element color scheme:",
                            c("Select a single color"    = "singlecolor",  
                            "Color by network degree" = "network_degree_norm"))),
            column(5, 
                conditionalPanel(condition = "input.color_element_by == 'singlecolor'",   
                    {colourpicker::colourInput("element_color", "Color:", value = "skyblue")}
                ),      
                conditionalPanel(condition = "input.color_element_by != 'singlecolor'",   
                    {pickerInput("elementpalette", label = "Palette:",
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
            column(7,
                 selectInput("color_mineral_by", "Mineral color scheme:",
                                 c("Select a single color"    = "singlecolor",  
                                   "Color by mean redox state"      = "redox",        
                                   "Color by maximum age"           = "max_age",      
                                   "Color by number of localities"  = "num_localities"))
            ),
            column(5,
                conditionalPanel(condition = "input.color_mineral_by == 'singlecolor'",   
                    {colourpicker::colourInput("mineral_color", "Color:", value = "firebrick3")}
                ),
                conditionalPanel(condition = "input.color_mineral_by != 'singlecolor'",   
                    {pickerInput("mineralpalette", label = "'Palette:",
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
        ),
        checkboxInput("color_by_cluster","Color by network cluster",value = FALSE),br()
        ),
        menuItem(text = "Node Label Selection",
            fluidRow(
                column(7, selectInput("element_size_type", "Element node size:", 
                                     c("Single size" = "singlesize",
                                       "Size by network degree" = "network_degree_norm"), selected = "singlesize")
                    ),
                column(5, conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                            {sliderInput("element_label_size","Size",value=50,min=10,max=100, step=10)}) #### !!!!!!! label size!!!!!!!
                    ),
                column(5, conditionalPanel(condition = "input.element_size_type != 'singlesize'", 
                            {sliderInput("size_scale","Scale size",value=20,min=10,max=100,step=10)}) 
                )
            ),                    

            fluidRow(
                    column(7, selectInput("mineral_size_type", "Mineral node size:", 
                             c("Single size" = "singlesize",
                               "Size by mean redox state" = "redox",
                               "Size by maximum age"           = "max_age",      
                               "Size by number of localities"  = "num_localities"
                               ), selected = "singlesize")
                    ),
                    column(5,       
                        conditionalPanel(condition = "input.mineral_size_type == 'singlesize'", 
                            {sliderInput("mineral_size","Size",value=10,min=0,max=50, step = 5)})
                    )
            )
        ),
        menuItem(text = "Node Labels", 
            fluidRow(
                column(6, "Element label size is automatically determined by element node size."),
                column(6, colourpicker::colourInput("element_label_color","Element label color",value = "#000000"))
            ),
            fluidRow( 
                column(6, sliderInput("mineral_label_size","Mineral label font size",value=0,min=0,max=100)),
                column(6, colourpicker::colourInput("mineral_label_color",tags$b("Mineral label color"),value = "#000000"))
            )
        ),
        menuItem(text = "Node Shapes", 
            fluidRow(
               column(6,
                   selectInput("element_shape", "Element node shape", ## shapes that scale with font
                        c("Circle"     = "circle",
                          "Ellipse"    = "ellipse",
                          "Box"        = "box", 
                          "Text only (no shape)"  = "text"), selected = "Circle" 
                    )
                ),
                column(6, 
                    selectInput("mineral_shape", "Mineral node shape",  ## shapes that DO NOT scale with font
                                c("Circle"   = "dot", #### !!!!!!
                                  "Square"   = "square",
                                  "Star"     = "star",
                                  "Triangle" = "triangle",
                                  "Diamond"  = "diamond"), selected = "Circle"    
                    )
                )
            )
        ),          
        menuItem(text = "Edge Attributes", 
            fluidRow(
                 column(6,
                   selectInput("color_edge_by", "Edge color scheme:",
                                           c("Single color" = "singlecolor",  
                                             "Color by mean element redox state" = "redox"))
                 ),
                 column(6,
                     conditionalPanel(condition = "input.color_edge_by == 'singlecolor'",   
                         {colourpicker::colourInput("edge_color", "Color:", value = "#5E5E5E")}
                     ),

                     conditionalPanel(condition = "input.color_edge_by != 'singlecolor'",   
                         {pickerInput("edgepalette", label = "Palette:",
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
                ), ## fluid
                sliderInput("edge_weight","Weight for edges",value=3,min=1,max=10) 
            )
        
    )),
    dashboardBody(
          fluidRow(
                box(width=12, height="600px",
                    div(style = "height:600px; overflow: hidden;", 
                        visNetworkOutput("networkplot", height = "90%")
                    )
                ),
                box(width=3, height="80px", align="center",
                    downloadBttn("exportNodes", "Save nodes as CSV", size = "xs", style = "bordered", color = "primary"),
                    downloadBttn("exportEdges", "Save edges as CSV", size = "xs", style = "bordered", color = "primary"),
                    downloadBttn("downloadNetwork_html", "Save as HTML",  size = "xs", style = "bordered", color = "success")
                ),
                #box(id = "hiddenBox", width=5),
                box(width=5, height="80px",
                    div(style = "height:75px;",
                        plotOutput("networklegend", height = "80%", width = "100%")
                    )
                ),
        
                box(width=12, 
                    div(style = "display: block; padding-top: 1em; margin-left: auto; margin-right: auto;width:90%;",
                        DT::dataTableOutput("nodeTable")
                    )
                )
          )## fluidRow
    ) ## dashboardBody
) ##dashboardPage


