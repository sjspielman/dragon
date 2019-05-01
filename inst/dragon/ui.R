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




dashboardPage(skin = "red",
    dashboardHeader(title = "dragon: Deep-time Redox Analysis of the Geobiology Ontology Network", titleWidth = "770px",
        dropdownMenu(
            type = "notifications", 
            icon = icon("question-circle"),
            badgeStatus = NULL,
            headerText = "Information:",
            notificationItem("Source Code", icon = icon("github"), href = "http://github.com/spielmanlab/dragon"),
            notificationItem("Mineral Evolution Database", icon = icon("globe"), href =  "http://rruff.info/ima/")
        )),
  dashboardSidebar(width = 350,
    sidebarMenu(
        chooseSliderSkin(skin = "Flat"),
            pickerInput("elements_of_interest", tags$span(style="font-weight:400", "Select focal element(s):"),
                                                    choices = c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                                    options = list(
                                                        `actions-box` = TRUE, 
                                                        size = 4
                                                    ), 
                                                    multiple = TRUE
                        ),
            prettyCheckbox("elements_by_redox","Use separate nodes for each element redox",value = FALSE, status="danger", animation="smooth", icon = icon("check")),
            prettyCheckbox("force_all_elements","Force element intersection in minerals",value = FALSE, status="danger", animation="smooth", icon = icon("check")),
            sliderInput("age_limit", "Age (Ga) for the youngest minerals:", min = 0, max = 4.5, step = 0.1, value = 0), #   
            #checkboxInput("refresh_rruff","Refresh rruff data",value = FALSE), ## Eventually we want an option to requery their server and get latest and greatest. This does slow things down, however.

            br(), 
            actionBttn("go", "Initialize Network", size="sm", color = "danger"),
        br(),
        menuItem(text = "Node Colors",

            fluidRow(
                column(7, 
                    pickerInput("color_element_by", "Element color scheme:",
                                 c("Single color"            = "singlecolor",  
                                   "Color by network degree" = "network_degree_norm",
                                   "Color by redox state"    = "redox",
                                   "Color by Pauling electronegativity" = "pauling"))                            
                ),
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
                     pickerInput("color_mineral_by", "Mineral color scheme:",
                                     c("Single color"    = "singlecolor",  
                                       "Color by maximum age"           = "max_age",      
                                       "Color by number of localities"  = "num_localities",
                                       "Color by mean Pauling electronegativity" = "mean_pauling", 
                                       "Color by std dev Pauling electronegativity" = "sd_pauling"))
                ),
                column(5,
                    conditionalPanel(condition = "input.color_mineral_by == 'singlecolor'",   
                        {colourpicker::colourInput("mineral_color", "Color:", value = "firebrick3")}
                    ),
                    conditionalPanel(condition = "input.color_mineral_by != 'singlecolor'",   
                        {pickerInput("mineralpalette", label = "Palette:",
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
            prettyCheckbox("color_by_cluster","Color by network cluster",value = FALSE, status="danger"),br()
            ),
        menuItem(text = "Color individual elements",
            fluidRow(
                column(7,prettyCheckbox("highlight_element","Highlight focal element(s)",value = FALSE, icon = icon("check"), animation="smooth", status="danger")
                    ),
                column(5, colourpicker::colourInput("highlight_color", "Color:", value = "lightgoldenrod1")
                    )
            ),
            fluidRow(
                column(7,uiOutput("choose_custom_elements_color")),                
                column(5, colourpicker::colourInput("custom_selection_color", "Color:", value = "chartreuse3"))    
            )), 
        menuItem(text = "Node Sizes",
            fluidRow(
                column(6, pickerInput("element_size_type", "Element node size:", 
                                     c("Single size" = "singlesize",
                                       "Size by network degree" = "network_degree_norm"), selected = "singlesize")
                    ),
                column(6, conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                            {sliderInput("element_label_size","Size",value=50,min=10,max=100, step=10)}) #### !!!!!!! label size!!!!!!!
                    ),
                column(6, conditionalPanel(condition = "input.element_size_type != 'singlesize'", 
                            {sliderInput("size_scale","Scale size",value=20,min=10,max=100,step=10)}) 
                )
            ),                    

            fluidRow(
                    column(6, pickerInput("mineral_size_type", "Mineral node size:", 
                             c("Single size" = "singlesize",
                               "Size by maximum age"           = "max_age",      
                               "Size by number of localities"  = "num_localities"
                               ), selected = "singlesize")
                    ),
                    column(6,       
                        conditionalPanel(condition = "input.mineral_size_type == 'singlesize'", 
                            {sliderInput("mineral_size","Size",value=10,min=0,max=50, step = 5)})
                    )
            )
        ),
        menuItem(text = "Node Labels and Font", 
            fluidRow(
                column(6, colourpicker::colourInput("element_label_color","Element font color",value = "#000000"))
            ),
            fluidRow(
                column(12, prettyCheckbox("only_use_element_label_color", "Always use the above color for element labels", value = FALSE, animation="smooth", icon = icon("check"), status="danger"))
            ),
            fluidRow( 
                column(6, colourpicker::colourInput("mineral_label_color","Mineral font color",value = "#000000")),
                column(6, sliderInput("mineral_label_size","Mineral font size",value=0,min=0,max=50))
            )
        ),
        menuItem(text = "Node Shapes", 
            fluidRow(
               column(6,
                   pickerInput("element_shape", "Element shape", ## shapes that scale with font
                        c("Circle"     = "circle",
                          "Ellipse"    = "ellipse",
                          "Box"        = "box", 
                          "Text only (no shape)"  = "text"), selected = "Circle" 
                    )
                ),
                column(6, 
                    pickerInput("mineral_shape", "Mineral shape",  ## shapes that DO NOT scale with font
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
                   pickerInput("color_edge_by", "Edge color scheme:",
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
                fluidRow(
                    column(12, sliderInput("edge_weight","Edge weight:",value=3,min=1,max=10))
                )
            ) #,
#         menuItem(text = "Network Interaction Preferences",
#                 numericInput("selected_degree", "Node selection highlight degree", min=1, max=5, 2, width = "270px"),
#                 switchInput(inputId = "drag_view", "Drag network in frame",value = TRUE, size="mini",labelWidth = "300px"),
#                 switchInput("zoom_view","Scroll in network frame to zoom",value = TRUE, size="mini",labelWidth = "300px"),
#                 switchInput("hide_edges_on_drag","Hide edges when dragging nodes",value = TRUE, size="mini",labelWidth = "300px"),
#                 switchInput("nav_buttons","Show navigation buttons",value = TRUE, size="mini",labelWidth = "300px"),
#                 switchInput("hover","Emphasize on hover",value = TRUE, size="mini",labelWidth = "300px"),
#                 switchInput("select_multiple_nodes", "Select multiple nodes at once", value=TRUE, size="mini",labelWidth = "300px")
# 
#             )
        
    )),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
          fluidRow(
                div(style = "margin-right:1%; margin-left:1%;",
                    
                    div(style = "float:left;margin-left:2%;margin-bottom:1%;",
                        dropdownButton(status = "danger", width="300px", circle=FALSE,icon=icon("gear"), tooltip = tooltipOptions(title = "Network interaction preferences"),
                            numericInput("selected_degree", "Node selection highlight degree", min=1, max=5, 2, width = "240px"),
                            switchInput("select_multiple_nodes", "Select multiple nodes at once", value=FALSE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
                            switchInput("hover","Emphasize on hover",value = TRUE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
                            switchInput("hide_edges_on_drag","Hide edges when dragging nodes (more efficient)",value = FALSE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
                            switchInput(inputId = "drag_view", "Drag network in frame",value = TRUE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
                            switchInput("zoom_view","Scroll in network frame to zoom",value = TRUE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
                            switchInput("nav_buttons","Show navigation buttons",value = FALSE, size="mini",labelWidth = "200px", onStatus = "success", offStatus = "danger")
                        )                                   
                    ),
                    
                    
                    
                    
                    tabBox(width=12, 
                        tabPanel("Visualize Network",                      
                            div(style = "height:600px; overflow: hidden;", 
                                visNetworkOutput("networkplot", height = "90%")
                            ),
                            div(style = "height:80px;",
                                plotOutput("networklegend", height = "80%", width = "75%")
                            )
                        ),  ## tabPanel
                        tabPanel("Analyze Network",
                            helpText("In this tab, you can construct a linear regression model to analyze properties of minerals in the specified network."),
                        
                        fluidRow(
                            column(7, 

                                pickerInput("response", tags$b("Select the response (dependent) variable:"), 
                                    choices = c("Maximum known age" ,#           = "max_age",
                                                #"Average redox state" = "redox",
                                                "Mean Pauling electronegativity",# = "mean_pauling", 
                                                "Standard deviation Pauling electronegativity",#  = "sd_pauling", 
                                                "Network degree (normalized)",#   = "network_degree_norm",
                                                "Number of known localities"), selected="Maximum known age",
                                    ),

                                pickerInput("predictor", tags$b("Select the predictor (independent) variable:"), 
                                    choices = c("Maximum known age" ,#           = "max_age",
                                                #"Average redox state" = "redox",
                                                "Mean Pauling electronegativity",# = "mean_pauling", 
                                                "Standard deviation Pauling electronegativity",#  = "sd_pauling", 
                                                "Louvain Cluster",#      = "cluster_ID",
                                                "Network degree (normalized)",#   = "network_degree_norm",
                                                "Number of known localities"), selected="Mean Pauling electronegativity",
                                ),
                                br(),br(),
                                span(textOutput("model_sanity"), style="color:red;font-weight:bold;font-size:1.25em;"),
                                br()
                                
                            ),
                            
                            column(5,
                                p(tags$b("Preferences for plot of model results:")),
                                prettyCheckbox("logx", "Use log scale on X-axis", status="danger", animation="smooth", icon = icon("check")),
                                prettyCheckbox("logy", "Use log scale on Y-axis", status="danger", animation="smooth", icon = icon("check")),
                                prettyCheckbox("bestfit", "Show regression line (with 95% confidence interval).", status="danger", animation="smooth", icon = icon("check")),
                                ## LOL no apologies for the line below.
                                helpText(HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'), "Note that these options are not applicable to Louvain Cluster analysis.")
                            )
                            ), ## fluidrow
                            fluidRow(
                            column(7,
                                DT::dataTableOutput("fitted_model"),
                                br(),br(),
                                conditionalPanel( condition = "input.predictor == 'Louvain Cluster'", {
                                    DT::dataTableOutput("fitted_tukey")
                                })
                            ),
                            column(5,
                                plotOutput("fitted_model_plot"),
                                div(style="display:inline-block; float:right;",downloadBttn("download_model_plot", "Download Plot", size = "sm", style = "minimal", color = "danger"))                                
                            )), br()
                        ) ## tabPanel                 
                    ), # tabBox
                    
                    
                    br(),br(),br(),
                    tabBox(width=12, 
                        tabPanel("Selected Node Information", 
                            DT::dataTableOutput("nodeTable")
                        ),
                        tabPanel("Associated Mineral Localities", 
                            DT::dataTableOutput("localityTable")
                        ), 
                        tabPanel("Node Clustering and Degree", 
                            DT::dataTableOutput("clusterTable")
                        )                       
                    ), # tabBox
                    box(width = 12,status = "primary",
                        fluidRow(
                            column(3,
                                downloadBttn("exportNodes", "Export nodes as CSV", size = "sm", style = "minimal", color = "danger")
                            ),
                            column(3,
                                downloadBttn("exportEdges", "Export edges as CSV", size = "sm", style = "minimal", color = "danger")
                            ),
                            column(3, 
                                downloadBttn("downloadNetwork_html", "Export HTML network", size = "sm", style = "minimal", color = "danger")
                            ),
                            column(3,
                                downloadBttn("download_legend", "Download Legend as PDF", size = "sm", style = "minimal", color = "danger")                              
                            )
                        )
                    )

                    
                     

                ) # outer div
          )## fluidRow
    ) ## dashboardBody
) ##dashboardPage


