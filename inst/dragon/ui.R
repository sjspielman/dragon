library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(colourpicker)
library(DT)
library(RColorBrewer)
library(tidyverse)
library(broom)
library(magrittr)
library(cowplot)
library(igraph)
library(visNetwork)


source("build_network.R")
source("defs.R")

dashboardPage(skin = "red",
    dashboardHeader(title = "dragon: Deep-time Redox Analysis of the Geobiology Ontology Network", titleWidth = "770px",
        dropdownMenu(
            type = "notifications", 
            icon = icon("question-circle"),
            badgeStatus = NULL,
            headerText = "Information:",
            notificationItem("You are using dragon version 0.1", icon = icon("box-open")),
            notificationItem("Source Code", icon = icon("github"), href = "http://github.com/spielmanlab/dragon"),
            notificationItem("IMA Database of Mineral Properties", icon = icon("globe"), href =  "http://rruff.info/ima/")
        )),
  dashboardSidebar(width = 350,
    sidebarMenu(id = "thismusttakeanidapparently",
        chooseSliderSkin(skin = "Flat"),
            
            pickerInput("elements_of_interest", tags$span(style="font-weight:400", "Select focal element(s):"),
                                                    #choices = c("Ag" = "Ag", "Al" = "Al", "As" = "As", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bi" = "Bi", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cl" = "Cl", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Dy" = "Dy", "Er" = "Er", "F" = "F", "Fe" = "Fe", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "Hf" = "Hf", "Hg" = "Hg", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "La" = "La", "Li" = "Li", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ni" = "Ni", "O" = "O", "Os" = "Os", "P" = "P", "Pb" = "Pb", "Pd" = "Pd", "Pt" = "Pt", "Rb" = "Rb", "Re" = "Re", "REE" = "REE", "Rh" = "Rh", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "U" = "U", "V" = "V", "W" = "W", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr"),
                                                    choices = all_elements,
                                                    options = list(
                                                        `actions-box` = TRUE, 
                                                        size = 6,
                                                        `live-search` = TRUE
                                                    ), 
                                                    multiple = TRUE
                        ),
            tipify(
                prettyCheckbox("elements_by_redox","Use separate nodes for each element redox",value = FALSE, status="danger"),
                title = "Separate element nodes into one per redox state, e.g. rather than one node for Iron (Fe) there may be several nodes such as Fe3+ and Fe2+, etc."
            ), 
            
            tipify(
                prettyCheckbox("force_all_elements","Force element intersection in minerals",value = FALSE, status="danger"),
                title = "When multiple elements are selected, this option ensures that only minerals containing all elements appear in the network."
            ),
            
            tipify( 
                sliderInput("age_limit", "Age (Ga) for the youngest minerals:", min = 0, max = total_max_age, step = 0.1, value = c(0,total_max_age)), 
                title = "Based on mineral discovery dates as recorded in MED"
            ),
            tipify(
                awesomeRadio("max_age_type", "Use maximum or minimum age of minerals", checkbox=TRUE, inline = TRUE, choices = c("Maximum", "Minimum"), selected="Maximum", status="danger"),
                title = "Determines which recorded date (maximum or minimum) is considered for including minerals in the network."
            ),
            tipify(
                prettyCheckbox("build_only","Build network without display",value = FALSE, status="danger"),
                title = "When checked, you will be able to build export the specified network but it will not be displayed. Useful for extremely large networks."
            ),
            fluidRow(
                column(8,
                    tipify(
                            pickerInput("network_layout", tags$span(style="font-weight:400", "Network layout:"),
                                choices = list(
                                   `Force-directed` = c("Fruchterman Reingold" = "layout_with_fr",
                                                      "GEM force-directed"      = "layout_with_gem"),
                                    Other = c("Dynamic physics layout (WARNING: Do not use if photosensitive)" = "physics",
                                              "Sugiyama (bipartite) Layout" = "layout_with_sugiyama",
                                              "Layout in circle"             = "layout_in_circle",
                                               "Layout in sphere"            = "layout_on_sphere")
                                                      
                                )
                            ),
                        title = "Algorithm for rendering the initial state of the interactive network"
                        )
                ),
                column(4,
                        tipify(
                           numericInput("network_layout_seed", tags$span(style="font-weight:400", "Seed:"), min = 0, max = Inf, value = 1),
                           title = "Set the random seed for stochastic (force-directed and dynamic) network layouts here."
                        )            
                )
            ),
            conditionalPanel('input.network_layout == "physics"', {
                #tipify(
                    pickerInput("physics_solver", tags$span(style="font-weight:400", "Solver for physics layout:"),
                                    choices = c("forceAtlas2Based" = "forceAtlas2Based",
                                                "Barnes-Hut" = "barnesHut",
                                                "Repulsion"  = "repulsion", 
                                                "Hierarchical repulsion" = "hierarchicalRepulsion"), selected = "forceAtlas2Based"
                                )#,
                    #title = "Algorithm for initial node positioning with physics layout"
                #)
            }),             
            pickerInput("cluster_algorithm", tags$span(style="font-weight:400", "Network community detection (clustering) algorithm:"),
                choices = c("Louvain",
                            "Leading eigenvector"), selected = "Louvain"
                ),
            br(), 

            actionBttn("go", "Initialize Network", size="sm", color = "danger"),
        br(),
        menuItem(text = "Node Colors",

            fluidRow(
                column(7, 
                    pickerInput("color_element_by", "Color elements based on:",
                                 c("Single color"            = "singlecolor",  
                                   "Degree centrality" = "network_degree_norm",
                                  # "Color by closeness centrality" = "closeness",  ## causes all kinds of problems for disconnected graphs
                                   "Redox state in network"  = "element_redox_network",
                                   "HSAB theory" = "element_hsab",
                                   "Electronegativity" = "pauling", 
                                   "Number of localities (based on mineral discovery)" = "num_localities",
                                   "Atomic mass" = "AtomicMass",
                                   "Number of protons" = "NumberofProtons",
                                   "Periodic Table Group"       = "TableGroup", 
                                   "Periodic Table Period"       = "TablePeriod", 
                                  # "Metal type"    = "MetalType", ## legend bad
                                   "Density"       = "Density",
                                   "Specific Heat"  = "SpecificHeat"))   
                                   
                         
                ),
                column(5, 
                    conditionalPanel(condition = "input.color_element_by == 'singlecolor'",   
                        {colourpicker::colourInput("element_color", "Color:", value = "skyblue")}
                    ),      
                    conditionalPanel(condition = "input.color_element_by != 'singlecolor' & input.color_element_by != 'element_hsab'",   
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
                     pickerInput("color_mineral_by", "Color minerals based on:",
                                     c("Single color"    = "singlecolor",  
                                       "Maximum known age"           = "max_age",      
                                       "Number of known localities"  = "num_localities",
                                       "Mean electronegativity" = "mean_pauling", 
                                       #"Color by std dev Pauling electronegativity" = "sd_pauling",
                                       "COV electronegativity" = "cov_pauling"))
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
            prettyCheckbox("color_by_cluster","Color by Community Cluster",value = FALSE, status="danger",icon = icon("check")),
            br()
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
                column(6, pickerInput("element_size_type", "Size element nodes based on:", 
                                     c("Single size" = "singlesize",
                                       "Degree centrality" = "network_degree_norm", 
                                       "Number of localities (based on mineral discovery)" = "num_localities",
                                       "Atomic mass" = "AtomicMass",
                                       "Number of protons" = "NumberofProtons",
                                       "Density"       = "Density",
                                       "Specific Heat"  = "SpecificHeat"), selected = "singlesize")
                    ),
                column(6, conditionalPanel(condition = "input.element_size_type == 'singlesize'", 
                            {sliderInput("element_label_size","Element size",value=50,min=10,max=100, step=10)}) #### !!!!!!! label size!!!!!!!
                    ),
                column(6, conditionalPanel(condition = "input.element_size_type != 'singlesize'", 
                            {sliderInput("element_size_scale","Scale element size",value=20,min=10,max=100,step=10)}) 
                )
            ),                    

            fluidRow(
                    column(6, pickerInput("mineral_size_type", "Size mineral nodes based on:", 
                             c("Single size" = "singlesize",
                               "Maximum known age"           = "max_age",      
                               "Number of known localities"  = "num_localities"
                               ), selected = "singlesize")
                    ),
                    column(6,       
                        conditionalPanel(condition = "input.mineral_size_type == 'singlesize'", 
                            {sliderInput("mineral_size","Mineral size",value=10,min=0,max=50, step = 5)})
                    ),
                    column(6, conditionalPanel(condition = "input.mineral_size_type != 'singlesize'", 
                            {sliderInput("mineral_size_scale","Scale mineral size",value=10,min=1,max=25,step=1)}) 
                )
            )
        ),
        menuItem(text = "Node Labels and Font", 
            fluidRow(
                column(6, colourpicker::colourInput("element_label_color","Element font color",value = "#000000"))
            ),
            #fluidRow(
            #    column(12, prettyCheckbox("only_use_element_label_color", "Always use the above color for element labels", value = FALSE, animation="smooth", icon = icon("check"), status="danger"))
            #),
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
                   pickerInput("color_edge_by", "Color network edges by:",
                                           c("Single color" = "singlecolor",  
                                             "Element redox state in network" = "element_redox_network",
                                             "Element redox state in mineral" = "element_redox_mineral",
                                             "Number of known mineral localities" = "num_localities_mineral"))
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
        ), # menuitem
        menuItem("Other network options",
            sliderInput("selected_degree", "Node selection highlight degree", min=1, max=5, value = 2, step=1),
            switchInput("hover","Emphasize on hover",value = TRUE, size="normal", labelWidth = "200px", onStatus = "success", offStatus = "danger"),
            switchInput("hide_edges_on_drag","Hide edges when dragging nodes",value = TRUE, size="normal",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
            switchInput(inputId = "drag_view", "Drag network in frame",value = TRUE, size="normal",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
            switchInput("zoom_view","Scroll in network frame to zoom",value = TRUE, size="normal",labelWidth = "200px", onStatus = "success", offStatus = "danger"),
            switchInput("nav_buttons","Show navigation buttons",value = FALSE, size="normal",labelWidth = "200px", onStatus = "success", offStatus = "danger")
        ) 
            
    )),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
          fluidRow(
                bsAlert("alert"),          
                div(style = "margin-right:1%; margin-left:1%;",
                                    
                    tabBox(width=12, 
                        tabPanel("Visualize Network",      
                            div(style = "height:650px; overflow: hidden;", 
                                div(style = "float:left;font-weight:bold;", 


                                    div(style = "font-style:italic;",
                                        textOutput("connectivity")
                                    ),
                                    textOutput("modularity"),
                                    textOutput("n_element_nodes"),
                                    textOutput("n_mineral_nodes"),
                                    textOutput("n_edges")
                                ),
                                conditionalPanel('input.build_only == true', {
                                    div(style = "text-align:center; font-weight:bold; color:red; font-size:1.25em;",
                                        br(),br(),br(),br(),
                                        textOutput("no_network_display")
                                    )
                                }),
                                visNetworkOutput("networkplot", height = "90%")

                            ),
                            conditionalPanel('input.build_only == false', {
                            div(style = "height:80px;",
                                plotOutput("networklegend", height = "80%", width = "100%")
                            )})
                        ),  ## tabPanel
                        
                        
                        tabPanel("Network Information",
                            fluidRow(
                                column(width = 12,
                                    div(style = "font-size:85%;", DT::dataTableOutput("networkTable"))
                                )
                            ),
                            br(),
                            fluidRow(
                                column(width = 2, offset = 10,
                                    downloadBttn("download_networkTable", "Download network information", size = "xs", style = "bordered", color = "danger")
                                )
                            )
                        ),


                        tabPanel("Analyze Network",
                            helpText("In this tab, you can construct a linear regression model to analyze properties of minerals in the specified network."),
                            bsAlert("lm_alert"),
                            fluidRow(
                                column(4, 
                                    pickerInput("response", tags$b("Select the response (dependent) variable:"), 
                                        choices = model_response_choices, selected=max_age_str
                                    )
                                    #actionBttn("gomodel", "Update linear model", size="sm", color = "danger"),
                                    #br(),br(),br(),
                                    #span(textOutput("model_sanity"), style="color:red;font-weight:bold;font-size:1.25em;"),
                                    #span(textOutput("model_sanity_n"), style="color:red;font-weight:bold;font-size:1.25em;"),
                                    #br()
                                ),
                                column(4,

                                    pickerInput("predictor", tags$b("Select the predictor (independent) variable:"), 
                                        choices = model_predictor_choices, selected=cov_pauling_str
                                    ),
                                    conditionalPanel('input.predictor == "Community cluster"',{ 
                                        textOutput("cluster_fyi")
                                    })
                                )
                            ), ## fluidrow
                            fluidRow(
                                column(7,
                                    DT::dataTableOutput("fitted_model"),
                                    br(),br(),
                                    conditionalPanel( condition = 'input.predictor == "Community cluster"', {
                                        DT::dataTableOutput("fitted_tukey")
                                    })
                                ),
                                column(5,
                                    plotOutput("fitted_model_plot"),
                                    #conditionalPanel('input.predictor != "Community cluster"',{ 
                                    #    uiOutput("fitted_model_plot_preferences")
                                    #}),
                                    p(tags$b("Plot styling options (except for community cluster analysis):")),                      
                                    prettyCheckbox("logx", "Use log scale on X-axis", value = FALSE, status="danger", animation="smooth", icon = icon("check")),
                                    prettyCheckbox("logy", "Use log scale on Y-axis", value = FALSE, status="danger", animation="smooth", icon = icon("check")),
                                    prettyCheckbox("bestfit", "Show regression line (with 95% confidence interval).", value = FALSE, status="danger", animation="smooth", icon = icon("check")),
                                    fluidRow(
                                        column(6, colourpicker::colourInput("point_color", "Color for points", value = "black")),
                                        column(6, colourpicker::colourInput("bestfit_color", "Color for regression line", value = "blue"))
                                    ),
                                    div(style="display:inline-block; float:right;",
                                        downloadBttn("download_model_plot", "Download Plot", size = "sm", style = "minimal", color = "danger")
                                    )                                
                                )
                            )
                        ), ## tabPanel    
                        tabPanel("Timeline View", id = "timeline", 
                            div(style = "height:700px;", 
                                
                                div(style="display:inline-block; float:right; height:15%; margin-top:30px;",
                                    downloadBttn("download_timeline_plot", "Download Plot", size = "sm", style = "minimal", color = "danger")
                                ), 
                                div(style = "display:inline-block; float:right; height:15%; width:260px; margin:10px;",
                                    colourpicker::colourInput("timeline_color_notselected", "Color of minerals outside selected age range", value = "chocolate4")
                                ),
                                div(style = "display:inline-block; float:right; height:15%; width:260px; margin:10px;",
                                    colourpicker::colourInput("timeline_color_selected", "Color of minerals within selected age range", value = "peru")
                                ),                                    
                                plotOutput("timeline", width = "100%", height = "80%")
                                                           
                            )
                        )  ## tabPanel    
                          
                    ), # tabBox
                    
                    br(),br(),br(),
                    conditionalPanel('input.build_only == false', {
                        fluidRow(
                            column(width = 12,
                                uiOutput("show_nodeTable")
                            )
                        )
                    }),
                    box(width = 12,status = "primary", title = "Network Export",
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
                                downloadBttn("download_legend", "Download Legend as PNG", size = "sm", style = "minimal", color = "danger")                              
                            )
                        )
                    )

                    
                     

                ) # outer div
          )## fluidRow
    ) ## dashboardBody
) ##dashboardPage


