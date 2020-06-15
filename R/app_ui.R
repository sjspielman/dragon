#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(skin = "red",
      dashboardHeader(title = "dragon: Deep-time Redox Analysis of the Geobiology Ontology Network", titleWidth = "800px",
        dropdownMenu(type = "notifications", 
                     icon = shiny::icon("question-circle"),
                     badgeStatus = NULL,
                     headerText = "Information:",
                     notificationItem("You are using dragon version 0.3.0", icon = icon("box-open")),
                     notificationItem("Source Code", icon = icon("github"), href = "http://github.com/spielmanlab/dragon"),
                     notificationItem("IMA Database of Mineral Properties", icon = icon("globe"), href =  "http://rruff.info/ima/")
                    )
        ), ## END dashboardHeader
        dashboardSidebar(width = 340,
          sidebarMenu(id = "thismusttakeanidapparently", chooseSliderSkin(skin = "Flat"),
            ## NETWORK DATA SELECTION ---------------------------------------------------------------------------------------------
            shinyWidgets::pickerInput("elements_of_interest", tags$span(style="font-weight:400", "Select focal element(s):"),
                        choices = element_info$element,
                        options = list(
                          `actions-box` = TRUE, 
                          size = 6,
                          `live-search` = TRUE
                        ), 
                        multiple = TRUE
            ), ## END pickerInput
            
              shiny::sliderInput("age_range", "Age (Ga) range of minerals:", min = 0, max = 5, step = 0.1, value = c(0,5)), 
              shinyWidgets::prettyRadioButtons("max_age_type", "Use maximum or minimum age of minerals", inline = TRUE, choices = c("Maximum", "Minimum"), selected="Maximum", status="danger"),
              shinyWidgets::prettySwitch("elements_by_redox","Use separate nodes for each element redox",value = FALSE, status="danger"),
              shinyWidgets::prettySwitch("force_all_elements","Force element intersection in minerals",value = FALSE, status="danger"),
              shinyWidgets::prettySwitch("build_only","Build network without display",value = FALSE, status="danger"),
              fluidRow(
                column(12, align="center",
                       shinyWidgets::actionBttn("go", "Initialize Network", size="md", color = "danger", style = "fill")
                )
              ),
            
            ## NETWORK LAYOUT AND CLUSTERING ---------------------------------------------------------------------------------------------
            menuItem("Network layout and clustering options",
              fluidRow(
                column(8,
                  #shinyBS::tipify(
                    shinyWidgets::pickerInput("network_layout", tags$span(style="font-weight:400", "Network layout:"),
                      choices = network_layout_choices, selected = "layout_with_fr"
                    ) #, # RESTORE COMMA IF RESTORING TIPIFY
                  #  title = "Algorithm for rendering the initial state of the interactive network"
                 # ) ## END tipify
                ), ## END column
                column(4,
                  #shinyBS::tipify(
                    numericInput("network_layout_seed", tags$span(style="font-weight:400", "Seed:"), min = 0, max = Inf, value = 1) #,# RESTORE COMMA IF RESTORING TIPIFY
                  #  title = "Set the random seed for stochastic (force-directed and dynamic) network layouts here."
                 # ) ## END tipify            
                )  ## END column
              ), ## END fluidRow
              conditionalPanel('input.network_layout == "physics"', {
                shinyWidgets::pickerInput("physics_solver", tags$span(style="font-weight:400", "Solver for physics layout:"),
                                            choices = physics_choices, selected = "forceAtlas2Based"
                )
              }), ## END conditionalPanel           
              shinyWidgets::pickerInput("cluster_algorithm", tags$span(style="font-weight:400", "Network community detection (clustering) algorithm:"),
                          choices = cluster_algorithm_choices, selected = "Louvain"
                         ) ## END pickerInput
            ), ## END menuitem
            
            
            
            ## NETWORK VISUAL STYLE MENUS ---------------------------------------------------------------------------------------------
            menuItem(text = "Node Colors",
              mod_ui_choose_color_sd_palette("mod_element_colors", 
                                             "Color elements based on:",
                                              element_color_by_choices,
                                              "singlecolor",
                                              default_element_color,
                                              default_element_palette),
              mod_ui_choose_color_sd_palette("mod_mineral_colors", 
                                             "Color minerals based on:",
                                              mineral_color_by_choices,
                                              "singlecolor",
                                              default_mineral_color,
                                              default_mineral_palette),       
              colourpicker::colourInput("na_color", "Color to use for missing for unknown values:", value = default_na_color),
              shinyWidgets::prettySwitch("color_by_cluster", "Color all nodes by community cluster", value = FALSE, status = "danger"), 
              conditionalPanel(condition = "input.color_by_cluster == true",{
                pickerInput("cluster_palette", 
                            label = "Palette:",
                            choices = qual_palettes_ui$name,
                            choicesOpt = list(content = qual_palettes_ui$img)
                )
              }) ## END conditionalPanel 
            ), ## END "Node Colors" menuItem
            
            
            menuItem(text = "Color individual elements",
              fluidRow(
                column(7, shinyWidgets::prettySwitch("highlight_element","Highlight focal element(s)", value = FALSE,  status = "danger")),
                column(5, colourpicker::colourInput("highlight_color", "Color:", value = default_highlight_color))
              ), ## END fluidRow
              fluidRow(
                column(7, shiny::uiOutput("choose_custom_elements_color")),
                column(5, colourpicker::colourInput("custom_selection_color", "Color:", value = default_selection_color))
              ) ## END fluidRow
            ), ## END "Color individual elements" menuItem
            
            menuItem(text = "Node Sizes",
              fluidRow(
                column(6, 
                  shinyWidgets::pickerInput("element_size_by", 
                                            "Size elements based on:", 
                                            element_size_by_choices, 
                                            selected = "singlesize")
                ), ## END column                
                column(6, 
                  conditionalPanel(condition = "input.element_size_by == 'singlesize'", {
                    shiny::sliderInput("element_label_size","Element size",value=50,min=10,max=100, step=10) #### !!!!!!! label size - this is how visnetwork does <shrug>!!!!!!!
                  })
                ), ## END column   
                column(6, 
                  conditionalPanel(condition = "input.element_size_by != 'singlesize'", {
                    shiny::sliderInput("element_size_scale","Scale element size",value=20,min=10,max=100,step=10)
                  }) 
                ) ## END column   
              ), ## END fluidRow                    
                     
              fluidRow(
                column(6, 
                  shinyWidgets::pickerInput("mineral_size_by", 
                                            "Size minerals based on:", 
                                            mineral_size_by_choices, 
                                            selected = "singlesize")
                ), ## END column
                column(6,       
                  conditionalPanel(condition = "input.mineral_size_by == 'singlesize'",{ 
                    shiny::sliderInput("mineral_size","Mineral size",value=10,min=0,max=50, step = 5)
                  })
                ), ## END column   
                column(6, 
                  conditionalPanel(condition = "input.mineral_size_by != 'singlesize'", {
                    shiny::sliderInput("mineral_size_scale","Scale mineral size",value=10,min=1,max=25,step=1)
                  }) 
                ) ## END column   
              ) ## END fluidRow
            ), ## END "Node Sizes" menuItem

            menuItem(text = "Node Shapes", 
              fluidRow(
                column(6, shinyWidgets::pickerInput("element_shape", "Element node shape:", element_shape_choices, selected = default_element_shape)),
                column(6, shinyWidgets::pickerInput("mineral_shape", "Mineral node shape:", mineral_shape_choices, selected = default_mineral_shape))
              ) ## END fluidRow
            ), ## END "Node Shapes" menuItem
                        
            menuItem(text = "Node Labels and Font", 
              fluidRow(
                column(12, colourpicker::colourInput("element_label_color", "Element font color (applies only when element shape is not 'text'):", value = default_element_label_color)),
              ),  ## END fluidRow
              fluidRow( 
                column(6, colourpicker::colourInput("mineral_label_color", "Mineral font color:", value = default_mineral_label_color)),
                column(6, shiny::sliderInput("mineral_label_size","Mineral font size",value=0,min=0,max=50))
              ) ## END fluidRow
            ), ## END "Node Labels and Font" menuItem
                       
            
            menuItem(text = "Edge Attributes", 
              mod_ui_choose_color_sd_palette("mod_edge_colors", 
                                             "Color edges based on:",
                                              edge_color_by_choices,
                                              "singlecolor",
                                              default_edge_color,
                                              default_edge_palette),
              tags$div(style = "margin-left:5%",
                       tags$p("Visit the 'Node Colors' menu tab to select the color", tags$br(), "used for missing or unknown values.")
              ),
              
              #shiny::helpText("Visit the 'Node Colors' menu tab",
              #                "to select the color used for missing values."),
              sliderInput("edge_weight","Edge weight:",value=3,min=1,max=10)
            ),  ## END "Edge Attributes" menuItem
            
            menuItem("Network interaction options",
                     shiny::sliderInput("selected_degree", "Node selection highlight degree", min=1, max=5, value = 2, step=1),
                     shinyWidgets::prettySwitch("hover","Emphasize on hover",value = TRUE, status = "danger"),
                     shinyWidgets::prettySwitch("hide_edges_on_drag","Hide edges when dragging nodes",value = TRUE, status = "danger"),
                     shinyWidgets::prettySwitch("drag_view", "Drag network in frame",value = TRUE, status = "danger"),
                     shinyWidgets::prettySwitch("zoom_view","Scroll in network frame to zoom", value = TRUE, status = "danger"),
                     shinyWidgets::prettySwitch("nav_buttons","Show navigation buttons", value = FALSE, status = "danger")        
            )#, ## END "Network interaction options" menuItem

          ) ## END sidebarMenu
        ), ## END dashboardSidebar

        dashboardBody(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
          ),
          fluidRow(
            div(style = "margin-right:1%; margin-left:1%;", ## div0                  
                
              ## MAIN TOP TABBOX ---------------------------------------------------------------------------------------------
              tabBox(id = "main-box",width=12, 
                       
                ## VISUALIZE NETWORK PANEL ---------------------------------------------------------------------------------------------
                shiny::tabPanel("Visualize Network",      
                  div(style = "height:700px; overflow: hidden;",  ## div1
                    #div(style = "float:left;font-weight:bold;", ## div2
                      #div(style = "font-style:italic;",       ## div3
                      # shiny::textOutput("connectivity")
                      #), ## END div3
                      #shiny::textOutput("modularity"),
                      #shiny::textOutput("n_element_nodes"),
                      #shiny::textOutput("n_mineral_nodes"),
                      #shiny::textOutput("n_edges")
                    #), ## END div2
                    conditionalPanel('input.build_only == true', {
                      div(style = "text-align:center; font-weight:bold; color:red; font-size:1.25em;",
                        br(),br(),br(),br(),
                        shiny::textOutput("no_network_display")
                      )
                    }), ## END `true` conditionalPanel
                    
                    visNetwork::visNetworkOutput("networkplot", height = "90%") #NOTE: this cannot be in a conditionalPanel; div1 gets ignored
                  ), ## END div1
                   
                  conditionalPanel('input.build_only == false', {
                    div(style = "height:90px;",
                      shiny::plotOutput("networklegend", height = "90%", width = "100%")
                    )
                  }) ## END 2nd `false` conditionalPanel
                ), ## END "Visualize Network" tabPanel
                 
                 
                ## NETWORK INFORMATION PANEL ---------------------------------------------------------------------------------------------
                shiny::tabPanel("Network Information",
                  fluidRow(
                    column(width = 12,
                      div(style = "font-size:85%;", 
                        DT::dataTableOutput("networkTable")
                      )
                    ) ## END column
                  ) ## END fluidRow
                ), ## END "Network Information" tabPanel
                 
                 
                ## ANALYZE NETWORK PANEL ---------------------------------------------------------------------------------------------                   
                shiny::tabPanel("Analyze Network Minerals",
                  #helpText("In this tab, you can construct a linear regression model to analyze properties of minerals in the specified network."),
                  # TODO: put this into boxes?
                  fluidRow(
                    column(4, 
                      shinyWidgets::pickerInput("response", 
                                                 tags$b("Select the response (dependent) variable:"), 
                                                 choices = model_response_choices, 
                                                 selected=max_age_str
                      )
                    ), ## END column
                    column(4,
                      shinyWidgets::pickerInput("predictor", 
                                  tags$b("Select the predictor (independent) variable:"), 
                                  choices = model_predictor_choices, 
                                  selected=cov_pauling_str
                      ),
                      conditionalPanel('input.predictor == "Community cluster"',{ 
                        shiny::textOutput("cluster_fyi")
                      })
                    ) ## END column
                  ), ## END fluidRow
                  
                  fluidRow(
                    column(6,
                      DT::dataTableOutput("fitted_model"),
                      br(),br(),
                      conditionalPanel( condition = 'input.predictor == "Community cluster"', {
                        DT::dataTableOutput("fitted_tukey")
                      })
                    ), ## END column
                    column(6,
                      shiny::plotOutput("fitted_model_plot"),
                      p(tags$b("Plot styling options:")),                      
                      shiny::uiOutput("model_plot_options"),
                      
                      div(style="display:inline-block; float:right;",
                        shinyWidgets::downloadBttn("download_model_plot", "Download Plot", size = "sm", style = "minimal", color = "danger")
                      )                                
                    ) ## END column
                  ) ## END fluidRow
                 ) ## END "Analyze Network Minerals" tabPanel  
                ), ## END TOP tabBox
                
                
                ## CONDITIONAL BUTTON TO SHOW/HIDE NODE TABLE -----------------------------------------------
                #br(),br(),br(),
                #conditionalPanel('input.build_only == false', {
                #  fluidRow(
                #    column(width = 12,
                #      shiny::uiOutput("show_nodeTable")
                #    )
                #  )
                #}),
                
                
                ## NETWORK EXPORT BOX -----------------------------------------------------------------------
                box(width = 12, status = "primary", title = "Network Export", collapsible = TRUE,
                  shinyWidgets::actionBttn("store_position", 
                                     "Click to prepare network for export to PDF.", 
                                     color = "danger", 
                                     style = "fill", 
                                     block = TRUE),            
                  div(style = "float:left;margin-top:20px;",
                    shinyWidgets::dropdownButton(circle =FALSE, up=TRUE, label  = "PDF options", icon = icon("cogs", lib = "font-awesome"), status = "info", width = "250px", size = "default",
                      shiny::numericInput("baseline_output_element_size", "Scale element node size", value = 1, max=5, step = 0.5),
                      shiny::numericInput("baseline_output_element_label_size", "Scale element node label size", value = 1, max=5, step = 0.5),
                      shiny::numericInput("baseline_output_mineral_size", "Scale mineral node size",  value = 1, max=5, step = 0.5),
                      shiny::numericInput("output_pdf_width", "Width of network PDF", min=1, max=20, 10),
                      shiny::numericInput("output_pdf_height", "Height of network PDF", min=1, max=20, 6),
                      shinyWidgets::prettySwitch("output_pdf_node_frame","Show node outlines in PDF?",value = FALSE, status="danger")
                    )
                  ), ## END div
                    
                  br(),br(),br(),br(),
                  
                  fluidRow(
                    column(3, shinyWidgets::downloadBttn("downloadNetwork_pdf", "Export network as PDF", size = "sm", style = "bordered", color = "danger")),
                    column(3, shinyWidgets::downloadBttn("download_legend", "Export legend as PDF", size = "sm", style = "bordered", color = "danger")),                            
                    column(3, shinyWidgets::downloadBttn("exportNodes", "Export nodes as CSV", size = "sm", style = "bordered", color = "danger")),
                    column(3, shinyWidgets::downloadBttn("exportEdges", "Export edges as CSV", size = "sm", style = "bordered", color = "danger"))
                  ) ## END fluidRow
                ) ## END box
                
            ) ## END div0
          ) ## END fluidRow
        ) ## END dashboardBody
    ) ## END dashboardPage
    
  
    
  ) ## END tagList
} ## END app_ui













































































#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    golem::favicon(),
    golem::activate_js(),
    shinyWidgets::useSweetAlert(), ## May not be necessary per docs except for progress bars, but let's use it.
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'dragon'
    ),
    tags$style(".palette-style{ 
                  display: inline;
                  vertical-align: middle;
                  color: black;
                  font-weight: 600;
                  padding-left: 7px;}"
                )
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

