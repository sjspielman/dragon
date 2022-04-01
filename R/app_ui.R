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
                     notificationItem("You are using THE DEVELOPMENET VERSION OF DRAGON", icon = icon("box-open")),
                     notificationItem("Source Code", icon = icon("github"), href = "http://github.com/spielmanlab/dragon"),
                     notificationItem("IMA Database of Mineral Properties", icon = icon("globe"), href =  "https://rruff.info/ima/")
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
            shinyWidgets::pickerInput("focal_from_mineral","Select focal elements based on mineral composition:",
                                        # TODO: THIS SHOULD USE UPDATED MED DATA WHEN SPECIFIED - has to be a renderUI 
                                        # TODO: SHOW FORMULAS ALSO?
                                        choices = sort(unique(med_data_cache$mineral_name)),
                                        choicesOpt = list(
                                          content = sprintf("<span>%s </span>", mineral_names_formulas()) 
                                        ),
                                        #choices = sort(unique(med_data_cache$mineral_name)),
                                        options = list(
                                          `actions-box` = TRUE, 
                                          size = 8,
                                          `live-search` = TRUE
                                        ), 
                                        multiple = TRUE
            ),
            shinyWidgets::sliderTextInput("age_range","Age (Ga) range of minerals:", choices = rev(seq(0, max(med_data_cache$max_age), 0.1)), grid=T, selected=c(max(med_data_cache$max_age),0)),
            shinyWidgets::prettyRadioButtons("max_age_type", "Use maximum or minimum age of minerals", inline = TRUE, choices = c("Maximum", "Minimum"), selected="Maximum", status="danger"),
            shinyWidgets::prettySwitch("elements_by_redox","Use separate nodes for each element redox",value = FALSE, status="danger"),
            conditionalPanel(condition = "input.elements_by_redox == true",{
              shinyWidgets::prettySwitch("ignore_na_redox","Ignore elements with unknown redox states",value = FALSE, status="danger")
            }),        
            shinyWidgets::prettySwitch("force_all_elements","Force element intersection in minerals",value = FALSE, status="danger"),
            shinyWidgets::prettySwitch("restrict_to_elements","Only consider minerals with focal element(s)",value = FALSE, status="danger"),
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
                  shinyWidgets::pickerInput("network_layout", tags$span(style="font-weight:400", "Network layout:"),
                    choices = network_layout_choices, selected = "layout_with_fr"
                  ), style='padding:0px;' 
                ),
                column(4,
                  numericInput("network_layout_seed", tags$span(style="font-weight:400", "Seed:"), min = 0, max = Inf, value = 1),
                  style='padding-left:0px'
                )
              ), # fluidRow
              fluidRow( 
                column(12,
                    conditionalPanel('input.network_layout == "physics"', {
                        shinyWidgets::pickerInput("physics_solver", tags$span(style="font-weight:400", "Solver for physics layout:"),
                                                  choices = physics_choices, selected = "forceAtlas2Based") 
                    }), style='padding:0px'
                ) # column 12
              ), # fluidrow        
              fluidRow(
                column(12, shinyWidgets::pickerInput("cluster_algorithm", tags$span(style="font-weight:400", "Network community detection (clustering) algorithm:"),
                                                      choices = cluster_algorithm_choices, selected = "Louvain"),
                  style='padding:0px')
              ) ## fluidRow  
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
                                              
              fluidRow(
                column(12,
                  colourpicker::colourInput("na_color", "Color to use for missing or unknown values:", value = default_na_color),
                  style = "padding:0px;"            
                )
              ),  
              fluidRow(
                column(12,
                  shinyWidgets::prettySwitch("color_by_cluster", "Color all nodes by community cluster", value = FALSE, status = "danger"), 
                  style = "padding:0px;"            
                )
              ),  
              fluidRow(
                column(12,
                  conditionalPanel(condition = "input.color_by_cluster == true",{
                    pickerInput("cluster_palette", 
                                label = "Palette:",
                                choices = qual_palettes_ui$name,
                                choicesOpt = list(content = qual_palettes_ui$img)
                    )
                  }), ## END conditionalPanel 
                  style = "padding:0px;"            
                ) ## end column
              )  ## end fluidRow
            ), ## END "Node Colors" menuItem
            
            
            menuItem(text = "Color individual elements",
              fluidRow(
                column(6, shinyWidgets::prettySwitch("highlight_element","Color focal element(s)", value = FALSE,  status = "danger"), style='padding:0px;'),
                column(6, colourpicker::colourInput("highlight_color", "Color:", value = default_highlight_color), style='padding-left:0px;')
              ),
              tags$span("Use the buttons below to add or remove custom groups",
                        br(),
                        "of elements to color."),
              fluidRow(
                column(6, actionButton('insert_custom', 'Add new color group.'), style = "padding:0px;"),
                column(6, actionButton('remove_custom', 'Remove color group.'),  style = "padding-left:0px;")
              ),
              tags$div(id = 'custom_color_chooser')              
            ), ## END "Color individual elements" menuItem
            
            menuItem(text = "Node Sizes",
              fluidRow(
                column(6, 
                  shinyWidgets::pickerInput("element_size_by", 
                                            "Size elements based on:", 
                                            element_size_by_choices, 
                                            selected = "singlesize"),
                  style='padding:0px;'
                ), ## END column                
                column(6, 
                  conditionalPanel(condition = "input.element_size_by == 'singlesize'", {
                    shiny::sliderInput("element_label_size","Element size",value=50,min=10,max=100, step=5) #### !!!!!!! label size - this is how visnetwork does <shrug>!!!!!!!
                  }), style='padding-left:0px;'
                ), ## END column   
                column(6, 
                  conditionalPanel(condition = "input.element_size_by != 'singlesize'", {
                    shiny::sliderInput("element_size_scale","Scale element size",value=20,min=10,max=100,step=5)
                  }), style='padding-left:0px;' 
                ) ## END column   
              ), ## END fluidRow                    
                     
              fluidRow(
                column(6, 
                  shinyWidgets::pickerInput("mineral_size_by", 
                                            "Size minerals based on:", 
                                            mineral_size_by_choices, 
                                            selected = "singlesize"), style='padding:0px;'
                ), ## END column
                column(6,       
                  conditionalPanel(condition = "input.mineral_size_by == 'singlesize'",{ 
                    shiny::sliderInput("mineral_size","Mineral size",value=10,min=0,max=50, step = 5)
                  }), style='padding-left:0px;'
                ), ## END column   
                column(6, 
                  conditionalPanel(condition = "input.mineral_size_by != 'singlesize'", {
                    shiny::sliderInput("mineral_size_scale","Scale mineral size",value=10,min=1,max=25,step=1)
                  }), style='padding-left:0px;'
                ) ## END column   
              ) ## END fluidRow
            ), ## END "Node Sizes" menuItem

            menuItem(text = "Node Shapes", 
              fluidRow(
                column(6, shinyWidgets::pickerInput("element_shape", "Element node shape:", element_shape_choices, selected = default_element_shape), style='padding:0px;'),
                column(6, shinyWidgets::pickerInput("mineral_shape", "Mineral node shape:", mineral_shape_choices, selected = default_mineral_shape), style='padding-left:0px;')
              ) ## END fluidRow
            ), ## END "Node Shapes" menuItem
                        
            menuItem(text = "Node Labels and Font", 
              fluidRow(
                column(12, colourpicker::colourInput("element_label_color", "Element font color (applies only when element shape is not 'text'):", value = default_element_label_color),
                      style='padding:0px;'),
              ),  ## END fluidRow
              fluidRow( 
                column(6, colourpicker::colourInput("mineral_label_color", "Mineral font color:", value = default_mineral_label_color),
                       style='padding:0px;'),
                column(6, shiny::sliderInput("mineral_label_size","Mineral font size",value=0,min=0,max=50, step = 5),
                       style='padding-left:0px;')
              ) ## END fluidRow
            ), ## END "Node Labels and Font" menuItem
                       
            
            menuItem(text = "Edge Attributes", 
              mod_ui_choose_color_sd_palette("mod_edge_colors", 
                                             "Color edges based on:",
                                              edge_color_by_choices,
                                              "singlecolor",
                                              default_edge_color,
                                              default_edge_palette),
                #div(style = "margin-left:15px;margin-right:15px;",
                  tags$span("Visit the 'Node Colors' menu tab to select the color used",
                          br(),
                          "for missing or unknown edge attribute values."),
                #),
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
                    div(style = "font-style:italic;",       ## div2
                      shiny::textOutput("connectivity")
                    ), ## END div2
                    conditionalPanel('input.build_only == true', {
                      div(style = "text-align:center; font-weight:bold; color:red; font-size:1.25em;",
                        br(),br(),br(),br(),
                        shiny::textOutput("no_network_display")
                      )
                    }), ## END `true` conditionalPanel
                    
                    visNetwork::visNetworkOutput("networkplot", height = "100%") #NOTE: this cannot be in a conditionalPanel; div1 gets ignored
                  ), ## END div1
                   
                  conditionalPanel('input.build_only == false', {
                    div(style = "height:75px;padding-left:20px;",
                      shiny::plotOutput("networklegend", height = "95%", width = "100%")
                    )
                  }) ## END 2nd `false` conditionalPanel
                ), ## END "Visualize Network" tabPanel
                 
                 
                ## NETWORK INFORMATION PANEL ---------------------------------------------------------------------------------------------
                shiny::tabPanel("Explore Network Attributes",
                  fluidRow(
                    column(width = 12,
                      div(style = "float:left;font-weight:bold;",
                        h3("Network contents:"),
                        textOutput("modularity"),
                        textOutput("n_element_nodes"),
                        textOutput("n_mineral_nodes"),
                        textOutput("n_edges")
                      )
                    ), ## END column
                    br(),
                    column(width = 12,
                      h3("Explore Element Attributes:"),
                      div(style="display:inline-block;vertical-align:top;",
                        downloadButton("export_element_table", label = "Export table"),
                        shinyWidgets::radioGroupButtons("export_element_table_fmt", 
                                                        "", 
                                                        choices = c("CSV", "Excel"),
                                                        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                                        selected = "CSV")
                      ),
                      div(style = "font-size:85%;", 
                        DT::dataTableOutput("element_exploration_table"),
                        shiny::sliderInput("element_table_digits", "Choose the number of digits to show in table:", value = 3, min = 1, max = 16, width = "275px")
                      ),
                      br(), br(),
                      h3("Explore Mineral Attributes:"),
                      div(style="display:inline-block;vertical-align:top;",
                        downloadButton("export_mineral_table", label = "Export table"),
                        shinyWidgets::radioGroupButtons("export_mineral_table_fmt", 
                                                        "", 
                                                        choices = c("CSV", "Excel"),
                                                        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                                        selected = "CSV")
                      ),
                      div(style = "font-size:85%;", 
                        DT::dataTableOutput("mineral_exploration_table"),
                        shiny::sliderInput("mineral_table_digits", "Choose the number of digits to show in table:", value = 3, min = 1, max = 16, width = "275px"),
                        
                      )
                    ) ## END column
                  ) ## END fluidRow
                ), ## END "Network Information" tabPanel
                 
                 
                ## ANALYZE NETWORK PANEL ---------------------------------------------------------------------------------------------                   
                shiny::tabPanel("Analyze Network Minerals",
                  #helpText("In this tab, you can construct a linear regression model to analyze properties of minerals in the specified network."),
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    column(4, 
                      shinyWidgets::pickerInput("response", 
                                                 tags$b("Select the response (dependent) variable:"), 
                                                 choices = model_response_choices, 
                                                 selected=max_age_str
                      ),
                      shinyWidgets::pickerInput("predictor", 
                                  tags$b("Select the predictor (independent) variable:"), 
                                  choices = model_predictor_choices, 
                                  selected=cov_pauling_str
                      ),
                      conditionalPanel('input.predictor == "Community cluster"',{ 
                        shiny::textOutput("cluster_fyi")
                      })
                    ), ## END column
                    column(8,
                      DT::dataTableOutput("fitted_model"),
                      br(),br(),
                      conditionalPanel( condition = 'input.predictor == "Community cluster"', {
                        DT::dataTableOutput("fitted_tukey")
                      })
                    ) ## END column
                  ), ## END fluidRow
                  fluidRow(
                    column(4,
                      p(tags$b("Plot styling options:")),                      
                      shiny::uiOutput("model_plot_options")
                    ), ## END column
                    column(8,
                      div(style="float:center;width:600px;height:400px;",
                        shiny::plotOutput("fitted_model_plot", height = "100%", width = "100%")
                      ),
                      div(style="display:inline-block; float:right;",
                        shinyWidgets::downloadBttn("download_model_plot", "Download Plot", size = "sm", style = "minimal", color = "danger")
                      ) ## END div                                
                    ) ## END column
                  ) ## END fluidRow
                 ), ## END "Analyze Network Minerals" tabPanel  
                
                ## TIMELINE PANEL ---------------------------------------------------------------------------------------------                   
                shiny::tabPanel("Mineral formation timeline",
                  div(style="float:center;width:100%;height:700px;",
                    plotOutput("timeline_plot_output", height = "100%", width = "100%")
                  ),
                  br(),
                  shinyWidgets::prettySwitch("timeline_view","Display minerals discovered at their oldest known age only. Turn off to display minerals discovered at any age.", value = TRUE, status="danger"),
                  fluidRow(
                    ## can't use the module here due to sizing 
                    column(4,
                      div(style="display:inline-block;vertical-align:top;",
                        shinyWidgets::pickerInput("color_timeline_by", 
                                                  "Color minerals inside the selected age range based on:", 
                                                  mineral_timeline_color_by_choices, 
                                                  selected = "singlecolor", width = "100%")
                      ),
                      div(style="display:inline-block;vertical-align:top;",
                        conditionalPanel(condition = "input.color_timeline_by == 'singlecolor'", 
                        {
                            colourpicker::colourInput("timeline_color", "Color:", value = "#68340e")
                        }),
                        conditionalPanel(condition = "input.color_timeline_by != 'singlecolor'", 
                        {
                            shinyWidgets::pickerInput("timeline_palette", 
                                                        label = "Mineral color palette:",
                                                        choices = sd_palettes_ui$name,
                                                        options = list( size = 6),
                                                        choicesOpt = list(content = sd_palettes_ui$img),
                                                        selected = "YlOrBr", width = "100%")
                        })
                      )
                    ), 
                    column(4, 
                      colourpicker::colourInput("outside_range_color", label = "Color for minerals outside the selected age range:", value="#fae9dd")
                    ),
                    column(4, 
                      br(),
                      shinyWidgets::downloadBttn("download_timeline", "Download Timeline Plot", size = "md", style = "minimal", color = "danger")
                    ) ## END column 3
                  ) ## END fluidRow
                ) ## END timeline tabPanel
              ), ## END TOP tabBox
                
                
              ## RENDER SELECTED NODE TABLE BELOW NETWORK ---------------------------------------------------------
              br(),br(),br(),
              shiny::uiOutput("show_nodeTable"),
              br(),
             
              
              ## NETWORK EXPORT BOX -----------------------------------------------------------------------
              box(width = 12, status = "primary", title = "Network Export", collapsible = TRUE,
                shinyWidgets::actionBttn("store_position", 
                                   "Click to prepare network for export to PDF.", 
                                   color = "danger", 
                                   style = "fill", 
                                   block = TRUE),      
                br(),br(),
                fluidRow(
                  column(3, br(), shinyWidgets::downloadBttn("export_network_pdf", "Export network as PDF", size = "sm", style = "minimal", color = "danger")),
                  column(3, shiny::sliderInput("baseline_output_element_size", "Scale network PDF element node size", min = 0.1, max = 5, step = 0.1, value = 1)),
                  column(3, shiny::sliderInput("baseline_output_element_label_size", "Scale network PDF element label size", min = 0.1, max = 5, step = 0.1, value = 1)),
                  column(3, shiny::sliderInput("baseline_output_mineral_size", "Scale network PDF mineral node size", min = 0.1, max = 5, step = 0.1, value = 1))
                ), ## END fluidRow
                br(),br(),br(),
                fluidRow(
                  column(3, shinyWidgets::downloadBttn("export_nodes_csv", "Export nodes as CSV", size = "sm", style = "minimal", color = "danger")),
                  column(3, shinyWidgets::downloadBttn("export_edges_csv", "Export edges as CSV", size = "sm", style = "minimal", color = "danger")),
                  column(3, shinyWidgets::downloadBttn("export_network_igraph", "Export network as text file", size = "sm", style = "minimal", color = "danger")),
                  column(3, shinyWidgets::pickerInput("igraph_output_format", label = "Choose network text file format:", choices = igraph_output_format_choices))
                ), ## END fluidRow
                br(),br(),
                shinyWidgets::downloadBttn("export_legend_pdf", "Export legend as PDF", size = "sm", style = "minimal", color = "danger")
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

