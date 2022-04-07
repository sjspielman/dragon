#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  ## Define MED reactives ----------------------------------------------------------------
  med <- reactiveVal(list("med_data"           = NULL, 
                          "element_redox_states" = NULL,
                          "cache"                = NULL))
  attempted_download <- reactiveVal(FALSE)
  ## Prompt for MED data to use -------------------------------------------------------------
  observe({
    
    most_recent_date <- find_most_recent_date()
    
    if (most_recent_date != FALSE) { ## If FALSE, curl fail. MUST BE !=
      # SUCCESSFULLY CURLED
      if (most_recent_date != med_cache_date) #!!!!! MUST BE BE != 
      { 
        shinyWidgets::confirmSweetAlert(
          session,
          "use_med_cache",
          title = "Welcome!",
          text = tags$span(style="text-align:justify;display:block", 
            tags$code("dragon"), " generates mineral-chemistry networks using data from the Mineral Evolution Database (MED).",
            tags$code("dragon"), " is using cached MED data released on", med_cache_date,
            ". MED was since updated on", most_recent_date, 
            ". Do you want to use the cached data, or download/update the MED data for this", tags$code("dragon"), "session?",
            br(),br(),
            tags$b("CAUTION: Downloading will take several minutes!")
          ),                                     
          type = "warning",
          btn_labels = c("Update MED data", "Use MED data cache"), # FALSE, TRUE
          btn_colors = c("#FE642E", "#04B404"),
          closeOnClickOutside = FALSE, 
          showCloseButton = FALSE, 
          html = TRUE
        )
      } else {
       # shinyWidgets::sendSweetAlert(
       #   session = session, title = "Welcome to dragon!", type = "info",
       #   text = paste0("We're using the most up to date MED data, which was released on ", med_cache_date, ". dragon is GPL-3 and by clicking this you acknowledge to agree to terms. Please cite DRAGON AND MED in your papers.")
       # )
        med(list("med_data"            = med_data_cache, 
                 "element_redox_states" = element_redox_states_cache,
                 "cache"                = TRUE))
      }
    } else {
      ## this is for most_recent_date == FALSE
     # shinyWidgets::sendSweetAlert(
     #   session = session, title = "Welcome to dragon!", type = "info",
     #   text = paste0("We're MED data, which was released on ", med_cache_date, ". dragon is GPL-3 and by clicking this you acknowledge to agree to terms. Please cite DRAGON AND MED in your papers. YOU ARE NOT CONNECTED TO THE INTERNET!")
     # )
      med(list("med_data"            = med_data_cache, 
               "element_redox_states" = element_redox_states_cache,
               "cache"                = TRUE))
    }
  }) ## END observe
  
    
  observeEvent(input$use_med_cache, {
    ## This observer will only be triggered if there is internet and MED website is responsive.
    if (!(input$use_med_cache))
    {
      shinyWidgets::sendSweetAlert(
          session = session, title = "Downloading now!", type = "info",
          text = tags$span(style="text-align:justify;display:block", 
                    tags$b("Please be aware this process will take 5-15 minutes."), br(),
                    "You will be notified when the download is complete. You can close this message any time."
                  ),
          html = TRUE
      )
      attempted_download(TRUE)
      ## Download from MED
      future::future({
        prepare_med_data()
      }) %...>% med()
    } else {
      med(list("med_data"             = med_data_cache, 
               "element_redox_states" = element_redox_states_cache,
               "cache"                = TRUE))
    }
  })


  observeEvent(med()$cache, {
    if (med()$cache == FALSE)
    {
      shinyWidgets::sendSweetAlert(
          session = session, title = "Download is complete!", type = "success",
          text = "You may now proceed to build your network with the most recent MED data."
        )
    } else {
      # download attempted and cache is TRUE
      if(attempted_download()){
          shinyWidgets::sendSweetAlert(
            session = session, title = "Unable to update data.", type = "warning",
            text = "MED data could not be downloaded either due to your internet connection or MED server status. Using cached data."
          )
          med(list("med_data"             = med_data_cache, 
                   "element_redox_states" = element_redox_states_cache,
                   "cache"                = TRUE))    
      }
    }
  })
  
  ## Call styling and table export modules ----------------------------------------------------------------------
  element_node_color  <- mod_server_choose_color_sd_palette("mod_element_colors")
  mineral_node_color  <- mod_server_choose_color_sd_palette("mod_mineral_colors")
  edge_color          <- mod_server_choose_color_sd_palette("mod_edge_colors")
  
  #mineral_node_color  <- callModule(mod_server_choose_color_sd_palette, id = "mod_mineral_colors")  
  #edge_color          <- callModule(mod_server_choose_color_sd_palette, id = "mod_edge_colors")

  
  

  
  ## Update selected elements_of_interest based on mineral composition selection -----------------------------
  observeEvent(input$focal_from_mineral,{
    
    shinyWidgets::updatePickerInput(session = session,
                                    "elements_of_interest", 
                                    selected = get_focal_from_minerals(input$focal_from_mineral, med()$med_data)
    ) 
    
    
  }, ignoreNULL = FALSE)
  
  ################################# Build network ####################################
  ## Construct the network from user input ------------------------------------------
  chemistry_network <- reactive({
  
    req(med()$med_data)
    req(med()$element_redox_states)
    #req(med()$cache) # THIS IS YOUR REMINDER TO NOT REQ THIS SINCE FALSE IS NOT TRUTHY
    req(input$elements_of_interest)
    
    elements_of_interest <- input$elements_of_interest
    force_all_elements   <- input$force_all_elements
    restrict_to_elements <- input$restrict_to_elements
    age_range            <- as.numeric(sort(input$age_range))  ## REVERSED, so sort here
    max_age_type         <- input$max_age_type
    elements_by_redox    <- input$elements_by_redox
    ignore_na_redox      <- input$ignore_na_redox
    build_only           <- input$build_only

    
    ## We want to trigger a re-build if layout changes as well:
    input$network_layout
    input$network_layout_seed
    
    if (length(elements_of_interest) == nrow(element_info) & !(input$build_only)) 
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = "Warning!", type = "warning",
        text = "Networks with all elements, especially at more recent time frames, may be very slow - please be patient."
      )
    }
    elements_only <- initialize_data(med()$med_data, med()$element_redox_states, elements_of_interest, force_all_elements, restrict_to_elements)
    if (nrow(elements_only) == 0)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "There is no network for selected element(s) as specified."
      )
      shiny::validate( shiny::need(nrow(elements_only) > 0, ""))
    }
  
    initialized <- initialize_data_age(elements_only, age_range, max_age_type)
    elements_only_age <- initialized$elements_only_age    
    if (nrow(elements_only_age) == 0)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "There is no network for selected element(s) at the age range specified."
      )    
      shiny::validate( shiny::need(nrow(elements_only_age) > 0, ""))
    }
  
    network <- construct_network(elements_only_age, elements_by_redox, ignore_na_redox, med()$element_redox_states)
    if (length(network) != 3 | nrow(network$nodes) == 0  | nrow(network$edges) == 0)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "Network could not be constructed. Please adjust input settings."
      ) 
      shiny::validate( shiny::need(length(network) ==3, "") )
    }
    nodes <- network$nodes 
    graph <- network$network
  
    ## Perform community clustering, which also updates nodes ----------------------------
    clustered <- specify_community_detect_network(graph, nodes, input$cluster_algorithm, input$cluster_seed)
  
    n_clusters <- length(unique(clustered$nodes$cluster_ID))
    if (n_clusters < 1)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "Clustering could not be performed."
      )    
      shiny::validate( shiny::need(n_clusters >= 1, ""))
    }
    
    ## Set cluster colors ----------------------------------------------------------------
    cluster_colors <- set_cluster_colors(input$cluster_palette, n_clusters)  
    if (length(cluster_colors) != n_clusters)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "Clustering could not be performed."
      )    
      shiny::validate( shiny::need(length(cluster_colors) == n_clusters, ""))
    }
  
    ## Subset mineral nodes, used in modeling --------------------------------------------
    mineral_nodes <- subset_mineral_nodes(clustered$nodes)

    ## Find element ids for custom highlighting -----------------------------------
    clustered$nodes %>%
        dplyr::filter(group == "element") %>%
        dplyr::select(id) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(id) -> network_element_ids
        
    return (list("nodes" = clustered$nodes, 
                 "edges" = network$edges, 
                 "graph" = graph, 
                 "elements_of_interest" = elements_of_interest,
                 "mineral_nodes" = mineral_nodes,
                 "locality_info" =  initialized$locality_info, 
                 "timeline_data" = prepare_timeline_data(elements_only, age_range, max_age_type),
                 "raw_node_table" = prepare_raw_node_table(network$edges, clustered$nodes),
                 "clustering"     = clustered$clustered_net, 
                 "cluster_colors" = cluster_colors, 
                 "network_element_ids" = network_element_ids))
  
  })
  
  ## UI components for highlighting a specified set of elements------------------------------------------
  custom_element_modules <- reactiveValues()
  most_recent_button_index <- reactiveVal(0)
  custom_element_modules_indices <- reactiveVal(c())  
  this_id <- reactive({ paste0('customcolor_', most_recent_button_index()) })
  button_reset <- reactiveVal(FALSE)
  
  ## Add button for more custom element colors -------------------------------------------
  observeEvent(input$insert_custom, {
    
    if (button_reset()){
      custom_element_modules_indices(c())
      most_recent_button_index(0)
    }
    button_reset(FALSE)
    last_most_recent <- ifelse(most_recent_button_index() < 0, 
                               0, most_recent_button_index())
    most_recent_button_index(last_most_recent + 1)
    insertUI(
      selector = '#custom_color_chooser', # label in UI
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        id = this_id(),
        mod_ui_choose_custom_element_colors(this_id(), chemistry_network()$network_element_ids) 
      )
    )
    
    ## save module output. only add in the index AFTER it's in the module list
    custom_element_modules[[ as.character(most_recent_button_index()) ]] <- mod_server_choose_custom_element_colors(this_id()) 
    previous_indices <- custom_element_modules_indices()
    custom_element_modules_indices( c(previous_indices, most_recent_button_index()) )
  })
  
  observeEvent(input$remove_custom, {
    
    button_reset(FALSE)
    ## Remove most recent UI 
    removeUI(selector = paste0("#customcolor_", most_recent_button_index()))
        
    ## remove the selection from module list and then update the most recent
    previous_indices <- custom_element_modules_indices()
    custom_element_modules_indices( previous_indices[-most_recent_button_index()]) 
    new_most_recent <- most_recent_button_index() - 1
    most_recent_button_index(new_most_recent)
  
    ## Clean the modules list, +1 since already -1 above
    custom_element_modules[[ as.character(most_recent_button_index() + 1) ]] <- NULL
  })


  ## Reactive that stores custom element colors as named list, by marching over custom_element_modules -------
  custom_element_colors <- reactive({

    custom <- c()
    #print(custom_element_modules_indices()) #1?
    #print(custom_element_modules) # good has 1 value only
    #print(most_recent_button_index()) # 1
    #print(button_reset()) ## FALSE
    if (most_recent_button_index() > 0 & !(button_reset())) {
      for (i in custom_element_modules_indices()) {
        ix <- as.character(i)
        this_one <- custom_element_modules[[ ix ]]() 
        if ( !(is.null( names(this_one)))) {
          for (nodename in names(this_one) ) {
            custom[nodename] <-unname( this_one[nodename] )
          }
        }
      }
    }
    custom
  })

  ## Observer to remove all buttons if the network changes -----------------------
  observeEvent(chemistry_network(),{
    req(input$go >= 1)
    
    # Remove all UIs AND module outputs
    for (i in custom_element_modules_indices()) {
      removeUI(selector = paste0("#customcolor_", i))
      custom_element_modules[[as.character(i)]] <- NULL
    }        
    # Reset all reactives
    button_reset(TRUE)
  })  
  


 

  ## Calculate network quantities reactively --------------------------------------------
  network_quantities <- reactive({
    
    num_nodes_edges <- calculate_number_nodes_edges(chemistry_network()$nodes, 
                                                    chemistry_network()$edges,
                                                    input$elements_by_redox)  
    
    modularity      <- calculate_modularity(chemistry_network()$clustering)
    connectivity    <- calculate_connectivity(chemistry_network()$graph)
    
    list("n_mineral_nodes" = num_nodes_edges$n_mineral_nodes,
         "n_element_nodes" = num_nodes_edges$n_element_nodes,
         "n_base_elements" = num_nodes_edges$n_base_elements,
         "n_edges" = num_nodes_edges$n_edges,
         "modularity" = modularity,
         "connectivity" = connectivity
         )
  })
  
  
  ## Render the "Visualize Network" tabPanel -----------------------------------------------------------------------
  observeEvent(input$go,{
    
    ## Isolate: are we just building (TRUE) or are we also displaying (FALSE)---------------------------------------
    build_only <- isolate(input$build_only)
    

    
    ## Text output associated with network display (or non-display) ------------------------------------------------
    output$no_network_display <- renderText({
      "Your network has been built and is available for export below and/or analysis in other tabs."
      })

    output$modularity <- renderText({
      paste0("Network modularity: ", network_quantities()$modularity)    
    })    
    
    output$connectivity <- renderText({
      if (!(network_quantities()$connectivity)){
        paste0("WARNING: This network is disconnected. Interpret network metrics with caution.")
      }
      else {
        paste0("")
      }
    })  
    
    output$n_element_nodes <- renderText({
      if (input$elements_by_redox)
      {
        element_phrase <- paste0("Number of elements: ", network_quantities()$n_base_elements, ". Number of element nodes: ", network_quantities()$n_element_nodes)    
      } else
      {
        element_phrase <- paste0("Number of element nodes: ", network_quantities()$n_element_nodes)    
      }
      
      paste0(element_phrase)    
    })  
    
    output$n_mineral_nodes <- renderText({
      paste0("Number of mineral nodes: ", network_quantities()$n_mineral_nodes)  
    })  

    output$n_edges <- renderText({
      paste0("Number of edges: ", network_quantities()$n_edges)
    })  
 
    
    ## Render the network itself using visNetwork -----------------------------------------------
    output$networkplot <- visNetwork::renderVisNetwork({
      nodes <- chemistry_network()$nodes
      ## network plot construction should be inside isolate. 
      ## visNetworkProxy() function takes over any viz changes from these initial settings
      isolate({
        
        ## Incorporate user-specified styles ----------------------------------------------------
        starting_nodes <- node_styler()$styled_nodes
        starting_edges <- edge_styler()$styled_edges
        
        
        ## MUST be here!
        if (build_only == FALSE) 
        {
          ## Define baseline network for visualization with user-specified styles -----------------
          base_network <- visNetwork::visNetwork(starting_nodes, starting_edges)

          ## Set the network layout ---------------------------------------------------------------
          if (input$network_layout == "physics") {
            ## Seizure-inducing layout
            base_network %<>% 
              visNetwork::visPhysics(solver = input$physics_solver, 
                                     stabilization = TRUE) # default is 1000 iterations we dont have time for that.
          } else {
            ## igraph network layout
            base_network %<>% 
              visNetwork::visIgraphLayout(layout     = input$network_layout, 
                                          type       = "full", 
                                          randomSeed = input$network_layout_seed) 
          }
          ## Plot it up with visNetwork options
          base_network %>%
           visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = input$selected_degree)) %>%
            visNetwork::visInteraction(dragView  = TRUE,
                                       dragNodes         = TRUE,
                                       zoomView          = TRUE,
                                       hover             = TRUE,
                                       selectConnectedEdges = TRUE,
                                       hideEdgesOnDrag   = TRUE,
                                       multiselect       = TRUE,
                                       navigationButtons = FALSE) %>%
            visNetwork::visGroups(groupname = "element",
                                  color = network_style_options()[["element_color"]], 
                                  shape = network_style_options()[["element_shape"]], 
                                  font  = list(size = input$element_label_size)
                                  ) %>%
            visNetwork::visGroups(groupname = "mineral",
                                  color = network_style_options()[["mineral_color"]], 
                                  shape = network_style_options()[["mineral_shape"]],
                                  size  = input$mineral_size,
                                  font  = list(size = ifelse(input$mineral_label_size == 0,
                                                             "NA",
                                                             input$mineral_label_size))
                                  ) %>%
            visNetwork::visEdges(color = network_style_options()[["edge_color"]],
                                 width = input$edge_weight,
                                 ## smooth=FALSE has no visual effect that I can perceive, and improves speed. Cool.
                                 smooth = FALSE) %>% 
            visNetwork::visEvents(select = "function(nodes) {
                                    Shiny.onInputChange('current_node_id', nodes.nodes);
                                    ;}")
                                    #dragEnd = "function(nodes) {
                                    #  Shiny.onInputChange('dragged_id', nodes.nodes);}")
        } ## END if(build_only == FALSE)
      })  ## END isolate()        
    }) ## END renderVisNetwork({})
    

    ## Output the network legend ----------------------------------------------------------------------------
    output$networklegend <- renderPlot({
      cowplot::ggdraw(final_legend())
    })    
  
    
    ## visNetworkProxy observer to perform *all network updates* ---------------------------------------
    observe({
      # DON'T BE TEMPTED TO GET RID OF THIS IF
      if (build_only ==  FALSE)
      {
        ## visGroups, visNodes, visEdges are global options shared among nodes/edges
        ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
        visNetwork::visNetworkProxy("networkplot") %>%
          visNetwork::visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
          visNetwork::visUpdateEdges(edges = edge_styler()$styled_edges) %>%
          visNetwork::visEdges(width = input$edge_weight) %>%
          visNetwork::visInteraction(dragView             = input$drag_view,  #dragNodes = input$drag_nodes, ## This option will reset all node positions to original layout. Not useful.
                                     hover                = input$hover,
                                     selectConnectedEdges = input$hover, ## shows edges vaguely bold in hover, so these are basically the same per user perspective.
                                     zoomView             = input$zoom_view,
                                     multiselect          = TRUE,
                                     hideEdgesOnDrag      = input$hide_edges_on_drag,
                                     navigationButtons    = input$nav_buttons) %>%
          visNetwork::visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree))
      }
    }) ## END observe

  }) ## END observeEvent(input$go,

  
  ## DOWNLOAD LINKS ------------------------------------------------------------------------
  observeEvent(input$store_position,{
    visNetwork::visNetworkProxy("networkplot") %>% visNetwork::visGetPositions() 
  })
  
  styled_nodes_with_positions <- reactive({
    output_layout <- isolate(input$network_layout)
    seed          <- isolate(input$network_layout_seed)
    
    if(output_layout == "physics")
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = "Warning!", type = "warning",
        text = "Dynamic physics layouts cannot be exported to PDF. Preparing export with Fruchterman-Reingold layout."
      )    
      output_layout <- "layout_with_fr"
    }
    calculate_output_node_positions(node_styler()$styled_nodes, 
                                    input$networkplot_positions, 
                                    chemistry_network()$graph,
                                    output_layout,
                                    seed)
  })
  
  
  
  ## Build the final legend image ---------------------------------------------------------------------
  final_legend <- reactive({
    build_legend(edge_styler(), node_styler())
  })  ## END final_legend reactive
    
    
  network_as_igraph <- reactive({
    visnetwork_to_igraph(styled_nodes_with_positions(), 
                         edge_styler()$styled_edges, 
                         input$baseline_output_element_size,
                         input$baseline_output_element_label_size,
                         input$baseline_output_mineral_size)   
  })
  
  
  
  
  ## Download the network as PDF image -----------------------------------------------------
  output$export_network_pdf <- downloadHandler(
    filename = function() { paste0('dragon_network_', Sys.Date(), '.pdf') },
    content  = function(outfile)
    {
      
      grDevices::pdf(outfile, 
                     useDingbats = FALSE, 
                     width = 10, ## ??
                     height = 8) ## ??
      igraph::plot.igraph(network_as_igraph()$igraph_network, 
                          layout     = network_as_igraph()$coords, 
                          asp        = network_as_igraph()$vis_aspect_ratio, 
                          edge.width = input$edge_weight/3) # a single weight provided to E() ends up being same size, since weight is handled RELATIVLELY by igraph. for scaling diffs, make my 3 --> 1. 
      grDevices::dev.off()
    }
  )
  

  
  ## Download the legend as PDF image ------------------------------------------------------------
  output$export_legend_pdf <- downloadHandler(
    filename = function() { paste0('dragon_legend_', Sys.Date(), '.pdf') },
    content  = function(outfile) 
    {
      #save_plot(outfile,  ggdraw( final_legend() ) , base_width = input$output_legend_width, base_height = input$output_legend_height )
      cowplot::save_plot(outfile,  cowplot::ggdraw( final_legend() ) , base_width = 8 ) ## due to cowplot args, it's too easy to mess this up. we choose for users.
      
    })
  
  ## Download the nodes as CSV ------------------------------------------------------------
  output$export_nodes_csv <- downloadHandler(
    filename = function() { paste0('dragon_node_data_', Sys.Date(), '.csv') },
    content  = function(outfile) 
    {
      readr::write_csv(node_styler()$styled_nodes, outfile)
    })
  
  ## Download the edges as CSV ------------------------------------------------------------
  output$export_edges_csv <- downloadHandler(
    filename = function() { paste0('dragon_edge_data_', Sys.Date(), '.csv') },
    content  = function(outfile) 
    {
      readr::write_csv(edge_styler()$styled_edges, outfile)
    })

  ## Download the network in a specified iGraph text format ------------------------------------------------------------
  output$export_network_igraph <- downloadHandler(
    filename = function() { paste0('dragon_network_', Sys.Date(), '.', input$igraph_output_format) },
    content  = function(outfile) 
    {
      igraph::write_graph(network_as_igraph()$igraph_network, outfile, format = input$igraph_output_format)
    })
  
  
  ## Construction of the edge table (Node Selection) ------------------------------------------------------
  
  ## UI component for choosing nodes whole information should be displayed --------------------------
  # NOTE: In server since this relies on knowing which elements are actually present in the network
  output$choose_nodes <- renderUI({
    chemistry_network()$nodes %>%
      dplyr::filter(group == "element") %>%
      dplyr::arrange(id) %>%
      dplyr::pull(id) -> ordered_ids
    raw_cluster_sel <- purrr::map2_chr("All cluster",
                                       sort(unique(chemistry_network()$nodes$cluster_ID)), 
                                       paste)
    cluster_sel     <- purrr::map2_chr(raw_cluster_sel,"elements", paste)
    ordered_ids <- c(cluster_sel, ordered_ids) ## don't need "All elements" since there is a "select all" button
    
    shinyWidgets::pickerInput("selected_nodes_custom", 
                              "Choose element nodes to view relationships with connected minerals:",             
                              choices = unique(ordered_ids),
                              options = list(`actions-box` = TRUE, size = 6), 
                              multiple = TRUE,
                              width = "425px"
    )
  })
  
  
  ## RENDER THE "Selected Node Information" BOX --------------------------------------------------------------------------------
  observeEvent(input$current_node_id, {
    shinyWidgets::updatePickerInput(session = session,
                                    "selected_nodes_custom", 
                                    selected = c(input$selected_nodes_custom, input$current_node_id)
    ) 
  })
  
  node_table <- reactive({
    selected_nodes <- unique(input$selected_nodes_custom)
    columns_to_display <- c(selected_node_table_constant, ## mineral, element, element's redox in that mineral
                            input$columns_selectednode_mineral, 
                            input$columns_selectednode_element, 
                            input$columns_selectednode_network) 
    
    if (is.null(selected_nodes))
    {
      tibble::tibble()
    }  else {
      
      chemistry_network()$raw_node_table %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, input$node_table_digits)) -> rounded_table
      build_final_node_table(rounded_table, selected_nodes, columns_to_display)
        
    } 

  })
  

  # WE WANT TO KEEP COL NAMES WRAPPED. THIS UNWRAPS: https://stackoverflow.com/questions/31293506/prevent-column-name-wrap-in-shiny-datatable
  output$nodeTable <- DT::renderDataTable(rownames= FALSE, escape = FALSE,  ### escape=FALSE for HTML rendering, i.e. the IMA formula
                                node_table(), 
                                extensions = c('ColReorder', 'Responsive'),
                                options = list(
                                  dom = 'frtip',
                                  colReorder = TRUE
                                )
  )
  
  
  output$show_nodeTable <- renderUI({
    box(width=12,status = "primary", 
        title = "Examine individual nodes", collapsible = TRUE,
        shiny::uiOutput("choose_nodes"),
        br(),
        div(style="display:inline-block;vertical-align:top;",
            shiny::actionButton("include_all_selectednodes", label="Include all attributes")
        ),
        div(style="display:inline-block;vertical-align:top;",
            shiny::actionButton("clear_all_selectednodes", label="Clear attribute selection")
        ), 
        br(),br(),
        div(style="display:inline-block;vertical-align:top;",
           shinyWidgets::prettyCheckboxGroup(
             inputId = "columns_selectednode_mineral",
             label = tags$span(style="font-weight:700", "Mineral attributes:"),
             choices = selected_node_table_column_choices_mineral
           )),
        div(style="display:inline-block;vertical-align:top;",
           shinyWidgets::prettyCheckboxGroup(
             inputId = "columns_selectednode_element",
             label = tags$span(style="font-weight:700", "Element attributes:"),
             choices = selected_node_table_column_choices_element
           )),
        div(style="display:inline-block;vertical-align:top;",
           prettyCheckboxGroup(
             inputId = "columns_selectednode_network",
             label = tags$span(style="font-weight:700", "Network attributes:"),
             choices = selected_node_table_column_choices_network
           )),
        shiny::div(style="font-size:85%;", 
          DT::dataTableOutput("nodeTable")
        ), 
        shiny::sliderInput("node_table_digits", "Choose the number of digits to show in table:", value = 3, min = 1, max = 16, width = "275px"),
        shiny::br(),      
        div(style="display:inline-block;vertical-align:top;",
          downloadButton("export_selected_table", label = "Export table"),
          shinyWidgets::radioGroupButtons("export_selected_table_fmt", 
                                          "", 
                                          choices = c("CSV", "Excel"),
                                          size = "sm",
                                          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                          selected = "CSV")
        ) # div   
    ) ## box
  }) ## renderUI
  
  ## Download handler for selected node table -----------------------------------------
  output$export_selected_table <- shiny::downloadHandler(
    filename = function() {
        if (input$export_selected_table_fmt == "Excel") return(paste("dragon_table-", Sys.Date(), ".xlsx", sep=""))
        if (input$export_selected_table_fmt == "CSV")   return(paste("dragon_table-", Sys.Date(), ".csv", sep=""))
    },
    content = function(filename) {
        if (input$export_selected_table_fmt == "Excel") openxlsx::write.xlsx(node_table(), filename)
        if (input$export_selected_table_fmt == "CSV")   readr::write_csv(node_table(), filename)
    }
  )
  
  
  ## Select all node attributes ----------------------------------------
  observeEvent(input$include_all_selectednodes, {
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_mineral",
                             choices = selected_node_table_column_choices_mineral,
                             selected = selected_node_table_column_choices_mineral)
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_element",
                             choices = selected_node_table_column_choices_element,
                             selected = selected_node_table_column_choices_element)
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_network",
                             choices = selected_node_table_column_choices_network,
                             selected = selected_node_table_column_choices_network)
  })
  
  ## De-select all node attributes ----------------------------------------
  observeEvent(input$clear_all_selectednodes, {
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_mineral",
                             choices = selected_node_table_column_choices_mineral,
                             selected = NULL)
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_element",
                             choices = selected_node_table_column_choices_element,
                             selected = NULL)
   updatePrettyCheckboxGroup(session=session,
                             inputId="columns_selectednode_network",
                             choices = selected_node_table_column_choices_network,
                             selected = NULL)
  })
  
  ## Network information panel ---------------------------------------------------------------------------------
  output$element_exploration_table <- DT::renderDataTable(rownames= FALSE, ## no IMA formulas for elements, dont need escape=F
                                                           build_element_exploration_table(chemistry_network()$nodes) %>%
                                                            dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, input$element_table_digits)),
                                                           extensions = c('ColReorder', 'Responsive'),
                                                           options = list(
                                                             dom = 'frtip',
                                                             colReorder = TRUE
                                                           )) 


  ## Download handler for element exploration -----------------------------------------
  output$export_element_table <- shiny::downloadHandler(
    filename = function() {
        if (input$export_element_table_fmt == "Excel") return(paste("dragon_elements-", Sys.Date(), ".xlsx", sep=""))
        if (input$export_element_table_fmt == "CSV")   return(paste("dragon_elements-", Sys.Date(), ".csv", sep=""))
    },
    content = function(filename) {
        if (input$export_element_table_fmt == "Excel") openxlsx::write.xlsx(build_element_exploration_table(chemistry_network()$nodes), filename)
        if (input$export_element_table_fmt == "CSV")   readr::write_csv(build_element_exploration_table(chemistry_network()$nodes), filename)
    }
  )

                                                           
  output$mineral_exploration_table <- DT::renderDataTable(rownames= FALSE, escape = FALSE,  ### escape=FALSE for HTML rendering, i.e. the IMA formula
                                                          build_mineral_exploration_table(chemistry_network()$nodes, chemistry_network()$locality_info) %>%
                                                            dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, input$mineral_table_digits)), 
                                                           extensions = c('ColReorder', 'Responsive'),
                                                           options = list(
                                                             dom = 'frtip',
                                                             colReorder = TRUE
                                                      ))

 ## Download handler for mineral exploration -----------------------------------------
  output$export_mineral_table <- shiny::downloadHandler(
    filename = function() {
        if (input$export_mineral_table_fmt == "Excel") return(paste("dragon_minerals-", Sys.Date(), ".xlsx", sep=""))
        if (input$export_mineral_table_fmt == "CSV")   return(paste("dragon_minerals-", Sys.Date(), ".csv", sep=""))
    },
    content = function(filename) {
        if (input$export_mineral_table_fmt == "Excel") openxlsx::write.xlsx(build_mineral_exploration_table(chemistry_network()$nodes, chemistry_network()$locality_info), filename)
        if (input$export_mineral_table_fmt == "CSV")   readr::write_csv(build_mineral_exploration_table(chemistry_network()$nodes, chemistry_network()$locality_info), filename)
    }
  ) 
  #################################################################################################################
  #################################################################################################################
  
  output$model_plot_options <- renderUI({
    if (input$predictor == cluster_ID_str) predictor <- "categorical"
    if (input$predictor != cluster_ID_str) predictor <- "scatter"
    switch(predictor,
      "categorical" = list(shinyWidgets::pickerInput("plot_type", "Plot type:", categorical_plot_choices, selected = "strip"),
                           shinyWidgets::prettySwitch("show_legend", "Show legend", value = FALSE, status="danger"),
                           shinyWidgets::prettySwitch("show_mean_se", "Show mean and standard error", value = FALSE, status="danger"),
                           shinyWidgets::prettySwitch("flip_coord", "Flip coordinates", value = FALSE, status="danger"),
                           shinyWidgets::prettySwitch("grid_cluster", "Show background grid", value = FALSE, status="danger"),
                           shiny::numericInput("point_size_cluster", "Point size for sina or strip chart", 2, min = 0.5, max = 4)
                          ),   
      "scatter"   =  list(shinyWidgets::prettySwitch("logx", "Use log scale on X-axis", value = FALSE, status="danger"),
                          shinyWidgets::prettySwitch("logy", "Use log scale on Y-axis", value = FALSE, status="danger",),
                          shinyWidgets::prettySwitch("bestfit", "Show regression line (with 95% confidence interval).", value = FALSE, status="danger"),
                          shinyWidgets::prettySwitch("grid_scatter", "Show background grid", value = FALSE, status="danger"),
                          shiny::numericInput("point_size_scatter",  "Point size", 2, min = 0.5, max = 4),
                          colourpicker::colourInput("point_color", "Point color", value = "black"),
                          colourpicker::colourInput("bestfit_color", "Regression line color", value = "blue")
                        )
    )
  })
 
 
  
  
  
  
  
  ## Render the "Analyze Network Minerals" tabPanel --------------------------------------------------------------
  linear_model_output <- reactive({
    
    ## Perform sanity checking on linear modeling options --------------------------------------------------------
    
    ## Ensure different predictor/response variables -------------------------------------------
    if (input$predictor == input$response)
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "You have selected the same predictor and response variable. Please select new variable(s)."
      )    
      shiny::validate( shiny::need(input$predictor != input$response, ""))
    }
    
    ## Ensure there are sufficient numbers of minerals to analyze (>= 3) -------------------------------
    if (nrow(chemistry_network()$mineral_nodes) < 3) {
      shinyWidgets::sendSweetAlert(
        session = session, title = sample(error_choices)[[1]], type = "error",
        text = "There are fewer than three minerals in your network. To perform statistics, you need at least three data points. Please construct a differet network."
      )    
      shiny::validate( shiny::need(nrow(chemistry_network()$mineral_nodes) >= 3, ""))
      fitted_linear_model <- NULL
      plotted_linear_model <- NULL
    }
    ## Checks to perform if modeling clustering ---------------------------------------------------------
    if (input$predictor == cluster_ID_str)
    {
      chemistry_network()$mineral_nodes %>%
        dplyr::count(cluster_ID, name = "num") %>% 
        dplyr::filter(num >= 3) %>% ## Only keep clusters with >=3 members 
        dplyr::select(cluster_ID) %>%
        dplyr::distinct() %>%
        nrow() -> n_clusters ## Need at least two to compare, see the if below.
      
      if ( n_clusters < 2  )
      {    
        shinyWidgets::sendSweetAlert(
          session = session, title = sample(error_choices)[[1]], type = "error",
          text = "There is insufficient data to analyze community clusters. Please select a different predictor variable."
        )
        shiny::validate( shiny::need(n_clusters >= 2, ""))
      } 
    } # END  if (input$predictor == cluster_ID_str)
    ####-------------------------------------SANITY CHECKING COMPLETED-----------------------------------------------####
    
    
    
    ## Perform modeling and create associated plot ----------------------------------------------------------------------------------
    fitted_linear_model  <- fit_linear_model(input$response, input$predictor, chemistry_network()$mineral_nodes)
    if (fitted_linear_model$tukey_ok_variance == FALSE) 
    {
      shinyWidgets::sendSweetAlert(
        session = session, title = "Caution!", type = "warning",
        text = "Community clusters have unequal variances and modeling results may not be precise."
      )
    }
    fitted_linear_model ## a list itself: $mineral_nodes, $model_fit, $rsquared, $tukey_fit, $tukey_ok_variance (only used above)
  })  ## END linear_model_output reactive
  
  
  linear_model_plot <- reactive({ 
    if (input$predictor == cluster_ID_str)
    {
      p <- plot_linear_model_cluster(input$response, linear_model_output()$keep_clusters, chemistry_network()$mineral_nodes, chemistry_network()$cluster_colors, input$plot_type, input$flip_coord, input$show_mean_se, input$show_legend, input$point_size_cluster, input$grid_cluster)
    } else  {
      p <- plot_linear_model_scatter(input$response, input$predictor, linear_model_output()$rsquared, chemistry_network()$mineral_nodes, input$logx, input$logy, input$point_color, input$point_size_scatter, input$bestfit, input$bestfit_color, input$grid_scatter)
    }    
    p
  })  ## END linear_model_plot reactive
    
    
  ## Renderings for linear model tab ------------------------------------------------------------------------------
    
  ## Render table with fitted model parameters and statistics, for any constructed model -----------------------
  output$fitted_model <- DT::renderDataTable(server = FALSE, rownames= FALSE, extensions = 'Buttons', options = list(dom = 'Bp', buttons = c('copy', 'csv', 'excel')), { 
    linear_model_output()$model_fit
  })
        
  ## Render table specifically for Tukey tests -----------------------------------------------------------------
  output$fitted_tukey <- DT::renderDataTable(server = FALSE, rownames= FALSE, extensions = 'Buttons', options = list(dom = 'Bp', buttons = c('copy', 'csv', 'excel')), { 
    linear_model_output()$tukey_fit
  })

      

  ## Render plot of fitted model -------------------------------------------------------
  output$fitted_model_plot <- renderPlot({
    linear_model_plot()
  })      
  
  ## FYI text for cluster analysis -----------------------------------------------------
  output$cluster_fyi <- renderText({
    "Note: Clusters with fewer than three minerals are excluded from analysis.\n\n"
  })

      
  ## Render the download button for model plot ------------------------------------------
  output$download_model_plot <- shiny::downloadHandler(
    filename = function() {
      paste("dragon_model_plot-", Sys.Date(), ".pdf", sep="")
    },
    content  = function(file) {
      ggplot2::ggsave(file, linear_model_plot())
  })        

  
  ## Renderings for the timeline tabPanel ------------------------------------------------------------
  ## Build the timeline plot
  timeline_plot <- reactive({
    if (input$timeline_view)    data <- chemistry_network()$timeline_data$maxage
    if (!(input$timeline_view)) data <- chemistry_network()$timeline_data$all
    
    if (input$color_timeline_by == "singlecolor"){
      mineral_color_palette <- input$timeline_color
    } else {
      mineral_color_palette <- input$timeline_palette
    }

    build_current_timeline(data, 
                           chemistry_network()$nodes,
                           input$color_timeline_by, # "singlecolor" or a variable 
                           mineral_color_palette, ## either color or the palette for inside the range
                           input$outside_range_color)  ## color for outside the range
  })
  
  ## Render the timeline plot
  output$timeline_plot_output <- renderPlot({
    timeline_plot()
  })
  
  ## Handler for timeline pdf download
  output$download_timeline <- shiny::downloadHandler(
    filename = function() {
      paste("dragon_mineral_timeline-", Sys.Date(), ".pdf", sep="")
    },
    content  = function(file) {
      ggplot2::ggsave(file, timeline_plot(), width = 15, height = 7.5)
    })        
  

  #################################################################################################################
  #################################################################################################################

  network_style_options <- shiny::reactive({
    
    ## VALIDATION ---------------------------------------------
    ## Element color
    element_color_variable  <- as.symbol(element_node_color()$color_by)
    if(element_color_variable != "singlecolor"){
      chemistry_network()$nodes %>%
        dplyr::filter(group == "element") %>%
        dplyr::select( element_color_variable ) %>%
        tidyr::drop_na() -> element_validate
        if (nrow(element_validate) <= 0)
        {
          shinyWidgets::sendSweetAlert(
            session = session, title = sample(error_choices)[[1]], type = "error",
            text = "The specified color scheme cannot be applied to elements due to insufficient node information in the MED database. Please select a different element color scheme."
          ) 
          shiny::validate( shiny::need(nrow(element_validate) > 0, ""))
        }
    }
    ## Mineral color
    mineral_color_variable  <- as.symbol(mineral_node_color()$color_by)
    if(mineral_color_variable != "singlecolor"){
      chemistry_network()$nodes %>%
        dplyr::filter(group == "mineral") %>%
        dplyr::select( mineral_color_variable ) %>%
        tidyr::drop_na() -> mineral_validate
      if (nrow(mineral_validate) <= 0)
      {
        shinyWidgets::sendSweetAlert(
          session = session, title = sample(error_choices)[[1]], type = "error",
          text = "The specified color scheme cannot be applied to minerals due to insufficient node information in the MED database. Please select a different mineral color scheme."
        ) 
        shiny::validate( shiny::need(nrow(mineral_validate) > 0, ""))
      }
    }
    ## Edge color
    edge_color_variable  <- as.symbol(edge_color()$color_by)
    if(edge_color_variable != "singlecolor"){
      chemistry_network()$edges %>%
        dplyr::select( edge_color_variable ) %>%
        tidyr::drop_na() -> edges_validate
      if (nrow(edges_validate) <= 0)
      {
        shinyWidgets::sendSweetAlert(
          session = session, title = sample(error_choices)[[1]], type = "error",
          text = "The specified color scheme cannot be applied to edges due to insufficient node information in the MED database. Please select a different edge color scheme."
        ) 
        shiny::validate( shiny::need(nrow(edges_validate) > 0, ""))
      }
    }
    
    ## Define, return the styles ----------------------------------------------    
    ## Colors shapes first
    list("color_by_cluster"    = input$color_by_cluster,
         "cluster_colors"      = chemistry_network()$cluster_colors,
         "mineral_color_by"    = mineral_node_color()$color_by,
         "mineral_palette"     = mineral_node_color()$palette,
         "mineral_color"       = mineral_node_color()$color,
         "mineral_label_color" = input$mineral_label_color,
         "element_color_by"    = element_node_color()$color_by,
         "element_palette"     = element_node_color()$palette,
         "element_color"       = element_node_color()$color,
         "element_label_color" = input$element_label_color,
         "na_color"            = input$na_color,
         "mineral_shape"       = input$mineral_shape,
         "element_shape"       = input$element_shape,
         ## Sizes
         "mineral_size_by"  = input$mineral_size_by,
         "mineral_size_scale" = input$mineral_size_scale,
         "mineral_label_size" = input$mineral_label_size,
         "mineral_size"       = input$mineral_size,
         "element_size_by"  = input$element_size_by,
         "element_size_scale" = input$element_size_scale,
         "element_label_size" = input$element_label_size,
         ## Single element colors, etc.
         "elements_of_interest"     = chemistry_network()$elements_of_interest,
         "elements_by_redox"        = input$elements_by_redox,
         "highlight_element"        = input$highlight_element,
         "highlight_color"          = input$highlight_color,
         "custom_element_colors"    = custom_element_colors(), ## NAMED LIST
         "custom_selection_element" = input$custom_selection_element, 
         "custom_selection_color"   = input$custom_selection_color,
         ## Edges
         "edge_color_by"    = edge_color()$color_by,
         "edge_palette"     = edge_color()$palette,
         "edge_color"       = edge_color()$color
      ) ## END list definition, which is returned
  })    
      
      



      
  
  ## Reactive to style nodes by user input ---------------------------------------------------------------------------
  node_styler <- reactive({
    
    style_nodes(chemistry_network()$nodes, network_style_options())
  })   
  
  
  ## Reactive to style edges by user input ---------------------------------------------------------------------------
  edge_styler <- reactive({
    style_edges(chemistry_network()$edges, network_style_options())
  })
  #################################################################################################################
  #################################################################################################################
  
  
} ## END server definition