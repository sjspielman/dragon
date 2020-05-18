#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {

  ################################# Build network ####################################
  
  
  ## Construct the network from user input ------------------------------------------
  chemistry_network <- reactive({
    
    req(input$elements_of_interest)
    
    elements_of_interest <- input$elements_of_interest
    force_all_elements   <- input$force_all_elements
    age_limit            <- input$age_limit
    max_age_type         <- input$max_age_type
    elements_by_redox    <- input$elements_by_redox
    build_only           <- input$build_only
    
    if (length(elements_of_interest) == length(all_elements)) 
    {
      shinyBS::createAlert(session, "alert", "all_elements_warning", title = "Warning", 
                  content = '<p style="color:black;">Networks with all elements, especially at more recent time frames, may be very slow - please be patient.</p>')
    }
    
    
    
    elements_only <- initialize_data(elements_of_interest, force_all_elements)
    if (nrow(elements_only) <= 0)
    {
      shinyBS::createAlert(session, "alert", "bad_elements", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                  content = '<p style="color:black;">There is no network for selected element(s) as specified. Please specify new element(s).</p>')
      shiny::validate( shiny::need(nrow(elements_only) > 0, ""))
    }

    initialized <- initialize_data_age(elements_only, age_limit, max_age_type)
    elements_only_age <- initialized$elements_only_age
    locality_info     <- initialized$locality_info
    
    if (nrow(elements_only_age) <= 0)
    {
      shinyBS::createAlert(session, "alert", "bad_age", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                  content = '<p style="color:black;">There is no network for selected element(s) at the age specified. Please specify new input(s).</p>')
      shiny::validate( shiny::need(nrow(elements_only_age) > 0, ""))
    }

    
    network_information <- obtain_network_information(elements_only_age, elements_by_redox)
    if (nrow(network_information) <= 0)
    {
      shinyBS::createAlert(session, "alert", "bad_network", title = '<h4 style="color:black;">Error</h4>', style = "warning", 
                  content = '<p style="color:black;">Network could not be constructed. Please adjust input settings.</p>')
      shiny::validate( shiny::need(nrow(network_information) > 0, ""))
    }
      
    network <- construct_network(network_information, elements_by_redox)
    nodes <- network$nodes 
    edges <- network$edges
    graph <- network$graph

    ## Add title, font to labels if we are viewing the network
    if(build_only == FALSE)
    {
      add_shiny_node_titles(nodes, elements_by_redox) %>%
        # Add type column for grouped node selection dropdown menu
        dplyr::mutate(type = dplyr::if_else(group == "element", 
                                                     "All elements", 
                                                     "All minerals")
                     ) -> nodes 
    }

    ## For the timeline, we need all the minerals
    initialize_data_age(elements_only, c(0, calc_total_max_age()), "Maximum") -> bloop # YES BLOOP WE GOTTA HAVE FUN AROUND HERE
    bloop$elements_only_age %>%
      dplyr::select(mineral_name, max_age) %>%
      dplyr::rename(age = max_age, 
                    id  = mineral_name) -> all_minerals
    
    
    return (list("nodes" = nodes, 
                 "edges" = edges, 
                 "graph" = graph, 
                 "elements_of_interest" = elements_of_interest,
                 "age_lb" = age_limit[1],
                 "age_ub" = age_limit[2],
                 "locality_info" = locality_info, 
                 "elements_only_minerals" = elements_only))
    
  })
  
  
  ## Perform clustering and set up consistent colors for cluster ------------------------------------------
  network_cluster <- reactive({
    
    clustered_net <- community_detect_network(chemistry_network()$graph, input$cluster_algorithm)
    cluster_tibble <- tibble::tibble( "id" = clustered_net$names, "cluster_ID" = as.numeric(clustered_net$membership) )
    
    
    ### Set cluster colors forever ### 
    n_clusters <- length(unique(cluster_tibble$cluster_ID))
    tibble::tibble(x=1:n_clusters, y = factor(1:n_clusters)) %>% 
      ggplot2::ggplot() + 
        ggplot2::aes(x = x, y = y, color = y) + 
        ggplot2::geom_point() + 
        ggplot2::scale_color_brewer(palette = input$clusterpalette) -> cluster_colors_gghack
    ggplot2::ggplot_build(cluster_colors_gghack)$data[[1]] %>% 
      tibble::as_tibble() %>% 
      dplyr::pull(colour) -> cluster_colors
    return( list("clustering" = clustered_net, "tib" = cluster_tibble, "cluster_colors" = cluster_colors) )
  })
  
  
  ## UI component for highlighting a specified set of elements------------------------------------------
  # NOTE: In server since this relies on knowing which elements are actually present in the network
  output$choose_custom_elements_color <- renderUI({
    chemistry_network()$nodes %>%
      dplyr::filter(group == "element") %>%
      dplyr::select(id) %>% 
      tidyr::separate(id, into=c("base_element", "blah")) %>%
      dplyr::arrange(base_element) %>%
      pull(base_element)-> available_base_elements
    
    pickerInput("custom_selection_element", "Highlight a set of elements",             
                choices = unique(available_base_elements),
                options = list(`actions-box` = TRUE, size = 4), 
                multiple = TRUE
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
      membership <- network_cluster()$clustering
      paste0("Network modularity: ", round( membership$modularity[[1]], 4))    
    })    
    
    output$connectivity <- renderText({
      conn <- igraph::vertex_connectivity(chemistry_network()$graph)
      if (conn == 0){
        paste0("WARNING: This network is disconnected. Interpret network metrics with caution.")
      }
      else {
        paste0("")
      }
    })  
    
    output$n_element_nodes <- renderText({
      chemistry_network()$nodes %>% 
        dplyr::filter(group == "element") %>%
        dplyr::distinct() %>%
        nrow() -> n_element_nodes
      
      if (input$elements_by_redox)
      {
        chemistry_network()$nodes %>% 
          dplyr::filter(group == "element") %>%
          tidyr::separate(id, c("base_element", "jazz"), sep = "[\\s\\+\\-]") %>%
          dplyr::select(base_element) %>%
          dplyr::distinct() %>%
          nrow() -> n_base_elements
        element_phrase <- paste0("Number of elements: ", n_base_elements, ". Number of element nodes: ", n_element_nodes)    
      } else
      {
        element_phrase <- paste0("Number of element nodes: ", n_element_nodes)    
      }
      
      paste0(element_phrase)    
    })  
    
    output$n_mineral_nodes <- renderText({
      chemistry_network()$nodes %>% 
        dplyr::filter(group == "mineral") %>%
        dplyr::distinct() %>%
        nrow() -> n_mineral_nodes
      paste0("Number of mineral nodes: ", n_mineral_nodes)  
    })  

    output$n_edges <- renderText({
      n_edges <- nrow(unique(chemistry_network()$edges))
      paste0("Number of edges: ", n_edges)
    })  
 
    
    ## Render the network itself using visNetwork -----------------------------------------------
    output$networkplot <- renderVisNetwork({
      
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
          base_network <- visNetwork(starting_nodes, starting_edges)
          
          
          ## Set the network layout ---------------------------------------------------------------
          if (input$network_layout == "physics") {
            ## Seizure-inducing layout
            base_network %<>% 
              visPhysics(solver        = input$physics_solver, 
                         stabilization = TRUE) # default is 1000 iterations we dont have time for that.
          } else {
            ## igraph network layout
            base_network %<>% 
              visIgraphLayout(layout     = input$network_layout, 
                              type       = "full", 
                              randomSeed = input$network_layout_seed) 
          }
          ## Plot it up with visNetwork options
          base_network %>%
            visOptions(highlightNearest = list(enabled = TRUE, degree = input$selected_degree), 
                       nodesIdSelection = list(enabled = TRUE, 
                                               values  = c( sort(nodes$id[nodes$group == "element"]), 
                                                           sort(nodes$id[nodes$group == "mineral"]) ),
                                               style   = css_string_selectedNode, 
                                               main    = "Select an individual node"),
                       selectedBy = list(variable = "type", 
                                         style    = css_string_selectedNode
                                                   
                       ) 
            )  %>% ## END visOptions              
            visInteraction(dragView  = TRUE, 
                           dragNodes         = TRUE, 
                           zoomView          = TRUE, 
                           hover             = TRUE,
                           selectConnectedEdges = TRUE,
                           hideEdgesOnDrag   = TRUE,
                           multiselect       = TRUE,
                           navigationButtons = FALSE) %>%   
            visGroups(groupname = "element", 
                      color = input$element_color, 
                      shape = input$element_shape,
                      font  = list(size = input$element_label_size)
                      ) %>%
            visGroups(groupname = "mineral", 
                      color = input$mineral_color, 
                      shape = input$mineral_shape,
                      size  = input$mineral_size,
                      font  = list(size = ifelse(input$mineral_label_size == 0, 
                                                 "NA", 
                                                 input$mineral_label_size))
                      ) %>%
            visEdges(color = input$edge_color,
                     width = input$edge_weight,
                     smooth = FALSE)  ## smooth=FALSE has no visual effect that I can perceive, and improves speed. Cool. 
        } ## END if(build_only == FALSE)
      })  ## END isolate()        
    }) ## END renderVisNetwork({})
    

    ## Output the network legend ----------------------------------------------------------------------------
    output$networklegend <- renderPlot({
      cowplot::ggdraw(finallegend())
    })    
    
    
    ## Observer for storing positions when. ---------------------------------------------------------------
    ## NOTE: *must* remain within observeEvent(input$go, 
    ## NOTE: input$store_position is the "Click to prepare network for export to PDF." button
    observeEvent(input$store_position, {
      # TODO does this need a build_only == F?
      #if (build_only == FALSE) visNetworkProxy("networkplot") %>% visGetPositions()
      visNetworkProxy("networkplot") %>% visGetPositions()
    })  ## END observeEvent
    
    ## visNetworkProxy observer to perform *all network updates* ---------------------------------------
    observe({
      # TODO does this need a build_only == F?
      #if (build_only ==  FALSE)
      #{
        #print(input$selected_nodes)
        ## visGroups, visNodes, visEdges are global options shared among nodes/edges
        ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
        visNetworkProxy("networkplot") %>%
          visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
          visUpdateEdges(edges = edge_styler()$styled_edges) %>%
          visEdges(width = input$edge_weight) %>%
          visGetSelectedNodes() %>%
          visInteraction(dragView             = input$drag_view,  #dragNodes = input$drag_nodes, ## This option will reset all node positions to original layout. Not useful.
                         hover                = input$hover, 
                         selectConnectedEdges = input$hover, ## shows edges vaguely bold in hover, so these are basically the same per user perspective.
                         zoomView             = input$zoom_view,
                         multiselect          = TRUE,
                         hideEdgesOnDrag      = input$hide_edges_on_drag,
                         navigationButtons    = input$nav_buttons) %>%
          visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree),
                     nodesIdSelection = list(enabled = TRUE, main  = "Select an individual node.")) 
     # }
    }) ## END observe
    

  }) ## END observeEvent(input$go,
  
  
  
  
  
  
  
  
  ## Build the final legend image ---------------------------------------------------------------------
  finallegend <- reactive({
    e <- edge_styler()
    n <- node_styler()
    finallegend <- NULL
    if (is.na(n$both_legend)) 
    {   ## Mineral, element
      if (is.na(e$edge_legend)) 
      { 
        finallegend <- cowplot::plot_grid(n$element_legend, n$mineral_legend, nrow=1)
      } else {
        ### mineral, element, edge
        finallegend <- cowplot::plot_grid(n$element_legend, n$mineral_legend, e$edge_legend, nrow=1, scale=0.75)
      }
    } else
    {
      ### both
      if (is.na(e$edge_legend)) 
      { 
        finallegend <- n$both_legend
      } else {
        ### both, edge
        finallegend <- cowplot::plot_grid(n$both_legend, e$edge_legend, nrow=1, scale=0.75)
      }
    }   
    return(finallegend)
  })  ## END finallegend reactive
  
  ## Prepare the nodes for download based on current positions -------------------------------------------
  styled_nodes_with_positions <- reactive({
    nodes <- node_styler()$styled_nodes
    positions <- input$networkplot_positions
    
    if (is.null(positions)){
      ## DEFAULT LAYOUT. Obtain the original coordinates, *unless physics* 
      output_layout <- isolate(input$network_layout)
      if(output_layout == "physics")
      {
        shinyBS::createAlert(session, "alert", "pdf_layout", title = '<h4 style="color:black;">Error</h4>', style = "warning", 
                             content = '<p style="color:black;">Note: Dynamic physics layout cannot be exported to PDF. Exporting Fruchterman-Reingold layout.</p>')
        output_layout <- "layout_with_fr"
      }
      shiny::validate( shiny::need(output_layout != "physics", ""))
      set.seed(input$network_layout_seed)
      inet <- chemistry_network()$graph
      coord_string <- paste0("igraph::", input$network_layout, "(inet)")
      as.data.frame( eval(parse(text = coord_string)) ) %>%
        dplyr::rename(x = V1, y = V2) %>%
        dplyr::mutate(id = igraph::vertex_attr(inet, "name")) -> coords
    } else {
      ## CUSTOM LAYOUT by dragging network around
      coords <- do.call("rbind", lapply(positions, function(p){ data.frame(x = p$x, y = p$y)}))
      coords$id <- names(positions)
    }
    nodes %>% dplyr::left_join(coords, by = "id")     
  })
  
  ## DOWNLOAD LINKS ------------------------------------------------------------------------
  
  ## Download the network as PDF image -----------------------------------------------------
  output$downloadNetwork_pdf <- downloadHandler(
    filename <- function() { paste0('dragon_network_', Sys.Date(), '.pdf') },
    content <- function(outfile)
    {
      ## NOTE: `visnetwork_to_igraph()` function is in fct_network_export.R
      igraph_version <- visnetwork_to_igraph(styled_nodes_with_positions(), 
                                             edge_styler()$styled_edges, 
                                             input$output_pdf_node_frame)      
      
      grDevices::pdf(outfile, 
                     useDingbats = FALSE, 
                     width = input$output_pdf_width, 
                     height = input$output_pdf_height)
      igraph::plot.igraph(igraph_version$igraph_network, 
                          layout = igraph_version$coords, 
                          asp    = igraph_version$vis_aspect_ratio)
      grDevices::dev.off()
    }
  )

  
  ## Download the legend as PDF image ------------------------------------------------------------
  output$download_legend <- downloadHandler(
    filename <- function() { paste0('dragon_legend_', Sys.Date(), '.pdf') },
    content <- function(outfile) 
    {
      #save_plot(outfile,  ggdraw( finallegend() ) , base_width = input$output_legend_width, base_height = input$output_legend_height )
      cowplot::save_plot(outfile,  cowplot::ggdraw( finallegend() ) , base_width = 8 ) ## due to cowplot args, it's too easy to mess this up. we choose for users.
      
    })
  
  ## Download the nodes as CSV ------------------------------------------------------------
  output$exportNodes <- downloadHandler(
    filename <- function() { paste0('dragon_node_data_', Sys.Date(), '.csv') },
    content <- function(outfile) 
    {
      readr::write_csv(node_styler()$styled_nodes, outfile)
    })
  
  
  ## Download the edges as CSV ------------------------------------------------------------
  output$exportEdges <- downloadHandler(
    filename <- function() { paste0('dragon_edge_data_', Sys.Date(), '.csv') },
    content <- function(outfile) 
    {
      readr::write_csv(edge_styler()$styled_edges, outfile)
    })


  ## Download button for "Timeline View" tabPanel ------------------------------------------------------------------
  output$download_timeline_plot <- downloadHandler(
    filename = function() {
      paste("dragon_timeline_plot-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      p <-  build_timeline_plot(chemistry_network()$elements_only_minerals, chemistry_network()$age_lb, chemistry_network()$age_ub, input$max_age_type, input$timeline_color_selected, input$timeline_color_notselected)
      
      ggplot2::ggsave(file, p, width=18, height=8)
  })   

  
  
  ## Render the "Timeline View" tabPanel ------------------------------------------------------------------
  output$timeline <- renderPlot({
    build_timeline_plot(chemistry_network()$elements_only_minerals, 
                        chemistry_network()$age_lb, 
                        chemistry_network()$age_ub, 
                        input$max_age_type, 
                        input$timeline_color_selected, 
                        input$timeline_color_notselected) -> final_timeline_plot
    print(final_timeline_plot)
  })
  

  ## RENDER THE "Network Information" tabPanel --------------------------------------------------------------------------------
  
  ## Define the table used in "Network Information" tabPanel ------------------------------------------------------------------
  network_table <- reactive({
    
    build_network_table(chemistry_network()$nodes, 
                        network_cluster()$tib)
    
  })
  
  ## Define the DT to display network_table() reactive ---------------------------------------------
  output$networkTable <- DT::renderDataTable(rownames= FALSE,   
                                             network_table(),
                                             extensions = c('ColReorder', 'Responsive'),
                                             options = list(
                                               dom = 'Bfrtip',
                                               colReorder = TRUE
                                             )
  ) 
  
  
  
  ## Define the download button for network_table() reactive ---------------------------------------------
  output$download_networkTable <- downloadHandler(
    filename <- function() { paste0('dragon_network_information_', Sys.Date(), '.csv') },
    content <- function(file) 
    {
      write_csv(network_table(), file)
    })
  
  
  
  
  ## TODO: This shoud have an associated mode. Based on selection or user types it?
  ## RENDER THE "Selected Node Information" BOX --------------------------------------------------------------------------------
  
  node_table <- reactive({
    list(input$networkplot_selectedBy, 
         input$networkplot_selected, 
         input$columns_selectednode_mineral, 
         input$columns_selectednode_element, 
         input$columns_selectednode_netinfo, 
         input$columns_selectednode_locality)
    
    sel    <- input$networkplot_selected
    sel_by <- input$networkplot_selectedBy
    e <- chemistry_network()$edges
    n <- chemistry_network()$nodes
    
    if ( sel == "" & sel_by == "" )
    {
      return(NULL)
    } 
    
    if (sel != "" | sel_by != "")
    {
      if (sel_by != "")
      {
        sel_by <- ifelse(input$networkplot_selectedBy == "All elements", "element", "mineral")
        
        sel_type <- sel_by
        selected_group_title <- paste0("Selected Node (", stringr::str_to_title(sel_type), ")")
        sel <- unique(n$id[n$group == sel_type])
      } else if (sel != "") {
        if (sel %in% e$mineral_name) 
        {
          sel_type      <- "mineral"
          selected_group_title <- "Selected Node (Mineral)"
        } else {
          sel_type <- "element"
          selected_group_title <- "Selected Node (Element)"
        }  
        sel <- c(sel)
      }            
      selected_vars <- c(input$columns_selectednode_mineral, input$columns_selectednode_element, input$columns_selectednode_netinfo, input$columns_selectednode_locality)
      
      if (is.null(selected_vars)) 
      {
        selected_vars <- selected_group_title
      }   
      e %>%
        filter(
          if (sel_type == "mineral") { mineral_name %in% sel } 
          else {  element %in% sel }
        ) %>%
        left_join(chemistry_network()$locality_info) %>%
        dplyr::select(-from, -to) -> node_table
      n %>% 
        filter(id %in% sel) %>% 
        select(id, closeness, network_degree_norm) -> node_net_info
      network_cluster()$tib %>% filter(id %in% sel) -> cluster_tib
      
      if (sel_type == "mineral") { 
        node_net_info %<>% rename(mineral_name = id)
        cluster_tib %<>% rename(mineral_name = id)
      }
      else { 
        node_net_info %<>% rename(element = id)
        cluster_tib %<>% rename(element = id)
      }
      
      node_table %<>% 
        left_join(node_net_info) %>%
        left_join(cluster_tib) %>%
        distinct() %>%
        mutate(network_degree_norm  = round(network_degree_norm, 5),
               closeness  = round(closeness, 5),
               mean_pauling  = round(mean_pauling, 5),
               cov_pauling   = round(cov_pauling, 5),
               element_redox_mineral  = round(element_redox_mineral, 5)) %>%
        rename(!! variable_to_title[["element_redox_mineral"]] := element_redox_mineral,
               !! variable_to_title[["element_redox_network"]] := element_redox_network,            
               !! variable_to_title[["mineral_id"]] := mineral_id,
               !! variable_to_title[["mineral_name"]] := mineral_name,
               !! variable_to_title[["element"]] := element,
               !! variable_to_title[["element_hsab"]] := element_hsab,
               !! variable_to_title[["mindat_id"]] := mindat_id,
               !! variable_to_title[["locality_longname"]] := locality_longname,
               !! variable_to_title[["max_age"]] := max_age,
               !! variable_to_title[["age_type"]] := age_type,
               !! variable_to_title[["max_age_locality"]] := max_age_locality,
               !! variable_to_title[["min_age_locality"]] := min_age_locality,
               !! variable_to_title[["num_localities_mineral"]] := num_localities_mineral,  
               !! variable_to_title[["num_localities_element"]] := num_localities_element,  
               !! variable_to_title[["network_degree_norm"]] := network_degree_norm,  
               !! variable_to_title[["closeness"]] := closeness,  
               !! variable_to_title[["cluster_ID"]] := cluster_ID,  
               !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry,
               !! variable_to_title[["pauling"]] := pauling,
               !! variable_to_title[["mean_pauling"]] := mean_pauling,
               !! variable_to_title[["cov_pauling"]] := cov_pauling,
               !! variable_to_title[["ima_chemistry"]] := ima_chemistry,
               !! variable_to_title[["element_name"]] := element_name,
               !! variable_to_title[["TableGroup"]] := TableGroup,
               !! variable_to_title[["TablePeriod"]] := TablePeriod,
               !! variable_to_title[["MetalType"]] := MetalType)              
      if (sel_type == "element") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["element"]] ]
        node_table %<>% rename( !!selected_group_title := !!variable_to_title[["element"]])
      }
      else if (sel_type == "mineral") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["mineral_name"]] ]
        node_table %<>% rename( !!selected_group_title := !! variable_to_title[["mineral_name"]])
      }
      
      
      node_table %>% 
        dplyr::select(!!selected_group_title, selected_vars) %>%
        distinct() %>%
        dplyr::select(!!selected_group_title, everything())   
    }
    
    
  })
  

  
  
  output$nodeTable <- DT::renderDataTable( rownames= FALSE, escape = FALSE, ### escape=FALSE for HTML rendering, i.e. the IMA formula
                                node_table(), 
                                extensions = c('ColReorder', 'Responsive'),
                                options = list(
                                  dom = 'Bfrtip',
                                  colReorder = TRUE)
  )
  
  output$download_nodeTable <- downloadHandler(
    filename <- function() { paste0('dragon_selected_node_information_', Sys.Date(), '.csv') },
    content <- function(file) 
    {
      write_csv(node_table(), file)
    })
  
  output$show_nodeTable <- renderUI({
    box(width=12,status = "primary", 
        title = "Selected Node Information", collapsible = TRUE,
        tags$b("Choose which variables to include in table."),
        br(),
        #div(style="display:inline-block;vertical-align:top;",
        actionButton("include_all_selectednodes", label="Include all variables"),
        actionButton("clear_all_selectednodes", label="Clear variable selection"),
        #),
        br(),br(),
        
        div(style="display:inline-block;vertical-align:top;",
            prettyCheckboxGroup(
              inputId = "columns_selectednode_mineral",
              label = tags$span(style="font-weight:700", "Mineral variables:"), 
              choices = selected_node_table_column_choices_mineral
            )),
        div(style="display:inline-block;vertical-align:top;",
            prettyCheckboxGroup(
              inputId = "columns_selectednode_element",
              label = tags$span(style="font-weight:700", "Element variables:"), 
              choices = selected_node_table_column_choices_element
            )),
        div(style="display:inline-block;vertical-align:top;",
            prettyCheckboxGroup(
              inputId = "columns_selectednode_netinfo",
              label = tags$span(style="font-weight:700", "Network variables:"), 
              choices = selected_node_table_column_choices_netinfo
            )),
        div(style="display:inline-block;vertical-align:top;",
            prettyCheckboxGroup(
              inputId = "columns_selectednode_locality",
              label = tags$span(style="font-weight:700", "Locality variables:"), 
              choices = selected_node_table_column_choices_locality
            )),
        div(style="font-size:85%;", DT::dataTableOutput("nodeTable")),
        br(),
        div(style = "float:right;", downloadBttn("download_nodeTable", "Download selected node information", size = "xs", style = "bordered", color = "danger"))
    )
  })   
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
                              inputId="columns_selectednode_netinfo", 
                              choices = selected_node_table_column_choices_netinfo, 
                              selected = selected_node_table_column_choices_netinfo)
    updatePrettyCheckboxGroup(session=session, 
                              inputId="columns_selectednode_locality", 
                              choices = selected_node_table_column_choices_locality, 
                              selected = selected_node_table_column_choices_locality)
  })
  
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
                              inputId="columns_selectednode_netinfo", 
                              choices = selected_node_table_column_choices_netinfo, 
                              selected = NULL)
    updatePrettyCheckboxGroup(session=session, 
                              inputId="columns_selectednode_locality", 
                              choices = selected_node_table_column_choices_locality, 
                              selected = NULL)
  })
  
  
  #################################################################################################################
  #################################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Render the "Analyze Network Minerals" tabPanel --------------------------------------------------------------
  
  ## Reactive that contains specifically only the data that will be used for linear modeling ---------------------
  mineral_nodes <- reactive({
    chemistry_network()$nodes %>%
      left_join(network_cluster()$tib) %>%
      filter(group == "mineral") %>%
      dplyr::select(cluster_ID, network_degree_norm, closeness, num_localities, max_age, mean_pauling, cov_pauling) %>% #sd_pauling
      mutate(cluster_ID = factor(cluster_ID)) %>%
      rename(!! variable_to_title[["cluster_ID"]] := cluster_ID,   ###`Community Cluster`
             !! variable_to_title[["network_degree_norm"]]  := network_degree_norm,
             !! variable_to_title[["closeness"]] := closeness,
             !! variable_to_title[["mean_pauling"]] := mean_pauling,
             !! variable_to_title[["cov_pauling"]] := cov_pauling,
             !! variable_to_title[["num_localities"]] := num_localities,
             !! variable_to_title[["max_age"]] := max_age)    
  })           
  
  ## Reactive to check that information is provided before launching into model buildings -------------------------
  ## TODO: These all have defaults. Can this just be an observeevent with input$go?!
  build_that_model <- reactive({
    c(input$go, input$response, input$predictor, input$logx, input$logy, input$bestfit, input$point_color, input$bestfit_color)
  })
  
  
  ## TODO: is this kosher?
  #observeEvent(build_that_model(), {
  observeEvent(input$go, {
    
    ## Perform sanity checking on linear modeling options -------------------------------------
    
    ## Ensure different predictor/reponse variables -------------------------------------------
    if (input$predictor == input$response)
    {
      createAlert(session, "lm_alert", "same_pred_resp", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                  content = '<p style="color:black;">You have selected the same predictor and response variable. Please select new variable(s).</p>')
      shiny::validate( shiny::need(input$predictor != input$response, ""))
    }
    
    
    use_mineral_nodes <- mineral_nodes() ## since we mutate it sometimes. uh are you kidding isn't this shallow'
    

    
    output$cluster_fyi <- renderText({})
    if (input$predictor == cluster_ID_str)
    {
      use_mineral_nodes %<>%
        group_by(!!sym(cluster_ID_str)) %>%
        mutate(n = n()) %>% 
        filter(n >= 3)
      
      use_mineral_nodes %>% 
        select(!!cluster_ID_str) %>%
        distinct() %>%
        nrow() -> n_clusters
      
      if (  nrow(use_mineral_nodes) == 0  | n_clusters < 2  )
      {                       
        createAlert(session, "lm_alert", "not_enough_clusters", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                    content = '<p style="color:black;">There is insufficient data to analyze community clusters. Please select a different predictor variable.</p>')
        shiny::validate( shiny::need(nrow(use_mineral_nodes) > 0  & n_clusters >= 2, ""))
        
      } else {            
        
        output$cluster_fyi <- renderText({
          "Note: Only those community clusters with at least three minerals are considered for this analysis.\n\n"
        })
      }
      
    }
  
    ## Ensure there are sufficient numbers of minerals to analyze (>= 3) -------------------------------
    if (nrow(use_mineral_nodes) < 3) {
      createAlert(session, "lm_alert", "not_enough_minerals", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                  content = '<p style="color:black;">There are fewer than three minerals in your network. To perform statistics, you need at least three data points. Please construct a differet network.</p>')
      shiny::validate( shiny::need(nrow(use_mineral_nodes) >= 3, ""))
      output$fitted_model <- DT::renderDataTable({})
      output$fitted_model_plot <- renderPlot({})
      
    } else {
      ## There are >=3 minerals. Onward! ---------------------------------------------------------------
      
      ## Build the linear model ------------------------------------------------------------------------
      response_string <- paste0("`", input$response, "`")
      predictor_string <- paste0("`", input$predictor, "`")
      fit_string <- paste(response_string, "~", predictor_string)
      fit <- lm(stats::as.formula(fit_string), data = use_mineral_nodes, na.action = na.omit )
      
      ## Special consideration needed for cluster as predictor (categorical variable, perform Tukey test) ----------
      if (input$predictor == cluster_ID_str)
      {
        ## Re-build the model - TukeyHSD dpes not work with spaces in variable names, so have to do some light bs
        use_mineral_nodes %>% rename(community_cluster = !!cluster_ID_str) -> mineral_nodes2
        aov_fit_string <- paste(response_string, "~community_cluster")
        aov_fit <- stats::aov(stats::as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit )
        
        ## Test for variance assumption and provide warning if not met ----------------------------------------------
        test_variance_pvalue <- stats::bartlett.test(stats::as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit)$p.value
        if (test_variance_pvalue <= 0.05) 
        {
          createAlert(session, "lm_alert", "bad_clusters", title = '<h4 style="color:black;">Warning</h4>', style = "warning",
                      content = '<p style="color:black;">Caution: Clusters have unequal variances and modeling results may not be precise.</p>')
        }
        
        
        ## Build the strip plot output for models with cluster as predictor ------------------------------------
        ggplot2::ggplot(mineral_nodes2) + 
          ggplot2::aes(x = community_cluster, 
                       y = !!sym(input$response), 
                       color = community_cluster) + 
          ggplot2::xlab(input$predictor) + 
          ggplot2::ylab(input$response) +
          ggplot2::geom_jitter(size=3, width=0.1) + 
          ggplot2::scale_color_manual(values = network_cluster()$cluster_colors, name = input$predictor) +
          ggplot2::stat_summary(geom="errorbar", width=0, color = "grey30", size=1)+
          ggplot2::stat_summary(geom="point", color = "grey30", size=3.5) + 
          ggplot2::theme(legend.text  = ggplot2::element_text(size=12), 
                         legend.title = ggplot2::element_text(size=13)) -> fitted_model_plot
        
      } else {
        ## Build the scatterplt for models that do NOT HAVE cluster as predictor ------------------------------------
        ggplot2::ggplot(use_mineral_nodes) + 
          ggplot2::aes_string(x = predictor_string, y = response_string) +
          ggplot2::xlab(input$predictor) + 
          ggplot2::ylab(input$response) + 
          ggplot2::geom_point(size=2, color = input$point_color) -> fitted_model_plot
        if (input$logx) fitted_model_plot <- fitted_model_plot + ggplot2::scale_x_log10()
        if (input$logy) fitted_model_plot <- fitted_model_plot + ggplot2::scale_y_log10()
        if (input$bestfit) fitted_model_plot <- fitted_model_plot + ggplot2::geom_smooth(method = "lm", color = input$bestfit_color)
      }
      
      
      ## Render plot of fitted model -------------------------------------------------------
      output$fitted_model_plot <- renderPlot({
        print(fitted_model_plot)
      })      
      
      ## Render table specifically for Tukey tests -----------------------------------------------------------------
      output$fitted_tukey <- DT::renderDataTable( rownames= FALSE, server=FALSE, extensions = 'Buttons', options = list(dom = 'Bt', buttons = c('copy', 'csv', 'excel')), { 
        stats::TukeyHSD(aov_fit) %>% 
          broom::tidy() %>%
          dplyr::select(-term) %>%
          dplyr::mutate(comparison  = stringr::str_replace_all(comparison, "-", " - "),
                        estimate    = round(estimate, 6),
                        conf.low    = round(conf.low, 6),
                        conf.high   = round(conf.high, 6),
                        adj.p.value = round(adj.p.value, 6)) %>%
          ## TODO: perhaps not hardcoded?
          rename("Cluster Comparison" = comparison, 
                 "Estimated effect size difference" = estimate,
                 "95% CI Lower bound" = conf.low,
                 "95% CI Upper bound" = conf.high,
                 "Adjusted P-value" = adj.p.value)
      })
      
      ## Render table with fitted model parameters and statistics, for any constructed model -----------------------
      output$fitted_model <- DT::renderDataTable( rownames= FALSE, server=FALSE, extensions = 'Buttons', options = list(dom = 'Bt', buttons = c('copy', 'csv', 'excel')), { 
        broom::tidy(fit) %>%
         dplyr::mutate(term = stringr::str_replace_all(term, "`", ""),
                       estimate = round(estimate, 6),
                       std.error = round(std.error, 6),
                       statistic = round(statistic, 6),
                       p.value   = round(p.value, 6)) %>%
         dplyr::rename("Coefficient"          = term, 
                       "Coefficient estimate" = estimate,
                       "Standard error"       = std.error,
                       "t-statistic"          = statistic,
                       "P-value"              = p.value) 
      })
      
      ## Render the download button for model plot
      output$download_model_plot <- downloadHandler(
        filename = function() {
          paste("dragon_model_plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          ggplot2::ggsave(file, fitted_model_plot)
        })        

    } ## END the `else` associated with "there are >=3 data points.    
    
  }) ## END observeEvent(input$go associated with building the linear model
  #################################################################################################################
  #################################################################################################################

  ## Reactive to style nodes by user input ---------------------------------------------------------------------------
  node_styler <- reactive({
    
    full_nodes <- left_join(chemistry_network()$nodes, network_cluster()$tib)
    
    node_attr <- list()   
    node_attr[["both_legend"]] <- NA 
    node_attr[["element_legend"]] <- NA     
    node_attr[["mineral_legend"]] <- NA     
   
    style_options <- list("color_by_cluster" = input$color_by_cluster,
                          "cluster_colors"   = network_cluster()$cluster_colors,
                          "color_mineral_by" = input$color_mineral_by,
                          "mineral_palette"   = input$mineral_palette,
                          "mineral_color"    = input$mineral_color,
                          "mineral_label_color"  = input$mineral_label_color,
                          "color_element_by" = input$color_element_by,
                          "element_palette"   = input$element_palette,
                          "element_color"    = input$element_color,
                          "element_label_color"  = input$element_label_color,
                          "mineral_shape"    = input$mineral_shape,
                          "element_shape"    = input$element_shape,
                          ## Sizes
                          "mineral_size_type"  = input$mineral_size_type,
                          "mineral_size_scale" = input$mineral_size_scale,
                          "mineral_label_size" = input$mineral_label_size,
                          "mineral_size"       = input$mineral_size,
                          "element_size_type"  = input$element_size_type,
                          "element_size_scale" = input$element_size_scale,
                          "element_label_size" = input$element_label_size,
                          ## Shapes
                          "mineral_shape"      = input$mineral_shape,
                          "element_shape"      = input$element_shape,
                          ## Single element colors
                          "elements_of_interest" = chemistry_network()$elements_of_interest,
                          "highlight_element"    = input$highlight_element,
                          "highlight_color"      = input$highlight_color,
                          "custom_selection_element" = input$custom_selection_element, 
                          "custom_selection_color" = input$custom_selection_color)

    ## Assigns node colors, shapes, and exports legend --------------------------------------
    node_attr <- style_nodes_colors_legend(session, full_nodes, node_attr, style_options)
    #print(names(node_attr))
    #readr::write_csv(node_attr[["colors"]], "nodes_attr_colors.csv")
    
    ## Assigns node sizes -------------------------------------------------------------------
    ## TODO: this will need a legend component for the saved plot eeeeek. 
    node_attr <- style_nodes_sizes(full_nodes, node_attr, style_options)
    
  
    
    ## Merge size and color specifications ---------------------------------------------
    full_nodes %>% 
      dplyr::left_join( node_attr[["colors"]] ) %>%
      dplyr::left_join( node_attr[["sizes"]] ) -> node_attr[["styled_nodes"]]

    
    ## Shape, highlight, and label styles ------------------------------------------------
    node_attr[["styled_nodes"]] <- style_nodes_shape_highlight_label(node_attr[["styled_nodes"]], style_options)
    

    ## Style nodes for elements_by_redox = TRUE --------------------------------------------------
    if (input$elements_by_redox){
      node_attr[["styled_nodes"]] <- style_nodes_elements_by_redox(node_attr[["styled_nodes"]], 
                                                                   style_options)
    }

    ## Lighten and darken colors appropriately
    node_attr[["styled_nodes"]] %<>% 
      dplyr::mutate(color.border           = colorspace::darken(color.background, 0.3),
                    color.highlight        = colorspace::lighten(color.background, 0.3),
                    color.hover.border     = colorspace::darken(color.background, 0.3),
                    color.hover.background = colorspace::lighten(color.background, 0.3))%>%
      dplyr::arrange(dplyr::desc(group)) ## arranging minerals first is necessary so that element nodes are always on top and not obscured by giant minerally networks; https://github.com/spielmanlab/dragon/issues/5

    return ( node_attr )
  })   
  
  
  ## Reactive to style edges by user input ---------------------------------------------------------------------------
  edge_styler <- reactive({
    edge_options <- list("color_edge_by" = input$color_edge_by,
                         "edge_color"    = input$edge_color, 
                         "edge_palette"  = input$edge_palette)
    style_edges(chemistry_network()$edges, edge_options)

  })
  #################################################################################################################
  #################################################################################################################
  
  
} ## END server definition