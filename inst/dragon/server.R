library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(colourpicker)
library(colorspace)
library(RColorBrewer)  ## colorspace conflict? something strange has been happening but not fully reprexable THIS IS A WORD.
library(DT)
library(tidyverse)
library(broom)
library(magrittr)
library(cowplot)
library(visNetwork)
library(igraph)
library(dragon) ## for the extdata
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) ## Make NA in DT show as NA instead of blank cell


source("build_network.R") ### Build the actual network
source("defs.R")          ### Variables and stylizing functions

  
  
  
  
  
server <- function(input, output, session) {
    
    
    ################################# Build network ####################################
    
    
    chemistry_network <- reactive({
        
        req(input$elements_of_interest)
        
        elements_of_interest <- input$elements_of_interest
        force_all_elements   <- input$force_all_elements
        age_limit            <- input$age_limit
        max_age_type         <- input$max_age_type
        elements_by_redox    <- input$elements_by_redox


        

        if (length(elements_of_interest) == length(all_elements)) 
        {
            createAlert(session, "alert", "all_elements_warning", title = "Warning", 
                        content = '<p style="color:black;">Networks with all elements, especially at more recent time frames, may be very slow - please be patient.</p>')
        }
        
        
        
        elements_only <- initialize_data(elements_of_interest, force_all_elements)
        if (nrow(elements_only) <= 0)
        {
            createAlert(session, "alert", "bad_elements", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                        content = '<p style="color:black;">There is no network for selected element(s) as specified. Please specify new element(s).</p>')
            shiny::validate( shiny::need(nrow(elements_only) > 0, ""))
        }
            
        
        initialized <- initialize_data_age(elements_only, age_limit, max_age_type)
        elements_only_age <- initialized$elements_only_age
        locality_info     <- initialized$locality_info





        if (nrow(elements_only_age) <= 0)
        {
            createAlert(session, "alert", "bad_age", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                        content = '<p style="color:black;">There is no network for selected element(s) at the age specified. Please specify new input(s).</p>')
            shiny::validate( shiny::need(nrow(elements_only_age) > 0, ""))
        }
        
        
        network_information <- obtain_network_information(elements_only_age, elements_by_redox)
        if (nrow(network_information) <= 0)
        {
            createAlert(session, "alert", "bad_network", title = '<h4 style="color:black;">Error</h4>', style = "warning", 
                        content = '<p style="color:black;">Network could not be constructed. Please adjust input settings.</p>')
            shiny::validate( shiny::need(nrow(network_information) > 0, ""))
        }
        
        network <- construct_network(network_information, elements_by_redox)

        nodes <- network$nodes %>% mutate(fancy_group = ifelse(group == "element", "All elements", "All minerals")) ## for node selection
        edges <- network$edges
        graph <- network$graph
        
        ## For the timeline, we need all the minerals
        initialize_data_age(elements_only, c(0, total_max_age), "Maximum") -> bloop # YES BLOOP WE GOTTA HAVE FUN AROUND HERE
        bloop$elements_only_age %>%
            dplyr::select(mineral_name, max_age) %>%
            rename(age = max_age, 
                   id = mineral_name) -> all_minerals


        return (list("nodes" = nodes, 
                     "edges" = edges, 
                     "graph" = graph, 
                     "elements_of_interest" = elements_of_interest,
                     "age_lb" = age_limit[1],
                     "age_ub" = age_limit[2],
                     "locality_info" = locality_info, 
                     "elements_only_minerals" = elements_only))

    })



    network_cluster <- reactive({
    
        clustered_net <- community_detect_network(chemistry_network()$graph, input$cluster_algorithm)
        cluster_tibble <- tibble( "id" = clustered_net$names, "cluster_ID" = as.numeric(clustered_net$membership) )

        ### Set cluster colors forever ### 
        n_clusters <- length(unique(cluster_tibble$cluster_ID))
        cluster_colors_gghack <- tibble(x=1:n_clusters, y = factor(1:n_clusters)) %>% ggplot(aes(x = x, y = y, color = y)) + geom_point() 
        ggplot_build(cluster_colors_gghack)$data[[1]] %>% 
            as_tibble() %>% 
            pull(colour) -> cluster_colors
        return( list("clustering" = clustered_net, "tib" = cluster_tibble, "cluster_colors" = cluster_colors) )
    })
                     
                     
                  
    output$choose_custom_elements_color <- renderUI({
        chemistry_network()$nodes %>%
            filter(group == "element") %>%
            dplyr::select(id) %>% 
            separate(id, into=c("base_element", "blah")) %>%
            arrange(base_element) -> available_base_elements
        pickerInput("custom_selection_element", "Highlight a set of elements",             
                        choices = unique(available_base_elements$base_element),options = list(`actions-box` = TRUE, size = 4), multiple = TRUE
                    )
    })



      

    ############################## NETWORK ITSELF AND NETWORK-LEVEL METRICS #############################
    observeEvent(input$go,{
    
        build_only <- isolate(input$build_only)
    
        
        output$modularity <- renderText({
            membership <- network_cluster()$clustering
            paste0("Network modularity: ", round( membership$modularity[[1]], 4))    
        })    

        output$connectivity <- renderText({
            conn <- vertex_connectivity(chemistry_network()$graph)
            if (conn == 0){
                paste0("Warning: This network is disconnected. Interpret network metrics with caution.")
            }
            else {
                paste0("")
            }
        })  

        output$n_element_nodes <- renderText({
            chemistry_network()$nodes %>% 
                filter(group == "element") %>%
                distinct() %>%
                nrow() -> n_element_nodes

            if (input$elements_by_redox)
            {
                chemistry_network()$nodes %>% 
                   filter(group == "element") %>%
                   separate(id, c("base_element", "jazz"), sep = "[\\s\\+\\-]") %>%
                   dplyr::select(base_element) %>%
                   distinct() %>%
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
                filter(group == "mineral") %>%
                distinct() %>%
                nrow() -> n_mineral_nodes
            paste0("Number of mineral nodes: ", n_mineral_nodes)  
        })  
    

        output$n_edges <- renderText({
            n_edges <- nrow(unique(chemistry_network()$edges))
            paste0("Number of edges: ", n_edges)
        })  
    
        
    
    
    
        output$timeline <- renderPlot({

            print( build_timeline_plot(chemistry_network()$elements_only_minerals, chemistry_network()$age_lb, chemistry_network()$age_ub, input$max_age_type, input$timeline_color_selected, input$timeline_color_notselected) )

        })
        
        output$download_timeline_plot <- downloadHandler(
            filename = function() {
              paste("dragon_timeline_plot-", Sys.Date(), ".pdf", sep="")
            },
            content = function(file) {
              p <-  build_timeline_plot(chemistry_network()$elements_only_minerals, chemistry_network()$age_lb, chemistry_network()$age_ub, input$max_age_type, input$timeline_color_selected, input$timeline_color_notselected)
              
              ggsave(file, p, width=18, height=8)
        }) 
   
   
   
        output$no_network_display <- renderText({"Your network has been built and is available for export below and/or analysis in other tabs."})
    
        output$networkplot <- renderVisNetwork({
        
            nodes <- chemistry_network()$nodes
            isolate({
                starting_nodes <- node_styler()$styled_nodes
                starting_edges <- edge_styler()$styled_edges
                
                if (!(build_only)){
                
                base_network <- visNetwork(starting_nodes, starting_edges)
                
                if (input$network_layout == "physics") {
                    base_network %<>% visPhysics(solver = input$physics_solver, stabilization = TRUE) # default is 1000 iterations we dont have time for that.
                } else {
                    base_network %<>% visIgraphLayout(layout = input$network_layout, type = "full", randomSeed = input$network_layout_seed) 
                }
                base_network %>%
                    visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree), 
                                nodesIdSelection = list(enabled = TRUE, 
                                                        #selected = selected_element,
                                                        values = c( sort(nodes$id[nodes$group == "element"]), sort(nodes$id[nodes$group == "mineral"]) ),
                                                        style   = "float:right; width: 200px; font-size: 14px; color: #989898; background-color: #F1F1F1; border-radius: 0; border: solid 1px #DCDCDC; height: 32px; margin: -1.4em 0.5em 0em 0em;",  ##t r b l 
                                                        main    = "Select an individual node"),
                                selectedBy = list(variable = "fancy_group", 
                                                  values=c("All elements", "All minerals"),
                                                  style   = "float:right; width: 200px; font-size: 14px; color: #989898; background-color: #F1F1F1; border-radius: 0; border: solid 1px #DCDCDC; height: 32px; margin: -1.4em 0.5em 0em 0em;",  ##t r b l 
                                                  main    = "Select group of nodes")
                                )  %>%              
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
                              font  = list(size = input$element_label_size)) %>%
                     visGroups(groupname = "mineral", 
                              color = input$mineral_color, 
                              shape = input$mineral_shape,
                              size  = input$mineral_size,
                              font  = list(size = ifelse(input$mineral_label_size == 0, "NA", input$mineral_label_size))) %>%
                     visEdges(color = input$edge_color,
                             width = input$edge_weight,
                             smooth = FALSE) ## no visual effect that I can perceive, and improves speed. Cool. 
                }
            })          
        })
        
        observe({
            if (build_only ==  FALSE)
            {
                #print(input$selected_nodes)
                ## visGroups, visNodes, visEdges are global options shared among nodes/edges
                ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
                visNetworkProxy("networkplot") %>%
                    visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
                    visUpdateEdges(edges = edge_styler()$styled_edges) %>%
                    visEdges(width = input$edge_weight) %>%
                    visGetNodes(input = "nodes_coord") %>%  ### retains last position
                    visGetSelectedNodes() %>%
                    visGetPositions() %>%
                    visInteraction(dragView          = input$drag_view,  #dragNodes = input$drag_nodes, ## This option will reset all node positions to original layout. Not useful.
                                   hover             = input$hover, 
                                   selectConnectedEdges = input$hover, ## shows edges vaguely bold in hover, so these are basically the same per user perspective.
                                   zoomView          = input$zoom_view,
                                   multiselect       = TRUE,
                                   hideEdgesOnDrag   = input$hide_edges_on_drag,
                                   navigationButtons = input$nav_buttons) %>%
                    visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree),
                               nodesIdSelection = list(enabled = TRUE, main  = "Select node"))
            }
        })     
    

        ########################################## legend ######################################
        output$networklegend <- renderPlot({
            ggdraw(finallegend())
        })          
        #####################################################################################

    })

    finallegend <- reactive({
        e <- edge_styler()
        n <- node_styler()
        finallegend <- NULL
        if (is.na(n$both_legend)) 
        {   ## Mineral, element
            if (is.na(e$edge_legend)) 
            { 
                finallegend <- plot_grid(n$element_legend, n$mineral_legend, nrow=1)
            } else {
                ### mineral, element, edge
                finallegend <- plot_grid(n$element_legend, n$mineral_legend, e$edge_legend, nrow=1, scale=0.75)
            }
        } else
        {
            ### both
            if (is.na(e$edge_legend)) 
            { 
                finallegend <- n$both_legend
            } else {
                ### both, edge
                finallegend <- plot_grid(n$both_legend, e$edge_legend, nrow=1, scale=0.75)
            }
        }   
        return(finallegend)
    })  
    
    ##################################### DOWNLOAD LINKS #######################################################
    output$downloadNetwork_html <- downloadHandler(
        #req(input$go > 0)
        filename <- function() { paste0('dragon_network-', Sys.Date(), '.html') },
        content <- function(file) 
        {
            outnet <- visNetwork(nodes = node_styler()$styled_nodes, edges = edge_styler()$styled_edges, height = "800px")
            if (input$network_layout == "physics") {
                    outnet %<>% visPhysics(solver = input$physics_solver, stabilization = TRUE) 
            } else {
                    outnet %<>% visIgraphLayout(layout = input$network_layout, type = "full", randomSeed = input$network_layout_seed) 
            }
            outnet %>% 
                visExport(type = "png") %>% 
                visSave(file)
        })
        
        
    output$exportNodes <- downloadHandler(
        filename <- function() { paste0('dragon_node_data_', Sys.Date(), '.csv') },
        content <- function(file) 
        {
            write_csv(node_styler()$styled_nodes, file)
        })
        
        
    output$exportEdges <- downloadHandler(
        filename <- function() { paste0('dragon_edge_data_', Sys.Date(), '.csv') },
        content <- function(file) 
        {
            write_csv(edge_styler()$styled_edges, file)
        })

    output$download_legend <- downloadHandler(
        filename <- function() { paste0('dragon_legend_', Sys.Date(), '.png') },
        content <- function(file) 
        {
            save_plot(file,  ggdraw( finallegend() ) , base_width = 8 )
        })
        

    ###############################################################################################################







    ################################### NODE TABLES ##############################################    
    
    network_table <- reactive({
        chemistry_network()$nodes %>% 
                left_join(network_cluster()$tib) %>%
                dplyr::select(id, group, max_age, num_localities, cluster_ID, network_degree, network_degree_norm, closeness, element_redox_network, pauling, mean_pauling, cov_pauling) %>% #sd_pauling
                arrange(group, id) %>%
                mutate(group = str_to_title(group)) %>%
                rename(!! variable_to_title[["id"]] := id,
                       !! variable_to_title[["group"]] := group,
                       !! variable_to_title[["cluster_ID"]] := cluster_ID,
                       !! variable_to_title[["network_degree"]] := network_degree,
                       !! variable_to_title[["network_degree_norm"]] := network_degree_norm,
                       !! variable_to_title[["closeness"]] := closeness, 
                       !! variable_to_title[["max_age"]] := max_age, 
                       !! variable_to_title[["element_redox_network"]] := element_redox_network,
                       !! variable_to_title[["num_localities"]] := num_localities,
                       !! variable_to_title[["pauling"]] := pauling,
                       !! variable_to_title[["mean_pauling"]] := mean_pauling,
                       !! variable_to_title[["cov_pauling"]] := cov_pauling) %>%
                distinct() 
    })
             
    node_table <- reactive({
        list(input$networkplot_selectedBy, input$networkplot_selected, input$columns_selectednode_mineral, input$columns_selectednode_element, input$columns_selectednode_netinfo, input$columns_selectednode_locality)

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
                selected_group_title <- paste0("Selected Node (", str_to_title(sel_type), ")")
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

    output$networkTable <- renderDT(rownames= FALSE,   
        network_table(),
        extensions = c('ColReorder', 'Responsive'),
         options = list(
             dom = 'Bfrtip',
             colReorder = TRUE)
    )


        
    output$download_networkTable <- downloadHandler(
        filename <- function() { paste0('dragon_network_information_', Sys.Date(), '.csv') },
        content <- function(file) 
        {
            write_csv(network_table(), file)
        })
        
        
    output$nodeTable <- renderDT( rownames= FALSE, escape = FALSE, ### escape=FALSE for HTML rendering, i.e. the IMA formula
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
            title = "Selected Node Information", 
                h5("Choose which variables to include in table."),
                                
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
                div(style="display:inline-block;vertical-align:top;",
                    actionButton("include_all_selectednodes", label="Include all variables"),
                    actionButton("clear_all_selectednodes", label="Clear variable selection")
                ),
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
















    
    
    ################################################ LINEAR MODEL TAB ###############################################
    #################################################################################################################
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
    
    build_that_model <- reactive({
        c(input$go, input$response, input$predictor, input$logx, input$logy, input$bestfit, input$point_color, input$bestfit_color)#input$community_include_lm_go,
    })
 
   
    observeEvent(build_that_model(), {

        if (input$predictor == input$response)
        {
            createAlert(session, "lm_alert", "same_pred_resp", title = '<h4 style="color:black;">Error</h4>', style = "warning",
                        content = '<p style="color:black;">You have selected the same predictor and response variable. Please select new variable(s).</p>')
            shiny::validate( shiny::need(input$predictor != input$response, ""))
        }

            
        use_mineral_nodes <- mineral_nodes() ## since we mutate it sometimes
        bad <- FALSE
        if (nrow(use_mineral_nodes) < 3) {
            bad <- TRUE
        }      
        
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
        
        if (bad) {
            output$fitted_model <- renderDT({})
            output$fitted_model_plot <- renderPlot({})
        } else {
 
            response_string <- paste0("`", input$response, "`")
            predictor_string <- paste0("`", input$predictor, "`")
            fit_string <- paste(response_string, "~", predictor_string)

            fit <- lm(as.formula(fit_string), data = use_mineral_nodes, na.action = na.omit )
            

            if (input$predictor == cluster_ID_str)
            {
                ## This part is *extra* dumb. TukeyHSD is not into spaces so we have to muck with name
                ## Only applies when Cluster is the predictor variable. It is NOT ALLOWED as a response because this is a linear model and we need quant response, sheesh.          
                use_mineral_nodes %>% rename(community_cluster = !!cluster_ID_str) -> mineral_nodes2
                aov_fit_string <- paste(response_string, "~community_cluster")

                test_variance_pvalue <- bartlett.test(as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit)$p.value
                if (test_variance_pvalue <= 0.01) 
                {
                    createAlert(session, "lm_alert", "bad_clusters", title = '<h4 style="color:black;">Warning</h4>', style = "warning",
                        content = '<p style="color:black;">Caution: Clusters have unequal variances and modeling results may not be precise.</p>')
                }
            
                aov_fit <- aov(as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit )

                output$fitted_tukey <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 'Bt', buttons = c('copy', 'csv', 'excel')), { 
                                            TukeyHSD(aov_fit) %>% 
                                                tidy() %>%
                                                dplyr::select(-term) %>%
                                                mutate(comparison = str_replace_all(comparison, "-", " - "),
                                                        estimate = round(estimate, 6),
                                                        conf.low = round(conf.low, 6),
                                                        conf.high = round(conf.high, 6),
                                                        adj.p.value = round(adj.p.value, 6)) %>%
                                                 rename("Cluster Comparison" = comparison, 
                                                         "Estimated effect size difference" = estimate,
                                                         "95% CI Lower bound" = conf.low,
                                                         "95% CI Upper bound" = conf.high,
                                                         "Adjusted P-value" = adj.p.value)
                                         })
                                         

                #if (!(is.null(input$community_include_lm))) {
                    use_cluster_colors <- network_cluster()$cluster_colors[ unique(mineral_nodes2$community_cluster) ]
                #} else
                #{
                #    use_cluster_colors <- network_cluster()$cluster_colors
                #}                           
                

                fitted_model_plot <- ggplot(mineral_nodes2, aes(x = community_cluster, y = !!sym(input$response))) + 
                                        xlab(input$predictor) + 
                                        ylab(input$response) +
                                        geom_jitter(aes(color = community_cluster), size=3, width=0.1) + 
                                        scale_color_manual(values = use_cluster_colors, name = input$predictor) +
                                        stat_summary(geom="errorbar", width=0, color = "grey30", size=1)+
                                        stat_summary(geom="point", color = "grey30", size=3.5) + 
                                        theme(legend.text=element_text(size=12), legend.title=element_text(size=13))
            
            } else {
               # req(input$logx)
               # req(input$logy)
               # req(input$bestfit)
                fitted_model_plot <- ggplot(use_mineral_nodes, aes_string(x = predictor_string, y = response_string)) +
                                        xlab(input$predictor) + 
                                        ylab(input$response) + 
                                        geom_point(size=2, color = input$point_color)
                if (input$logx) fitted_model_plot <- fitted_model_plot + scale_x_log10()
                if (input$logy) fitted_model_plot <- fitted_model_plot + scale_y_log10()
                if (input$bestfit) fitted_model_plot <- fitted_model_plot + geom_smooth(method = "lm", color = input$bestfit_color)
            }

        
            output$fitted_model <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 'Bt', buttons = c('copy', 'csv', 'excel')), { 
                                                    broom::tidy(fit) %>%
                                                    mutate(term = str_replace_all(term, "`", ""),
                                                           estimate = round(estimate, 6),
                                                           std.error = round(std.error, 6),
                                                           statistic = round(statistic, 6),
                                                           p.value   = round(p.value, 6)) %>%
                                                    rename("Term" = term, 
                                                            "Effect size (slope)" = estimate,
                                                            "Standard error" = std.error,
                                                            "t-statistic" = statistic,
                                                            "P-value" = p.value) 
                                                })
       
            output$fitted_model_plot <- renderPlot({
                print(fitted_model_plot)
            })      
    }    
   
    
    
    
    
    output$download_model_plot <- downloadHandler(
        filename = function() {
          paste("dragon_model_plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          ggsave(file, fitted_model_plot)
        })         
        

    })
    #################################################################################################################
    #################################################################################################################
    
    
    
    ######################################### NODE AND EDGE STYLING ##################################################
    ##################################################################################################################
    
    ################################## Node colors, shape, size, labels ##############################################
    node_styler <- reactive({
                        
        full_nodes <- left_join(chemistry_network()$nodes, network_cluster()$tib)
            
        node_attr <- list()   
        node_attr[["both_legend"]] <- NA 
        node_attr[["element_legend"]] <- NA     
        node_attr[["mineral_legend"]] <- NA     
        ################ Colors ####################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(session, full_nodes, "cluster_ID", "d", "NA", variable_to_title[["cluster_ID"]], discrete_colors = network_cluster()$cluster_colors)
            node_attr[["both_legend"]] <- out$leg
            node_attr[["colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
        } else 
        { 
            if (input$color_mineral_by == "singlecolor")
            {
                out <- obtain_colors_legend_single("Mineral", vis_to_gg_shape[input$mineral_shape], input$mineral_color)
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- full_nodes %>% 
                                                filter(group == "mineral") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$mineral_color)
                
            } else
            {  
                out <- obtain_colors_legend(session, full_nodes %>% filter(group == "mineral"), input$color_mineral_by, "c", input$mineralpalette, variable_to_title[[input$color_mineral_by]])
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 
            
            
            
            
           
            if (input$color_element_by == "singlecolor")
            {
                if (input$element_shape == "text") { this_color <- input$element_label_color} else { this_color <- input$element_color}
                out <- obtain_colors_legend_single("Element", vis_to_gg_shape[input$element_shape], this_color)
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- full_nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$element_color)
            } else if (input$color_element_by == "element_redox_network" & input$elements_by_redox == TRUE)
            {  
                out <- obtain_colors_legend(session, chemistry_network()$edges %>% select(element, element_redox_network), input$color_element_by, "c", input$elementpalette, variable_to_title[[input$color_element_by]])
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% 
                                                    select(element, color) %>% 
                                                    rename(id = element, color.background = color) %>% 
                                                    left_join(full_nodes) %>% 
                                                    filter(group == "element") %>%
                                                    select(label, id, color.background) %>%
                                                    unique()
            }  else
            {
                full_nodes %>% filter(group == "element") -> legend_data
                if (input$color_element_by %in% discrete_color_variables)
                {  
                    out <- obtain_colors_legend(session, legend_data, input$color_element_by, "d", "NA", variable_to_title[[input$color_element_by]])
                } else {
                    out <- obtain_colors_legend(session, legend_data, input$color_element_by, "c", input$elementpalette, variable_to_title[[input$color_element_by]])
                }
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            }   
            node_attr[["colors"]] <- bind_rows(node_attr[["element_colors"]], node_attr[["mineral_colors"]]) 
        }   
        
        ################ Sizes ####################
        if (input$element_size_type != "singlesize") 
        {
            node_attr[["sizes"]] <- obtain_node_sizes(full_nodes %>% filter(group == "element"), 
                                                            input$element_size_type, 1, 4, size_scale = input$element_size_scale) %>%
                                                        select(label, id, size) %>%
                                                        mutate(group = "element") 

        } else
        {
            node_attr[["sizes"]] <- full_nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id, group) %>%
                                                mutate(size = input$element_label_size)

        }                     
         
        if (input$mineral_size_type != "singlesize") {
             minsizes <- obtain_node_sizes(full_nodes %>% filter(group == "mineral"), 
                                                            input$mineral_size_type, 5, 30, size_scale = input$mineral_size_scale / 10) %>%
                                                            select(label, id, size) %>%
                                                            mutate(group = "mineral")
 
        } else 
        {
            minsizes <- full_nodes %>% 
                            filter(group == "mineral") %>% 
                            mutate(size = input$mineral_size) %>%
                            select(label, id, size, group)
        }         
        node_attr[["sizes"]] %<>% 
             bind_rows(minsizes) %>% 
             mutate(font.size = ifelse(group == "element", size, input$mineral_label_size))


        ########## Merge and add in remaining attributes including shape, highlight, label #################
        node_attr[["styled_nodes"]] <- full_nodes %>% 
                                           left_join( node_attr[["colors"]] ) %>%
                                           left_join( node_attr[["sizes"]]   ) %>% 
                                           mutate(color.background = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element), input$highlight_color, color.background), 
                                                  color.background = ifelse(id %in% input$custom_selection_element, input$custom_selection_color, color.background), 
                                                  font.color = ifelse(group == "element", input$element_label_color, input$mineral_label_color),
                                                  font.color = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element & input$element_shape == "text"), input$highlight_color, font.color),
                                                  font.color = ifelse((id %in% input$custom_selection_element & input$element_shape == "text"), input$custom_selection_color, font.color),
                                                  shape = ifelse(group == "element", input$element_shape, input$mineral_shape))
               
        ############################## Deal with certain edge cases at the END ###########################                                
        #if (input$color_by_cluster & input$element_shape == "text" & !(input$only_use_element_label_color))
        #{
        #    node_attr[["styled_nodes"]]$font.color <- node_attr[["styled_nodes"]]$color.background
        #} 
        if (input$elements_by_redox)
        {
             node_attr[["styled_nodes"]] %<>%
                filter(group == "element") %>% 
                mutate(id2 = id) %>%
                separate(id2, into=c("base_element", "blah")) %>%
                mutate(color.background = ifelse(base_element %in% chemistry_network()$elements_of_interest & input$highlight_element, input$highlight_color, color.background),
                       font.color       = ifelse(base_element %in% chemistry_network()$elements_of_interest & input$element_shape == "text" & input$highlight_element, input$highlight_color, font.color),
                       color.background = ifelse(base_element %in% input$custom_selection_element, input$custom_selection_color, color.background),
                       font.color       = ifelse(base_element %in% input$custom_selection_element & input$element_shape == "text", input$input$custom_selection_element, font.color)) %>%
                select(-base_element, -blah) %>%
                bind_rows( node_attr[["styled_nodes"]] %>% filter(group == "mineral") ) 
        }
        
        ### also for sure at end, since depends on all colors being set properly above.
        node_attr[["styled_nodes"]] %<>% mutate(color.border = darken(color.background, 0.3),
                                                color.highlight = lighten(color.background, 0.3),
                                                color.hover.border = darken(color.background, 0.3),
                                                color.hover.background = lighten(color.background, 0.3))%>%
                                         arrange(desc(group)) ## arranging minerals first is necessary so that element nodes are always on top and not obscured by giant minerally networks; https://github.com/spielmanlab/dragon/issues/5
        ######################################################
        return ( node_attr )
    })   
    
    
    
    edge_styler <- reactive({

        if (input$color_edge_by == "singlecolor") 
        {
            edge_colors <- chemistry_network()$edges %>% 
                                mutate(color = input$edge_color)             
            colorlegend_edge <- NA
        } else 
        {
             out <- obtain_colors_legend(session, chemistry_network()$edges, input$color_edge_by, "c", input$edgepalette, variable_to_title[[input$color_edge_by]])

             edge_colors <-  left_join(chemistry_network()$edges, out$cols)
             colorlegend_edge <- out$leg
         }
        #edge_colors %<>% mutate(width = input$edge_weight)
        return (list("edge_legend" = colorlegend_edge, "styled_edges" = edge_colors) )
    })
    #################################################################################################################
    #################################################################################################################
    
    
}
