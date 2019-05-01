library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(colorspace)
library(RColorBrewer)  ## colorspace conflict? something strange has been happening but not fully reprexable THIS IS A WORD.
library(DT)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(cowplot)
library(igraph)
library(broom)
library(dragon) ## for the extdata

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) ## Make NA in DT show as NA instead of blank cell


source("build_network.R")

variable_to_title <- list("redox" = "Mean Redox State", 
                          "max_age" = "Maximum Age (Ga)", 
                          "num_localities" = "Number of known localities", 
                          "network_degree_norm" = "Network degree", 
                          "pauling" = "Pauling Electronegativity", 
                          "mean_pauling" = "Mean Pauling Electronegativity", 
                          "sd_pauling" = "Std Dev Pauling Electronegativity")
                          
## mediocre matching here.
vis_to_gg_shape <- list("circle"  = 19,
                        "dot"     = 19,
                        "ellipse" = 19,
                        "box"     = 15,
                        "text"    = 19,
                        "square"  = 15,
                        "star"    = 8,
                        "diamond" = 18,
                        "triangle" = 17)

na.gray <- "#DCDCDC"
geom.point.size <- 8
theme_set(theme_cowplot() + theme(legend.position = "bottom",
                                  legend.text = element_text(size=11),
                                  legend.key.size = unit(1, "cm"),
                                  legend.title = element_text(size=13),
                                  legend.box.background = element_rect(color = "white")))                                  


obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, legendtitle)
{
    
    cvar <- as.symbol(color_variable)
    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.

    dat2 %>% 
        ungroup() %>%
        dplyr::select( color_variable ) %>% 
        na.omit() -> dat_check
    
    shiny::validate(
        shiny::need(nrow(dat_check) > 0, 
        "ERROR: The specified color scheme cannot be applied due to insufficient node information in the MED database.")
    )  

    if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = factor(!!cvar), color = factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle, na.value = na.gray) + guides(colour = guide_legend(title.position="left", title.hjust = 0.5, byrow=TRUE)  )
    if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename, direction = -1, na.value = na.gray)+ guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))

    data.colors <- ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    rename(color = colour) ## god help me, hadley. 
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

obtain_colors_legend_single <- function(group, singleshape, singlecolor)
{
    p <- tibble(x = 1, y = 1, type = group) %>% 
        ggplot(aes(x=x,y=y,color=type)) + 
            geom_point(size = geom.point.size, shape = singleshape) + 
            scale_color_manual(name = "", values=c(singlecolor), na.value=na.gray) + 
            theme(legend.text = element_text(size=16))
    data.colors <- ggplot_build(p)$data[[1]]$colour
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

  
obtain_node_sizes <- function(dat, size_variable, lowsize, highsize, size_scale = 1)
{
    
    svar <- as.symbol(size_variable)

    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.
    p <- ggplot(dat2, aes(x = label, y = !!svar, size = !!svar)) + geom_point() + scale_size(range = c(lowsize,highsize))
    
    data.size <-  ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    mutate(size = size * size_scale)

    return (data.size)
}
  
  
  
  
  
  
server <- function(input, output, session) {
    
    
    ################################# Build network ####################################
    chemistry_network <- reactive({
        
        req(input$elements_of_interest)
        
        elements_of_interest <- input$elements_of_interest
        force_all_elements   <- input$force_all_elements
        age_limit            <- input$age_limit
        elements_by_redox    <- input$elements_by_redox
        
        
        elements_only <- initialize_data(elements_of_interest, force_all_elements)
        shiny::validate(
            shiny::need(nrow(elements_only) > 0, 
            "ERROR: There is no network for selected element(s) as specified. Please specify new element(s).")
        )        
        
        elements_only_age <- initialize_data_age(elements_only, age_limit)
        shiny::validate(
            shiny::need(nrow(elements_only_age) > 0, 
            "ERROR: No network for selected element(s) at the age specified. Please specify a different age.")
        )
        
        network_information <- obtain_network_information(elements_only_age, elements_by_redox)
        shiny::validate(
            shiny::need(nrow(network_information) > 0, 
            "ERROR: Network could not be constructed. Please adjust input settings.")
        )
        
        network <- construct_network(network_information, elements_by_redox)

        nodes <- network$nodes
        edges <- network$edges
        return (list("nodes" = nodes, 
                     "edges" = edges, 
                     #"mineral_indices" = which(nodes$group == "mineral"),
                     #"element_indices" = which(nodes$group == "element"),
                     #"mineral_labels"  = nodes$label[nodes$group == "mineral"],
                     "elements_of_interest" = input$elements_of_interest))

    })
    



    output$choose_custom_elements_color <- renderUI({
        chemistry_network()$nodes %>%
            filter(group == "element") %>%
            dplyr::select(id) %>% 
            separate(id, into=c("base_element", "blah")) %>%
            arrange(base_element) -> available_base_elements
        pickerInput("custom_selection_element", "Highlight a set of elements",             
                        choices = unique(available_base_elements$base_element),
                               options = list(`actions-box` = TRUE, 
                                              size = 4
                                              ), 
                                              multiple = TRUE
                    )
    })

    

    ######################### Node colors, shape, size, labels #########################
    node_styler <- reactive({

        node_attr <- list()   
        node_attr[["both_legend"]] <- NA 
        node_attr[["element_legend"]] <- NA     
        node_attr[["mineral_legend"]] <- NA     
        ################ Colors ####################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(chemistry_network()$nodes, "cluster_ID", "d", "NA", "Network cluster identity")
            node_attr[["both_legend"]] <- out$leg
            node_attr[["colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
        } else 
        { 
            if (input$color_mineral_by == "singlecolor")
            {
                out <- obtain_colors_legend_single("Mineral", vis_to_gg_shape[input$mineral_shape], input$mineral_color)
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "mineral") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$mineral_color)
                
            } else
            {  
                out <- obtain_colors_legend(chemistry_network()$nodes %>% filter(group == "mineral"), input$color_mineral_by, "c", input$mineralpalette, variable_to_title[[input$color_mineral_by]])
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 
            
            
            
            
           
            if (input$color_element_by == "singlecolor")
            {
                if (input$element_shape == "text") { this_color <- input$element_label_color} else { this_color <- input$element_color}
                out <- obtain_colors_legend_single("Element", vis_to_gg_shape[input$element_shape], this_color)
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$element_color)
            } 

            if ((input$color_element_by == "redox" & input$elements_by_redox == FALSE) |  input$color_element_by == "pauling" | input$color_element_by == "network_degree_norm")
            {  
            
                chemistry_network()$nodes %>% filter(group == "element") -> legend_data
                out <- obtain_colors_legend(legend_data, input$color_element_by, "c", input$elementpalette, variable_to_title[[input$color_element_by]])
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 


            if (input$color_element_by == "redox" & input$elements_by_redox == TRUE)
            {  
                out <- obtain_colors_legend(chemistry_network()$edges %>% select(element, redox), input$color_element_by, "c", input$elementpalette, "Element redox state")
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% 
                                                    select(element, color) %>% 
                                                    rename(id = element, color.background = color) %>% 
                                                    left_join(chemistry_network()$nodes) %>% 
                                                    filter(group == "element") %>%
                                                    select(label, id, color.background) %>%
                                                    unique()
            }    
            node_attr[["colors"]] <- bind_rows(node_attr[["element_colors"]], node_attr[["mineral_colors"]]) 
        }   
        
        ################ Sizes ####################
        if (input$element_size_type != "singlesize") 
        {
            node_attr[["sizes"]] <- obtain_node_sizes(chemistry_network()$nodes %>% filter(group == "element"), 
                                                            input$element_size_type, 1, 4, size_scale = input$size_scale) %>%
                                                        select(label, id, size) %>%
                                                        mutate(group = "element") 

        } else
        {
            node_attr[["sizes"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id, group) %>%
                                                mutate(size = input$element_label_size)

        }                     
         
        if (input$mineral_size_type != "singlesize") {
             minsizes <- obtain_node_sizes(chemistry_network()$nodes %>% filter(group == "mineral"), 
                                                            input$mineral_size_type, 5, 30) %>%
                                                            select(label, id, size) %>%
                                                            mutate(group = "mineral")
 
        } else 
        {
            minsizes <- chemistry_network()$nodes %>% 
                            filter(group == "mineral") %>% 
                            mutate(size = input$mineral_size) %>%
                            select(label, id, size, group)
        }         
        node_attr[["sizes"]] %<>% 
             bind_rows(minsizes) %>% 
             mutate(font.size = ifelse(group == "element", size, input$mineral_label_size))


        ########## Merge and add in remaining attributes including shape, highlight, label #################
        node_attr[["styled_nodes"]] <- chemistry_network()$nodes %>% 
                                           left_join( node_attr[["colors"]] ) %>%
                                           left_join( node_attr[["sizes"]]   ) %>% 
                                           mutate(color.background = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element), input$highlight_color, color.background), 
                                                  color.background = ifelse(id %in% input$custom_selection_element, input$custom_selection_color, color.background), 
                                                  font.color = ifelse(group == "element", input$element_label_color, input$mineral_label_color),
                                                  font.color = ifelse(group == "element" & input$element_shape == "text" & input$only_use_element_label_color == FALSE, color.background, font.color),
                                                  font.color = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element & input$element_shape == "text"), input$highlight_color, font.color),
                                                  font.color = ifelse((id %in% input$custom_selection_element & input$element_shape == "text"), input$custom_selection_color, font.color),
                                                  shape = ifelse(group == "element", input$element_shape, input$mineral_shape))
               
        ############################## Deal with certain edge cases at the END ###########################                                
        if (input$color_by_cluster & input$element_shape == "text" & !(input$only_use_element_label_color))
        {
            node_attr[["styled_nodes"]]$font.color <- node_attr[["styled_nodes"]]$color.background
        } 
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
             out <- obtain_colors_legend(chemistry_network()$edges, input$color_edge_by, "c", input$edgepalette, "Mean redox state")

             edge_colors <-  left_join(chemistry_network()$edges, out$cols)
             colorlegend_edge <- out$leg
         }
         
         edge_colors %<>% mutate(width = input$edge_weight)
        return (list("leg" = colorlegend_edge, "styled_edges" = edge_colors) )
    })
        

    observeEvent(input$go,{
    

        output$networkplot <- renderVisNetwork({


            nodes <- chemistry_network()$nodes
            edges <- chemistry_network()$edges
            #selected_element <- chemistry_network()$elements_of_interest[1]
        
            isolate({
                starting_nodes <- node_styler()$styled_nodes
                starting_edges <- edge_styler()$styled_edges

                    visNetwork(starting_nodes, starting_edges) %>%
                    visIgraphLayout(layout = "layout_with_fr", type = "full") %>% ## stabilizes
                    visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree), 
                               nodesIdSelection = list(enabled = TRUE, 
                                                       #selected = selected_element,
                                                       values = c( sort(nodes$id[nodes$group == "element"]), sort(nodes$id[nodes$group == "mineral"]) ),
                                                       main    = "Select node",
                                                       style   = "float:left; width: 200px; font-size: 14px; color: #989898; background-color: #F1F1F1; border-radius: 0; border: solid 1px #DCDCDC; height: 32px; margin: -1.25em 0.5em 0em 0em;")  ##t r b l 
                           ) %>%
                    visInteraction(dragView          = TRUE, 
                                   dragNodes         = TRUE, 
                                   zoomView          = TRUE, 
                                   hover             = TRUE,
                                   selectConnectedEdges = TRUE,
                                   hideEdgesOnDrag   = FALSE,
                                   multiselect       = FALSE,
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
                             smooth = FALSE)  ## no visual effect that I can perceive, and improves speed. Cool. 
            })                 
        })
    

        ########################## legend ##########################
        finallegend <- reactive({
            e <- edge_styler()
            n <- node_styler()
            finallegend <- NULL
            if (is.na(n$both_legend)) 
            {   ## Mineral, element
                if (is.na(e$leg)) 
                { 
                    finallegend <- plot_grid(n$element_legend, n$mineral_legend, nrow=1)
                } else {
                    ### mineral, element, edge
                    finallegend <- plot_grid(n$element_legend, n$mineral_legend, e$leg, nrow=1)
                }
            } else
            {
                ### both
                if (is.na(e$leg)) 
                { 
                    finallegend <- n$both_legend
                } else {
                    ### both, edge
                    finallegend <- plot_grid(n$both_legend, e$leg, nrow=1, scale=0.75)
                }
            }   
            return(finallegend)
        })


        output$networklegend <- renderPlot({
            ggdraw(finallegend())
        })          
        
        output$download_legend <- downloadHandler(
        filename = function() {
          paste("dragon_legend-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          ggsave(file, finallegend())
        })      
        ###########################

    })
  









   

    output$downloadNetwork_html <- downloadHandler(
        #req(input$go > 0)
        filename <- function() { paste0('dragon-', Sys.Date(), '.html') },
        content <- function(con) 
        {
            visNetwork(nodes = node_styler()$styled_nodes, edges = edge_styler()$styled_edges, height = "800px") %>%
                visIgraphLayout(layout = "layout_with_fr", type = "full") %>% ## stabilizes
                visExport(type = "png") %>% ## somehow pdf is worse resolution than png.......??
                visSave(con)
        })
        
        
    output$exportNodes <- downloadHandler(
        filename <- function() { paste0('dragon_node_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(node_styler()$styled_nodes, con)
        })
    output$exportEdges <- downloadHandler(
        filename <- function() { paste0('dragon_edge_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(edge_styler()$styled_edges, con)
        })



    output$clusterTable <- renderDT(rownames= FALSE,  server = FALSE,
       
       
       
       
       
       chemistry_network()$nodes %>% 
            select(id, group, cluster_ID, network_degree, mean_pauling, sd_pauling, pauling, max_age) %>%
            mutate(mean_pauling = round(mean_pauling, 5),
                   sd_pauling = round(sd_pauling, 5)) %>%
            rename("Node name" = id,
                   "Node type" = group,
                   "Louvain Cluster"  = cluster_ID,
                   "Normalized network degree" = network_degree,
                   "Maximum Age" = max_age, 
                   "Pauling electronegativity (elements)" = pauling,
                   "Mean Pauling electronegativity (minerals)" = mean_pauling,
                   "Std Dev Pauling electronegativity (minerals)" = sd_pauling) %>%
            arrange(`Louvain Cluster`, `Normalized network degree`),
         extensions = c('Buttons', 'ColReorder', 'Responsive'),
                        options = list(
                        dom = 'Bfrtip',
                        colReorder = TRUE
        )
    )


    observeEvent(input$networkplot_selected, {

        sel <- input$networkplot_selected
        e <- chemistry_network()$edges

        ################ ID table ##################
        if (sel %in% e$mineral_name){
            e %>% 
                 filter(mineral_name == sel) %>%
                 select(mineral_name, mean_pauling, sd_pauling) %>% 
                 left_join(rruff) %>% 
                 select(mineral_name, mineral_id, mindat_id, at_locality, is_remote, rruff_chemistry, max_age, mean_pauling, sd_pauling) %>%
                 unique() %>% 
                 mutate(at_locality = ifelse(at_locality == 0, "No", "Yes"),
                        is_remote   = ifelse(is_remote == 0, "No", "Yes"),
                        mean_pauling = round(mean_pauling, 5),
                        sd_pauling = round(sd_pauling, 5)) %>%
                rename("Mineral" = mineral_name,
                       "Mineral ID" = mineral_id,
                       "Mindat ID"  = mindat_id,
                       "At Locality?" = at_locality,
                       "Is Remote?" = is_remote,
                       "Chemistry"  = rruff_chemistry,
                       "Maximum Age (Ga)" = max_age,
                       "Mean Pauling electronegativity" = mean_pauling,
                       "Std Dev Pauling electronegativity" = sd_pauling) %>%
                arrange(`Maximum Age (Ga)`, Mineral) -> node_table 
            locality_table <- node_table %>% select(-`Maximum Age (Ga)`)
            
        } else{
            e %>% 
                filter(element == sel) %>% 
                 left_join(rruff) %>% 
                 select(element, mineral_name, max_age, num_localities, redox, rruff_chemistry, pauling) %>%
                 unique() %>%
                 rename("Element"                        = element,
                        "Mineral"                        = mineral_name, 
                        "Maximum age (Ga)"               = max_age, 
                        "Number of known localities"     = num_localities, 
                        "Element redox state in mineral" = redox,
                        "Chemistry"                      = rruff_chemistry,
                        "Pauling electronegativity"      = pauling) %>%
                 arrange(`Maximum age (Ga)`, Mineral) -> node_table 
           
            e %>% 
                filter(element == sel) %>% 
                select(mineral_name, mean_pauling, sd_pauling) %>%
                left_join(rruff) %>% 
                select(-chemistry_elements) %>%
                unique() %>%
                mutate(at_locality = ifelse(at_locality == 0, "No", "Yes"),
                       is_remote   = ifelse(is_remote == 0, "No", "Yes"),
                       mean_pauling = round(mean_pauling, 5),
                       sd_pauling = round(sd_pauling, 5)) %>%
                rename("Mineral" = mineral_name,
                       "Mineral ID" = mineral_id,
                       "Mindat ID"  = mindat_id,
                       "At Locality?" = at_locality,
                       "Maximum Age (Ga)" = max_age,
                       "Is Remote?" = is_remote,
                       "Chemistry"  = rruff_chemistry, 
                       "Mean Pauling electronegativity" = mean_pauling,
                       "Std Dev Pauling electronegativity" = sd_pauling) -> locality_table
        }
            
            
        
        output$nodeTable <- renderDT( rownames= FALSE, server=FALSE, 
                                node_table, 
                                extensions = c('Buttons', 'ColReorder', 'Responsive'),
                                options = list(
                                    dom = 'Bfrtip',
                                    colReorder = TRUE
                                ))
        output$localityTable <- renderDT( rownames= FALSE, server=FALSE, 
                                locality_table, 
                                extensions = c('Buttons', 'ColReorder', 'Responsive'),
                                options = list(
                                    dom = 'Bfrtip',
                                    colReorder = TRUE
                                ))
    })
    
    observe({

        ## visGroups, visNodes, visEdges are global options shared among nodes/edges
        ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
        visNetworkProxy("networkplot") %>%
                visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
                visUpdateEdges(edges = edge_styler()$styled_edges) %>%
                visGetSelectedNodes() %>%
                visGetPositions() %>%
                visInteraction(dragView          = input$drag_view,  #dragNodes = input$drag_nodes, ## This option will reset all node positions to original layout. Not useful.
                               hover             = input$hover, 
                               selectConnectedEdges = input$hover, ## shows edges vaguely bold in hover, so these are basically the same per user perspective.
                               zoomView          = input$zoom_view,
                               multiselect       = input$select_multiple_nodes,
                               hideEdgesOnDrag   = input$hide_edges_on_drag,
                               navigationButtons = input$nav_buttons) %>%
                visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree),
                           nodesIdSelection = list(enabled = TRUE, main  = "Select node"))
    
    })     
    
    
    
    
    
    ############### LINEAR MODEL TAB ###########################
    
    observe({
       
       
        output$model_sanity <- renderText({
            if (input$predictor == input$response)
            {
                "ERROR: You cannot build a meaningful model with the same variable selected for response and predictor. Please select new variable(s).\n\n"
            }      
        })

        chemistry_network()$nodes %>%
        filter(group == "mineral") %>%
        dplyr::select(cluster_ID, network_degree_norm, num_localities, max_age, mean_pauling, sd_pauling) %>%
        rename("Louvain Cluster" = cluster_ID,
               "Network degree (normalized)" = network_degree_norm,
               "Mean Pauling electronegativity" = mean_pauling,
               "Standard deviation Pauling electronegativity" = sd_pauling, 
               "Number of known localities" = num_localities,
               "Maximum known age" = max_age)  -> mineral_nodes    
        mineral_nodes$`Louvain Cluster` <- as.factor(mineral_nodes$`Louvain Cluster`)
        
        response_string <- paste0("`", input$response, "`")
        predictor_string <- paste0("`", input$predictor, "`")
        #predictor_strings <- str_replace( str_replace(input$predictors, "$", "`"), "^", "`")
        #fit_string <- paste(response_string, "~", paste0(predictor_strings, collapse = " + "))
        fit_string <- paste(response_string, "~", predictor_string)
        fit <- lm(as.formula(fit_string), data = mineral_nodes, na.action = na.omit )
        
        
        if (input$predictor == "Louvain Cluster")
        {
            ## This part is *extra* dumb. TukeyHSD is not into spaces so we have to muck with louvain name
            ## Only applies when Cluster is the predictor variable. It is NOT ALLOWED as a response because this is a linear model and we need quant response, sheesh.          
            mineral_nodes %>% rename(louvain_cluster = `Louvain Cluster`) -> mineral_nodes2
            aov_fit_string <- paste(response_string, "~louvain_cluster")

            
            aov_fit <- aov(as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit )
            
            output$fitted_tukey <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 't'), { 
                                        
                                     
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




        } 
        else {
            output$fitted_tukey <- renderDT({})
        }
        
        output$fitted_model <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 't'), { 
                                                broom::tidy(fit) %>%
                                                mutate(term = str_replace_all(term, "`", ""),
                                                       estimate = round(estimate, 6),
                                                       std.error = round(std.error, 6),
                                                       statistic = round(statistic, 6),
                                                       p.value   = round(p.value, 6)) %>%
                                                rename("Term" = term, 
                                                        "Estimated effect size (Slope)" = estimate,
                                                        "Standard error of effect size" = std.error,
                                                        "t-value statistic" = statistic,
                                                        "P-value" = p.value) 
                                            })
        
        top_plot <- ggplot(mineral_nodes, aes_string(x = predictor_string, y = response_string)) +
                            xlab(input$predictor) + 
                            ylab(input$response)
                            
        if (input$predictor == "Louvain Cluster")
        {
            fitted_model_plot <- top_plot + 
                                    geom_jitter(aes_string(color = predictor_string), width=0.2, size=1.5) + 
                                    labs(color = input$predictor)  +
                                    stat_summary(geom="errorbar", width=0, color = "grey30", size=1)+
                                    stat_summary(geom="point", color = "grey30", size=2.5) + 
                                    theme(legend.text=element_text(size=12), legend.title=element_text(size=13))
        } else
        {
            fitted_model_plot <- top_plot + geom_point()
            if (input$logx) fitted_model_plot <- fitted_model_plot + scale_x_log10()
            if (input$logy) fitted_model_plot <- fitted_model_plot + scale_y_log10()
            if (input$bestfit) fitted_model_plot <- fitted_model_plot + geom_smooth(method = "lm")
        } 
        

        
        output$fitted_model_plot <- renderPlot({
            print(fitted_model_plot)
        })      
    
   
    
    
    
    
    output$download_model_plot <- downloadHandler(
        filename = function() {
          paste("dragon_model_plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          ggsave(file, fitted_model_plot)
        })         
        

    })
}
